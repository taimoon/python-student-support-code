import ast
from ast import *
from utils import *
from x86_ast import *
import os
from typing import List, Tuple, Set, Dict

Binding = Tuple[Name, expr]
Temporaries = List[Binding]


class Compiler:

    ############################################################################
    # Remove Complex Operands
    ############################################################################

    def rco_exp(self, e: expr, need_atomic: bool) -> Tuple[expr, Temporaries]:
        # YOUR CODE HERE
        match e:
            case BinOp(left,op,right):
                op = op.__class__()
                l,l_bs = self.rco_exp(left,True)
                r,r_bs = self.rco_exp(right,True)
                e = BinOp(l,op,r)
                bs = l_bs + r_bs
                if need_atomic is True:
                    tmp = Name(generate_name('tmp'))
                    bs += [(tmp,e)]
                    e = tmp
                return e,bs
            case UnaryOp(USub(), v):
                e,bs = self.rco_exp(v,True)
                e = UnaryOp(USub(),e)
                if need_atomic is True:
                    tmp = Name(generate_name('tmp'))
                    bs += [(tmp,e)]
                    e = tmp
                return e,bs
            case Call(Name('input_int'), []):
                if need_atomic:
                    name = Name(generate_name('tmp'))
                    return name,[(name,e)]
                else:
                    return e,[]
            case Constant(v):
                return e,[]
            case Name(id):
                return e,[]
            case _:
                raise Exception('rco_exp not implemented',ast.dump(e))

    def rco_stmt(self, s: stmt) -> List[stmt]:
        match s:
            case Expr(Call(Name('print'), [arg])):
                exp,bindings = self.rco_exp(arg,True)
                ss = make_assigns(bindings) + [Expr(Call(Name('print'),[exp]))]
                return ss
            case Expr(e):
                exp,bindings = self.rco_exp(e,False)
                ss = make_assigns(bindings) + [Expr(exp)]
                return ss
            case Assign(name,e):
                exp,bindings = self.rco_exp(e,False)
                ss = make_assigns(bindings) + [Assign(name,exp)]
                return ss
            case _:
                raise Exception('rco_stmt not implemented',s)

    def remove_complex_operands(self, p: Module) -> Module:
        # YOUR CODE HERE
        match p:
            case Module(body):
                from functools import reduce
                from operator import add
                return Module(reduce(add,[self.rco_stmt(s) for s in body]))
            case _:
                raise Exception('remove_complex_operands not implemented')
        

    ############################################################################
    # Select Instructions
    ############################################################################

    def select_arg(self, e: expr) -> arg:
        # Immediate(int) | Reg(reg) | Deref(reg,int)
        match e:
            case Constant(v):
                return Immediate(v)
            case Name(id):
                return Variable(id)
            case _:
                raise Exception('select_arg not implemented',e)
        pass        

    def select_stmt(self, s: stmt) -> List[instr]:
        match s:
            case Expr(Call(Name('print'), [arg])):
                arg = self.select_arg(arg)
                return [
                    Instr('movq',[arg,Reg('rdi')]),
                    Callq(label_name('print_int'),1),
                ]
            case Assign([name],Call(Name('input_int'), [])):
                var = self.select_arg(name)
                return [
                    Callq(label_name('read_int'),0),
                    Instr('movq',[Reg('rax'),var]),
                ]
            case Assign([name],BinOp(left,op,right)):
                arg1 = self.select_arg(left)
                arg2 = self.select_arg(right)
                var = self.select_arg(name)
                op = {
                    Add:'addq',
                    Sub:'subq',
                }[op.__class__]
                if var == arg2:
                    return [Instr(op,[arg1,var])]
                elif var == arg1 and op == 'addq':
                    return [Instr(op,[arg2,arg1])]
                elif arg2 != var :
                    return [
                        Instr('movq',[arg1,var]),
                        Instr(op,[arg2,var]),
                    ]
                else:
                    return [
                        Instr('movq',[arg1,Reg('rax')]),
                        Instr(op,[arg2,Reg('rax')]),
                        Instr('movq',[Reg('rax'),var]),
                    ]
            case Assign([name],UnaryOp(USub(), v)):
                arg = self.select_arg(v)
                var = self.select_arg(name)
                if var == arg:
                    return [Instr('negq',[var])]
                else:
                    return [
                        Instr('movq',[arg,var]),
                        Instr('negq',[var]),
                    ]
            case Assign([name],arg):
                var = self.select_arg(name)
                arg = self.select_arg(arg)
                return [
                    Instr('movq',[arg,var])
                ]
            case Expr(e):
                return self.select_stmt(
                    Assign([Name('_')],e)
                )
            case _:
                raise NotImplementedError(ast.dump(s))

    def select_instructions(self, p: Module) -> X86Program:
        # YOUR CODE HERE
        match p:
            case Module(body):
                from functools import reduce
                from operator import add
                return X86Program(reduce(add,[self.select_stmt(s) for s in body]))
            case _:
                raise Exception    

    ############################################################################
    # Assign Homes
    ############################################################################

    def assign_homes_arg(self, a: arg, home: Dict[Variable, arg]) -> arg:
        # YOUR CODE HERE
        match a:
            case Variable(id):
                if a not in home:
                    offset = -8*(len(home)+1)
                    home[a] = Deref('rbp',offset)
                return home[a]
            case a if isinstance(a,Immediate|Reg):
                return a
            case _:
                raise NotImplementedError('assign_homes_arg',a)

    def assign_homes_instr(self, i: instr,
                           home: Dict[Variable, arg]) -> instr:
        # YOUR CODE HERE
        match i:
            case Instr(op,args):
                return Instr(op,[self.assign_homes_arg(a,home) for a in args])
            case i if isinstance(i,Callq):
                return i
            case _:
                raise NotImplementedError('assign_homes_instr',str(i))

    def assign_homes_instrs(self, ss: List[instr],
                            home: Dict[Variable, arg]) -> List[instr]:
        # YOUR CODE HERE
        return [self.assign_homes_instr(s,home) for s in ss]

    def assign_homes(self, p: X86Program) -> X86Program:
        # YOUR CODE HERE
        return X86Program(self.assign_homes_instrs(p.body,{}))    

    ############################################################################
    # Patch Instructions
    ############################################################################

    def patch_instr(self, i: instr) -> List[instr]:
        # YOUR CODE HERE
        match i:
            case Instr(op,[Deref(reg_1,off_1),Deref(reg_2,off_2)]):
                return [
                    Instr('movq',[Deref(reg_1,off_1),Reg('rax')]),
                    Instr(op,[Reg('rax'),Deref(reg_2,off_2)])
                ]
            case Instr(op,[Immediate(i),arg2])\
                if i > 2**16:
                 return [
                    Instr('movq',[Immediate(i),Reg('rax')]),
                    Instr(op,[Reg('rax'),arg2])
                ]
            case i if isinstance(i,Instr|Callq):
                return [i]
            case _:
                raise NotImplementedError('patch_instr : unknown instruction', i)

    def patch_instrs(self, ss: List[instr]) -> List[instr]:
        from functools import reduce
        from operator import add
        return reduce(add,[self.patch_instr(i) for i in ss])

    def patch_instructions(self, p: X86Program) -> X86Program:
        match p:
            case X86Program(body):
                return X86Program(self.patch_instrs(body))
            case _:
                raise NotImplementedError('patch_instructions, unknown argument', p)

    ############################################################################
    # Prelude & Conclusion
    ############################################################################

    def prelude_and_conclusion(self, p: X86Program) -> X86Program:
        # YOUR CODE HERE
        prelude = [
            Instr('pushq',[Reg('rbp')]),
            Instr('movq',[Reg('rsp'),Reg('rbp')]),
            Instr('subq',[Immediate(16),Reg('rsp')]),
        ]
        conclusion = [
            Instr('addq',[Immediate(16),Reg('rsp')]),
            Instr('popq',[Reg('rbp')]),
            Instr('retq',[]),
        ]
        match p:
            case X86Program(body):
                return X86Program(prelude + body + conclusion)
            case _:
                raise NotImplementedError("prelude_and_conclusion")

