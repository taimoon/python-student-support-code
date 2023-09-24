from ast import *
from utils import *
from x86_ast import *
from typing import List, Tuple, Set, Dict

Binding = Tuple[Name, expr]
Temporaries = List[Binding]


class Compiler:

    ############################################################################
    # Remove Complex Operands
    ############################################################################
    def atomize(self,e,bs):
        tmp = Name(generate_name('tmp'))
        bs += [(tmp,e)]
        e = tmp
        return e,bs

    def rco_exp(self, e: expr, need_atomic: bool) -> Tuple[expr, Temporaries]:
        atomize = self.atomize
        match e:
            case BinOp(left,op,right):
                l,l_bs = self.rco_exp(left, True)
                r,r_bs = self.rco_exp(right, True)
                e = BinOp(l,op,r)
                bs = l_bs + r_bs
                return atomize(e,bs) if need_atomic is True else (e,bs)
            case UnaryOp(op, v):
                e,bs = self.rco_exp(v, True)
                e = UnaryOp(op, e)
                return atomize(e,bs) if need_atomic is True else (e,bs)
            case Call(Name('input_int'), []):
                bs = []
                return atomize(e,bs) if need_atomic is True else (e,bs)
            case Constant()|Name():
                return e,[]
            case _:
                raise Exception('rco_exp, unexpected',e)

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
            case Assign([Name(var)],e):
                exp,bindings = self.rco_exp(e,False)
                ss = make_assigns(bindings) + [Assign([Name(var)],exp)]
                return ss
            case _:
                raise Exception('rco_stmt not implemented',s)
    
    def rco_stmts(self, ss:list[stmt]) -> list[stmt]:
        from functools import reduce
        from operator import add
        return reduce(add,[self.rco_stmt(s) for s in ss])
    
    def remove_complex_operands(self, p: Module) -> Module:
        match p:
            case Module(body):
                return Module(self.rco_stmts(body))
            case _:
                raise Exception('remove_complex_operands not implemented')
        

    ############################################################################
    # Select Instructions
    ############################################################################

    def select_arg(self, e: expr) -> arg:
        match e:
            case Constant(v):
                return Immediate(v)
            case Name(id):
                return Variable(id)
            case _:
                raise Exception('select_arg not implement',e)

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
            case Assign([name],Constant(v)):
                var = self.select_arg(name)
                arg = self.select_arg(Constant(v))
                return [Instr('movq',[arg,var])]
            case Assign([name],Name(id)):
                var = self.select_arg(name)
                arg = self.select_arg(Name(id))
                return [Instr('movq',[arg,var])]
            case Expr(e):
                return self.select_stmt(
                    Assign([Name('_')],e)
                )
            case _:
                raise NotImplementedError(s)

    def select_stmts(self, ss: List[stmt]) -> List[instr]:
        from functools import reduce
        from operator import add
        return reduce(add,[self.select_stmt(s) for s in ss])
    
    def select_instructions(self, p: Module) -> X86Program:
        match p:
            case Module(body):
                return X86Program(self.select_stmts(body))
            case _:
                raise NotImplementedError('select_instructions unexpected', type(p), p)  

    ############################################################################
    # Assign Homes
    ############################################################################

    def assign_homes_arg(self, a: arg, home: Dict[Variable, arg]) -> arg:
        match a:
            case Immediate()|Reg():
                return a
            case Variable():
                if a not in home:
                    home[a] = Deref('rbp',offset = -8*(len(home)+1))
                return home[a]
            case _:
                raise NotImplementedError('assign_homes_arg',a)

    def assign_homes_instr(self, i: instr,
                           home: Dict[Variable, arg]) -> instr:
        match i:
            case Instr(op,args):
                return Instr(op,[self.assign_homes_arg(a,home) for a in args])
            case Callq():
                return i
            case _:
                raise NotImplementedError('assign_homes_instr ',str(i))

    def assign_homes_instrs(self, ss: List[instr],
                            home: Dict[Variable, arg]) -> List[instr]:
        return [self.assign_homes_instr(s,home) for s in ss]

    def assign_homes(self, p: X86Program) -> X86Program:
        home = {}
        res = X86Program(self.assign_homes_instrs(p.body,home))
        self.spilled = set(home.keys())
        return res

    ############################################################################
    # Patch Instructions
    ############################################################################
    def is_mem_ref(self,a:arg):
        return isinstance(a,Deref)
    
    def patch_instr(self, i: instr) -> List[instr]:
        is_ref = self.is_mem_ref
        match i:
            case Instr(op,args=[arg1,arg2])\
                if is_ref(arg1) and is_ref(arg2):
                return [
                    Instr('movq',[arg1,Reg('rax')]),
                    Instr(op,[Reg('rax'),arg2])
                ]
            case Instr(op,[Immediate(i),ref])\
                if  is_ref(ref) and i > 2**16:
                 return [
                    Instr('movq',[Immediate(i),Reg('rax')]),
                    Instr(op,[Reg('rax'),ref])
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
        sz = len(self.spilled)*8
        sz = sz if sz%16 == 0 else sz+8
        prelude = [
            Instr('pushq',[Reg('rbp')]),
            Instr('movq',[Reg('rsp'),Reg('rbp')]),
            Instr('subq',[Immediate(sz),Reg('rsp')]),
        ]
        conclusion = [
            Instr('addq',[Immediate(sz),Reg('rsp')]),
            Instr('popq',[Reg('rbp')]),
            Instr('retq',[]),
        ]
        match p:
            case X86Program(body):
                return X86Program(prelude + body + conclusion)
            case _:
                raise NotImplementedError("prelude_and_conclusion")

