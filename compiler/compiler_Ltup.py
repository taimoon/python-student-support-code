from ast import *
from ast import List, arg, expr, stmt
from typing import List
from x86_ast import *
from compiler.compiler import Temporaries
from utils import (
    CProgram, stmt, TupleType,  
    make_assigns, make_begin, generate_name, 
    Begin, Collect, Allocate,
    GlobalValue,
    )
from compiler.compiler_Lif import Blocks
from compiler.compiler_Lwhile import Compiler as Compiler_Lwhile
from x86_ast import X86Program

'''
Ltup extends Lwhile
note:
only exp[int], constant index is allowed
'''

class Compiler(Compiler_Lwhile):
    # shrink pass
    def shrink(self, p: Module) -> Module:
        return super().shrink(p)
    
    def expand_exp(self, e):
        expand_exp = self.expand_exp
        match e:
            case Tuple([*es],ctx=ctx):
                r = Tuple([expand_exp(e) for e in es],ctx=ctx)
                if hasattr(e,'has_type'):
                    r.has_type = e.has_type
                return r
            case Subscript(tup_exp,Constant(idx),ctx=ctx):
                return Subscript(expand_exp(tup_exp),Constant(idx),ctx=ctx)
            case _:
                return super().expand_exp(e)
    
    '''
    The output of expose_allocation is a language LAlloc that replaces tuple creation with new lower-level forms that we use in the translation of tuple creation.
    
    exp ::= collect(int) | allocate(int, type) | global_value(name)
    stmt ::= exp[int] = exp
    '''
    # expose_allocation pass
    def expose_allocation(self,p: Module) -> Module:
        return Module(self.expose_stmts(p.body))
    
    def expose_stmts(self, ss: list[stmt]) -> list[stmt]:
        return [self.expose_stmt(s) for s in ss]
    
    def expose_stmt(self, s: stmt) -> stmt:
        expose_stmts = self.expose_stmts
        expose_exp = self.expose_exp
        match s:
            case If(pred,conseq,alter):
                return If(expose_exp(pred),expose_stmts(conseq),expose_stmts(alter))
            case Expr(e):
                return Expr(expose_exp(e))
            case Assign([name],e):
                return Assign([name],expose_exp(e))
            case _:
                raise NotImplementedError('expose_stmt, unexpected', s)
    
    def expose_exp(self, e):
        expose_exp = self.expose_exp
        match e:
            case IfExp(pred,conseq,alter):
                return IfExp(pred,expose_exp(conseq),expose_exp(alter))
            case BinOp(e1,op,e2):
                return BinOp(expose_exp(e1),op,expose_exp(e2))
            case UnaryOp(op,_e):
                return UnaryOp(op,expose_exp(_e))
            case Tuple([*es],ctx=Load()):
                if not hasattr(e,'has_type') or not isinstance(e.has_type,TupleType):
                    raise Exception("expose_exp, bad argument, typeless tuple: ", e)
                bytes_v = 8*(len(es)+1)
                init = [(Name(generate_name('init')),expose_exp(e)) for e in es]
                collect = [If(
                            test=Compare(
                                left=BinOp(
                                    left=GlobalValue('free_ptr'),
                                    op=Add(),
                                    right=Constant(bytes_v)),
                                ops=[Lt()],
                                comparators=[GlobalValue('fromspace_end')]),
                            body=[Expr(value=Constant(value=0))],
                            orelse=[Collect(bytes_v)]
                        )]
                tup_exp = Name(generate_name('alloc'))
                alloc = [(Subscript(tup_exp,Constant(i),Store()),name) for i,(name,_) in enumerate(init)]
                alloc = [(tup_exp,Allocate(len(init),e.has_type))] + alloc
                
                alloc = make_assigns(alloc)
                init = make_assigns(init)
                res = Begin(init + collect + alloc, tup_exp)
                return res
            case Call(fn,[*es]):
                return Call(fn,[expose_exp(e) for e in es])
            case Subscript(tup_exp,Constant(idx),ctx):
                return Subscript(expose_exp(tup_exp),Constant(idx),ctx)
            case Constant()|Name():
                return e
            case _:
                raise NotImplementedError('expose_exp, unexpected ', e)
    
    # remove_complex_operands
    # expose(p) : Lalloc -> rco(p) : L_monadic_alloc
    def rco_stmt(self, s: stmt) -> List[stmt]:
        rco_exp = self.rco_exp
        match s:
            case Collect():
                return [s]
            case Assign([Subscript(tup_exp,Constant(i),ctx=Store())],e):
                tup_exp,xs = rco_exp(tup_exp,True)
                e,bs = rco_exp(e,False)
                bs = xs + bs
                ss = make_assigns(bs) + [
                    Assign([Subscript(tup_exp,Constant(i),ctx=Store())],e)]
                return ss
            case _:
                return super().rco_stmt(s)
    
    def rco_exp(self, e: expr, need_atomic: bool) -> tuple[expr, Temporaries]:
        rco_exp = self.rco_exp
        atomize = self.atomize
        match e:
            case GlobalValue():
                return e,[]
            case Begin(ss,_e):
                _e,bs = rco_exp(_e,True)
                ss = self.rco_stmts(ss)+ make_assigns(bs)
                e = Begin(ss,_e)
                bs = []
                return atomize(e,bs) if need_atomic is True else (e,bs)
            case Subscript(tup_exp,Constant(idx),ctx=Load()):
                tup_exp,bs = rco_exp(tup_exp,True)
                e = Subscript(tup_exp,Constant(idx),ctx=Load())
                return atomize(e,bs) if need_atomic is True else (e,bs)
            case Allocate(i,t):
                e,bs = Allocate(i,t), []
                return atomize(e,bs) if need_atomic is True else (e,bs)
            case Call(Name('len'),[e]):
                e,bs = rco_exp(e)
                return atomize(e,bs) if need_atomic is True else (e,bs)
            case _:
                return super().rco_exp(e, need_atomic)
    
    # explicate_control pass
    def explicate_control(self, p) -> CProgram:
        return super().explicate_control(p)
    
    def explicate_effect(self, e, cont, basic_blocks: Blocks) -> List[stmt]:
        return super().explicate_effect(e, cont, basic_blocks)
    
    def explicate_stmt(self, s, cont, basic_blocks) -> List[stmt]:
        match s:
            case Collect():
                return [s] + cont
            case _:
                return super().explicate_stmt(s, cont, basic_blocks)
    
    def explicate_assign(self, rhs, lhs, cont, basic_blocks) -> List[stmt]:
        match rhs:
            case _:
                return super().explicate_assign(rhs, lhs, cont, basic_blocks)
    
    # select_instructions pass
    def select_instructions(self, p: CProgram) -> X86Program:
        return super().select_instructions(p)
    
    def attach_tag(self,size:int,ts:TupleType):
        if not (size <= 50):
            raise NotImplementedError("Length of tuple is limited to 50", size)
        '''
        [0] = yet to be copied to the ToSpace
        [1..6],[1:7] = length of tuple
        [7..56],[7:57] = pointer mask
        [57..60] = ???
        [61..63] = unused
        '''
        sz = 3
        sz = [int(s) for s in bin(sz)[2:]]
        sz = [0 for _ in range(len(sz),7)] + sz
        ptr_msk = [(1 if isinstance(t,TupleType) else 0) for t in ts.types] 
        ptr_msk = ptr_msk + [0 for _ in range(len(ptr_msk),50)]
        ptr_msk = ptr_msk + [0 for _ in range(57,64)]

        tag = sz + ptr_msk
        xs = ''.join([str(i) for i in tag])
        tag = int(xs,2)
        
        return tag
    
    def select_arg(self, e: expr) -> arg:
        match e:
            case Global():
                raise NotImplementedError()
            case GlobalValue(name):
                return GlobalValue(name)
            case _:
                return super().select_arg(e)
    
    def select_stmt(self, s: stmt) -> List[instr]:
        match s:
            case Assign([name],Subscript(tup_exp,Constant(i),ctx=Load())):
                var = self.select_arg(name)
                tup_exp = self.select_arg(tup_exp)
                return [
                    Instr('movq',[tup_exp,Reg('r11')]),
                    Instr('movq',[Deref('r11',8*(i+1)),var])
                ]
            case Assign([Subscript(tup_exp,Constant(i),ctx=Store())],rhs):
                rhs = self.select_arg(rhs)
                tup_exp = self.select_arg(tup_exp)
                return [
                    Instr('movq',[tup_exp,Reg('r11')]),
                    Instr('movq',[rhs,Deref('r11',8*(i+1))])
                ]
            case Assign([name],Allocate(sz,t)):
                var = self.select_arg(name)
                return [
                    Instr('movq',[GlobalValue('free_ptr'),Reg('r11')]),
                    Instr('addq',[Immediate(8*(sz+1)),GlobalValue('free_ptr')]),
                    Instr('movq',[Immediate(self.attach_tag(sz,t)), Deref('r11',0)]),
                    Instr('movq',[Reg('r11'),var])
                ]
            case Expr(Call(Name('len'), [tup])):
                var = self.select_arg(tup)
                raise NotImplementedError()
            case Collect(sz):
                return [
                    Instr('movq',[Reg('r15'),Reg('rdi')]),
                    Instr('movq',[Immediate(sz),Reg('rsi')]),
                    Callq('collect',2)
                ]
            case Assign([name],Compare(left,[Is()],[right])):
                var = self.select_arg(name)
                left = self.select_arg(left)
                right = self.select_arg(right)
                return [
                    Instr('cmpq',[right,left]),
                    Instr('sete',[ByteReg('al')]),
                    Instr('movzbq',[ByteReg('al'),var])
                ]
            case _:
                return super().select_stmt(s)
    
    # assign_homes
    def assign_homes(self, p: CProgram) -> CProgram:
        return super().assign_homes(p)
    
    # patch_instructions
    def patch_instructions(self, p: CProgram) -> CProgram:
        return super().patch_instructions(p)
    
    # prelude_and_conclusion
    def prelude_and_conclusion(self, p: CProgram) -> X86Program:
        prelude = [
            Instr('pushq',[Reg('rbp')]),
            Instr('movq',[Reg('rsp'),Reg('rbp')]),
            Instr('subq',[Immediate(16),Reg('rsp')]),
            Jump('start'),
        ]
        conclusion = [
            Instr('addq',[Immediate(16),Reg('rsp')]),
            Instr('popq',[Reg('rbp')]),
            Instr('retq',[]),
        ]
        match p:
            case CProgram(body):
                body[label_name('main')] = prelude
                body[label_name('conclusion')] = conclusion
                return X86Program(body)
            case _:
                raise NotImplementedError("prelude_and_conclusion unexpected",p)