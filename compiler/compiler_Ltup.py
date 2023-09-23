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
But Ltup is very limited such that
cmp ::= is
exp ::= exp,…,exp | exp[int] | len(exp)
LTup ::= stmt*
where constant index is allowed

root stack pointer (r15)
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
            case While(exp,[*ss],[]):
                return While(expose_exp(exp),expose_stmts(ss),[])
            case _:
                raise NotImplementedError('expose_stmt, unexpected', s)
    
    def expose_exp(self, e):
        expose_exp = self.expose_exp
        match e:
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
            case IfExp(pred,conseq,alter):
                return IfExp(expose_exp(pred),expose_exp(conseq),expose_exp(alter))
            case BinOp(e1,op,e2):
                return BinOp(expose_exp(e1),op,expose_exp(e2))
            case UnaryOp(op,_e):
                return UnaryOp(op,expose_exp(_e))
            case Compare(left,[op],[right]):
                return Compare(expose_exp(left),[op],[expose_exp(right)])
            case Call(fn,[*es]):
                return Call(fn,[expose_exp(e) for e in es])
            case Subscript(tup_exp,Constant(idx),ctx=ctx):
                return Subscript(expose_exp(tup_exp),Constant(idx),ctx=ctx)
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
                ss = self.rco_stmts(ss) + make_assigns(bs)
                e = Begin(ss,_e)
                bs = []
                return atomize(e,bs) if need_atomic is True else (e,bs)
            case Subscript(tup_exp,Constant(idx),ctx=Load()):
                tup_exp,bs = rco_exp(tup_exp,True)
                e = Subscript(tup_exp,Constant(idx),ctx=Load())
                return atomize(e,bs) if need_atomic is True else (e,bs)
            case Allocate(sz,ty):
                bs = []
                return atomize(e,bs) if need_atomic is True else (e,bs)
            case Call(Name('len'),[e]):
                e,bs = rco_exp(e,True)
                e = Call(Name('len'),[e])
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
        assert(hasattr(p,'var_types'))
        res = super().select_instructions(p)
        res.var_types = p.var_types
        return res
    
    def attach_tag(self,size:int,ts:TupleType):
        if not (size <= 50):
            raise NotImplementedError("Length of tuple is limited to 50", size)
        '''
        [0] = whether the tuple has yet to be copied to the ToSpace. If the bit has value 1, then this tuple has not yet been copied. If the bit has value 0, then the entire tag is a forwarding pointer.
        [1..6],[1:7] = length of tuple
        [7..56],[7:57] = pointer mask
        [57..60] = ???
        [61..63] = unused
        '''
        size = '0'*(6 - size.bit_length()) + bin(size)[2:]
        ptr_msk = ''.join(('1' if isinstance(t,TupleType) else '0') for t in reversed(ts.types))
        tag = ptr_msk+size+'1'
        return int(tag,2)
    
    def select_arg(self, e: expr) -> arg:
        match e:
            case GlobalValue(name):
                return Global(name)
            case _:
                return super().select_arg(e)
    
    def select_stmt(self, s: stmt) -> List[instr]:
        match s:
            case Assign([Subscript()],Subscript()):
                raise NotImplementedError()
            case Assign([Subscript(tup_exp,Constant(i),ctx=Store())],rhs):
                rhs = self.select_arg(rhs)
                tup_exp = self.select_arg(tup_exp)
                return [
                    Instr('movq',[tup_exp,Reg('r11')]),
                    Instr('movq',[rhs,Deref('r11',8*(i+1))])
                ]
            case Assign([name],Subscript(tup_exp,Constant(i),ctx=Load())):
                var = self.select_arg(name)
                tup_exp = self.select_arg(tup_exp)
                return [
                    Instr('movq',[tup_exp,Reg('r11')]),
                    Instr('movq',[Deref('r11',8*(i+1)),var]),
                ]
            case Assign([name],Allocate(sz,t)):
                var = self.select_arg(name)
                return [
                    Instr('movq',[Global('free_ptr'),Reg('r11')]),
                    Instr('addq',[Immediate(8*(sz+1)),Global('free_ptr')]),
                    Instr('movq',[Immediate(self.attach_tag(sz,t)), Deref('r11',0)]),
                    Instr('movq',[Reg('r11'),var]),
                ]
            case Assign([name],Call(Name('len'), [tup_exp])):
                # (2**6-1) & (*tup >> 1) 
                var = self.select_arg(name)
                tup_exp = self.select_arg(tup_exp)
                return [
                    Instr('movq',[tup_exp,Reg('r11')]),
                    Instr('movq',[Deref('r11',0),var]),
                    Instr('sarq',[Immediate(1),var]),
                    Instr('andq',[Immediate(2**6-1),var]),
                ]
            case Collect(sz):
                return [
                    Instr('movq',[Reg('r15'),Reg('rdi')]),
                    Instr('movq',[Immediate(sz),Reg('rsi')]),
                    Callq('collect',2),
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
        assert(hasattr(p,'var_types'))
        self.var_types = p.var_types
        self.spilled = set()
        self.tuples = set()
        res = super().assign_homes(p)
        return res
    
    def get_var_type(self,name:str):
        return self.var_types[name]
    
    def assign_homes_arg(self, a: arg, home: dict[Variable, arg]) -> arg:
        # rbp for variable spilled
        # r15 for root stack spilled
        match a:
            case Variable(id) if isinstance(self.get_var_type(id),TupleType):
                self.tuples.add(id)
                if a not in home:
                    home[a] = Deref('r15',offset = -8*(len(self.tuples)+1))
                return home[a]
            case Variable(id):
                self.spilled.add(id)
                if a not in home:
                    home[a] = Deref('rbp',offset = -8*(len(self.spilled)+1))
                return home[a]
            case Deref('r11',offset=offset):
                return a
            case Immediate()|Reg():
                return a
            case Global(name):
                return Global(name)
            case _:
                raise NotImplementedError("assign_homes_arg, bad argument: ", a)
    
    # patch_instructions
    def patch_instructions(self, p: CProgram) -> CProgram:
        return super().patch_instructions(p)
    
    def is_mem_ref(self,a: arg) -> bool:
        return isinstance(a,Global) or super().is_mem_ref(a)
    
    # prelude_and_conclusion
    def prelude_and_conclusion(self, p: CProgram) -> X86Program:
        sz = len(self.spilled)*8
        sz = sz if sz%16 == 0 else sz+8
        root_stack_sz = len(self.tuples)*8
        prelude = [
            Instr('pushq',[Reg('rbp')]),
            Instr('movq',[Reg('rsp'),Reg('rbp')]),
            Instr('subq',[Immediate(sz),Reg('rsp')]),
        ]
        prelude_init_gc = [
            Instr('movq',[Immediate(65536),Reg("rdi")]),
            Instr('movq',[Immediate(16),Reg("rsi")]),
            # void initialize(uint64_t rootstack_size, uint64_t heap_size);
            Callq("initialize",2),
            Instr('movq',[Global('rootstack_begin'),Reg('r15')]),
            Instr('movq',[Immediate(0),Deref('r15',0)]),
            Instr('addq',[Immediate(root_stack_sz),Reg('r15')]),
        ]
        jmp = [Jump('start'),]
        conclusion_gc = [
            Instr('subq',[Immediate(root_stack_sz),Reg('r15')])
        ]
        conclusion = [
            Instr('addq',[Immediate(sz),Reg('rsp')]),
            Instr('popq',[Reg('rbp')]),
            Instr('retq',[]),
        ]
        match p:
            case CProgram(body):
                body[label_name('main')] = prelude + prelude_init_gc + jmp
                body[label_name('conclusion')] = conclusion_gc + conclusion
                return X86Program(body)
            case _:
                raise NotImplementedError("prelude_and_conclusion unexpected",p)