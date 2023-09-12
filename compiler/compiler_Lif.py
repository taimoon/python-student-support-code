from ast import *
from compiler.compiler import Temporaries
from utils import (
    generate_name, Begin, CProgram,
    make_assigns, make_begin, Goto, stmt,
    )
from x86_ast import *
from typing import (List,Tuple,Dict)

Blocks = Dict[str,List[stmt]]
import compiler.compiler as compiler

class Compiler(compiler.Compiler):
    # expansion
    def shrink(self,p:Module) -> Module:
        return Module([self.expand_stmt(s) for s in p.body])
    
    def expand_stmt(self,s:stmt) -> stmt:
        expand_exp = self.expand_exp
        expand_stmt = self.expand_stmt
        match s:
            case If(test,body,orelse):
                return If(expand_exp(test),expand_stmt(body),expand_stmt(orelse))
            case Assign([name], e):
                return Assign([name],expand_exp(e))
            case Expr(e):
                return Expr(expand_exp(e))
            case _:
                return s
    
    def expand_exp(self,e):
        expand = self.expand_exp
        match e:
            case BoolOp():
                return self.expand_bool_op(e)
            case Call(name, [*args]):
                return Call(name, [expand(a) for a in args])
            case _:
                return e
    
    def expand_bool_op(self,e):
        expand = self.expand_bool_op
        match e:
            case BoolOp(And(),[]):
                return Constant(True)
            case BoolOp(And(),[e,*es]):
                return IfExp(e,expand(BoolOp(And(),[*es])),Constant(False))
            case BoolOp(Or(),[]):
                return Constant(False)
            case BoolOp(Or(),[e,*es]):
                return IfExp(e,Constant(True),expand(BoolOp(Or(),[*es])))
            case _:
                raise NotImplementedError('expand_bool_op',e)
    
    # single assignment
    def remove_complex_operands(self, p: Module) -> Module:
        return super().remove_complex_operands(p)
    
    def rco_exp(self, e: expr, need_atomic: bool) -> Tuple[expr, Temporaries]:
        rco_exp = self.rco_exp
        atomize = self.atomize
        match e:
            case IfExp(test,body,orelse):
                test,t_bs = rco_exp(test,True)
                
                body,bs = rco_exp(body,False)
                body = make_begin(bs,body)
                
                orelse,bs = rco_exp(orelse,False)
                orelse = make_begin(bs,orelse)
                
                e,bs = IfExp(test,body,orelse),t_bs
                return atomize(e,bs) if need_atomic is True else (e,bs)
            case Compare(left,[cmp],[right]):
                l,l_bs = self.rco_exp(left, True)
                r,r_bs = self.rco_exp(right, True)
                e = Compare(l,[cmp],[r])
                bs = l_bs + r_bs
                return atomize(e,bs) if need_atomic is True else (e,bs)
            case Begin(ss,_e):
                raise NotImplementedError('rco_exp, unexpected: ', e)
            case _:
                return super().rco_exp(e, need_atomic)
    
    def rco_stmt(self, s: stmt) -> List[stmt]:
        match s:
            case If(test,body,orelse):
                test,t_bs = self.rco_exp(test,True)
                
                body = self.rco_stmts(body)
                # body = make_assigns(b_bs) + [body]
                
                orelse = self.rco_stmts(orelse)
                # orelse = make_assigns(e_bs) + [orelse]
                
                return make_assigns(t_bs) + [If(test,body,orelse)]
            case _:
                return super().rco_stmt(s)
    
    # explicate
    def explicate_control(self, p: List[stmt]) -> CProgram:
        match p:
            case Module(body):
                cont = [Return(Constant(0))]
                basic_blocks = {}
                cont = self.explicate_stmts(body,cont,basic_blocks)
                basic_blocks[label_name('start')] = cont
                return CProgram(basic_blocks)
            case _:
                raise NotImplementedError('explicate_control',p)
    
    def explicate_stmts(self, ss:list[stmt],cont,basic_blocks) -> list[stmt]:
        for s in reversed(ss):
            cont = self.explicate_stmt(s,cont,basic_blocks)
        return cont
    
    def create_block(self,stmts:List[stmt],basic_blocks:Blocks) \
        -> List[Goto]:
        match stmts:
            case [Goto(l)]:
                return stmts
            case _:
                label = label_name(generate_name('block'))
                basic_blocks[label] = stmts
                return [Goto(label)]

    def explicate_effect(self, e, cont, basic_blocks:Blocks) \
        -> List[stmt]:
        match e:
            case IfExp(test, body, orelse):
                raise NotImplementedError()
            case Begin(body, result):
                raise NotImplementedError()
            case Call(Name('print'|'input_int'),_)|BinOp()|Compare():
                return self.create_block([Expr(e)] + cont,basic_blocks)
            case _:
                raise NotImplementedError(self.explicate_effect.__name__,'unexpected: ',e)

    def explicate_assign(self, rhs, lhs, cont, basic_blocks) \
        -> List[stmt]:
        explicate_assign = self.explicate_assign
        match rhs:
            case IfExp(test, body, orelse):
                body = explicate_assign(body,lhs,cont,basic_blocks)
                orelse = explicate_assign(orelse,lhs,cont,basic_blocks)
                test = self.explicate_pred(test,body,orelse,basic_blocks)
                return test
            case Begin(body, result):
                cont = explicate_assign(result,lhs,cont,basic_blocks)
                for s in reversed(body):
                    cont = self.explicate_stmt(s,cont,basic_blocks)
                return cont
            case _:
                return [Assign([lhs], rhs)] + cont
    
    def explicate_pred(self, cnd, thn: List[stmt], els: List[stmt], basic_blocks: Blocks) \
        -> List[stmt]:
        create_block = self.create_block
        goto_thn = lambda: create_block(thn, basic_blocks)
        goto_els = lambda: create_block(els, basic_blocks)
        match cnd:
            case Compare(left, [op], [right]):
                return [If(cnd, goto_thn(), goto_els())]
            case Constant(True):
                return thn
            case Constant(False):
                return els
            case UnaryOp(Not(), operand):
                return [If(operand, goto_els(), goto_thn())]
            case IfExp(test, body, orelse):
                raise NotImplementedError()
            case Begin(body, result):
                cont = self.explicate_pred(result,thn,els,basic_blocks)
                for s in reversed(body):
                    cont = self.explicate_stmt(s,cont,basic_blocks)
                return cont
            case _:
                cnd = Compare(cnd,[Eq()],[Constant(False)])
                return [If(cnd, goto_els(), goto_thn())]
    
    def explicate_stmt(self, s, cont, basic_blocks) -> List[stmt]:
        match s:
            case Assign([lhs], rhs):
                return self.explicate_assign(rhs,lhs,cont,basic_blocks)
            case Expr(value):
                return self.explicate_effect(value,cont,basic_blocks)
            case If(test,body,orelse):
                # TODO: workaround
                cont = self.create_block(cont,basic_blocks)
                body = self.explicate_stmts(body,cont,basic_blocks)
                orelse = self.explicate_stmts(orelse,cont,basic_blocks)
                return self.explicate_pred(test,body,orelse,basic_blocks)
            case _:
                raise NotImplementedError('explicate_stmt',s)
    
    # select instruction
    def select_instructions(self, p: CProgram) -> X86Program:
        match p:
            case CProgram(body):
                return CProgram({lbl:self.select_stmts(ss) for lbl,ss in body.items()})
            case _:
                return super().select_instructions(p)
    
    def select_arg(self, e: expr) -> arg:
        match e:
            case Constant(True):
                return Immediate(1)
            case Constant(False):
                return Immediate(0)
            case _:
                return super().select_arg(e)
    
    cc_corpsd = {Eq : 'e',NotEq : 'ne', Lt  : 'l',LtE : 'le',Gt  : 'g',GtE : 'ge'}
    def select_cc(self,cmp) -> str:
        return self.cc_corpsd[cmp.__class__]
    
    def select_stmt(self, s: stmt) -> List[instr]:
        select_arg = self.select_arg
        match s:
            case Assign([name],UnaryOp(Not(),v)):
                var = select_arg(name)
                arg = select_arg(v)
                if arg == var:
                    return [Instr('xorq',[Immediate(1),var])]
                else:
                    return [Instr('movq',[arg,var]),
                            Instr('xorq',[Immediate(1),var])]
            case Assign([name],Compare(left,[cmp],[right])):
                '''
                cmpq rhs lhs # lhs < rhs
                '''
                var = select_arg(name)
                right = select_arg(right)
                left = select_arg(left)
                cc = 'set' + self.select_cc(cmp)
                return [
                    Instr('cmpq',[right,left]),
                    Instr(cc,[ByteReg('al')]),
                    Instr('movzbq',[ByteReg('al'),var])
                ]
            case If(Compare(left,[cmp],[right]),[Goto(conseq)],[Goto(alter)]):
                left = select_arg(left)
                right = select_arg(right)
                cc = self.select_cc(cmp)
                return [
                    Instr('cmpq',[right,left]),
                    JumpIf(cc,conseq),
                    Jump(alter),
                ]
            case Goto(label):
                return [Jump(label)]
            case Return(v):
                v = select_arg(v)
                return [Instr('movq',[v,Reg('rax')]),
                        Jump('conclusion'),]
            case _:
                return super().select_stmt(s)
    
    # assign homes
    def assign_homes(self, p: CProgram) -> CProgram:
        match p:
            case CProgram(body):
                home = {}
                for lbl,ss in body.items():
                    body[lbl] = self.assign_homes_instrs(ss,home)
                # body = {lbl:self.assign_homes_instrs(ss,home) for lbl,ss in body.items()}
                return CProgram(body)
            case _:
                return super().assign_homes(p)
    
    
    def assign_homes_instr(self, i: instr,
                           home: Dict[Variable, arg]) -> instr:
        match i:
            case Jump()|JumpIf():
                return i
            case _:
                return super().assign_homes_instr(i,home)
    
    # patch_instructions
    def patch_instructions(self, p: CProgram) -> CProgram:
        match p:
            case CProgram(body):
                return CProgram({lbl:self.patch_instrs(ss) for lbl,ss in body.items()})
            case _:
                raise NotImplementedError('patch_instructions, unknown argument', p)
        
    def patch_instr(self, i: instr) -> list[instr]:
        match i:
            case Instr('cmpq',[arg1,Immediate(v)]):
                instrs = [
                    Instr('movq',[Immediate(v),Reg('rax')]),
                    Instr('cmpq',[arg1,Reg('rax')]),
                ]
                return self.patch_instrs(instrs)
            case Instr('movzbq',[arg1,Deref(reg,offset)]):
                instrs = [
                    Instr('movzbq',[arg1,Reg('rax')]),
                    Instr('movq',[Reg('rax'),Deref(reg,offset)]),
                ]
                return self.patch_instrs(instrs)
            case Jump()|JumpIf():
                return [i]
            case _:
                return super().patch_instr(i)
    
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
        