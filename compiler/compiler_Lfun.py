'''context
This chapter studies the compilation of a subset of Python in which only top-level
function definitions are allowed. This kind of function appears in the C programming
language, and it serves as an important stepping-stone to implementing lexically
scoped functions in the form of lambda abstractions, which is the topic of chapter 9.

exp ::= Call(exp, exp*)
stmt ::= Return(exp)
params ::= (var,type)*
def ::= FunctionDef(var, params, stmt+, None, type, None)
LFun ::= Module([def ... stmt ...])
'''
from ast import *
from typing import List
from compiler.compiler import Temporaries
from x86_ast import *
from utils import (
    FunRef,IntType,TailCall,
    CProgram, stmt, CProgramDefs,
    make_assigns, Begin, TupleType,make_begin
    )
from compiler.compiler_Lif import Blocks
from compiler.compiler_Ltup import Compiler as Compiler_Ltup
from compiler.compiler_regalloc_Ltup import Compiler as Compiler_Regalloc

@dataclass
class ASM_COMMENT:
    content:str
    def __str__(self) -> str:
        return f'// {self.content}\n'

class Compiler(Compiler_Ltup):
    arg_pass_ord = Compiler_Regalloc.arg_pass_ord
    
    ### shrink
    def shrink(self, p: Module) -> Module:
        res = super().shrink(p)
        for i,fun_def in enumerate(res.body):
            if not isinstance(fun_def,FunctionDef):
                break
        return Module(res.body[:i] + [self.create_main_fun(res.body[i:])])
    
    def create_main_fun(self,ss:list[stmt]) -> FunctionDef:
        # ("name", "args", "body", "decorator_list", "returns", "type_comment")
        return FunctionDef('main', [], ss + [Return(Constant(0))], None, IntType(), None)
    
    def expand_stmts(self,ss:list[stmt]) -> list[stmt]:
        return [self.expand_stmt(s) for s in ss]
    
    def expand_stmt(self, s: stmt) -> stmt:
        match s:
            case FunctionDef(name=var, args=params, body=stmts, 
                             decorator_list=_, returns=type, type_comment=_):
                'https://docs.python.org/3/library/ast.html#ast.FunctionDef'
                return FunctionDef(var, params, self.expand_stmts(stmts), None, type, None)
            case Return(exp):
                return Return(self.expand_exp(exp))
            case _:
                return super().expand_stmt(s)
    
    
    ### reveal_functions pass
    def reveal_functions(self,p: Module) -> Module:
        self.init_global_functions(p.body)
        return Module(self.reveal_stmts(p.body))
    
    def init_global_functions(self, defns:list[FunctionDef]):
        self._global_functions = set(defn.name for defn in defns).union(
            ['input_int','print']
        )
    
    def is_global_function(self, name:str) -> bool:
        return name in self._global_functions
    
    def reveal_stmts(self,ss: list[stmt]) -> list[stmt]:
        return [self.reveal_stmt(s) for s in ss]
    
    def reveal_stmt(self,s:stmt) -> stmt:
        reveal_stmts = self.reveal_stmts
        reveal_exp = self.reveal_exp
        match s:
            case FunctionDef(var, params, ss, None, type, None):
                return FunctionDef(var, params, reveal_stmts(ss), None, type, None)
            case Return(exp):
                return Return(reveal_exp(exp))
            case If(test,body,orelse):
                return If(reveal_exp(test),reveal_stmts(body),reveal_stmts(orelse))
            case While(e,[*ss],[]):
                return While(reveal_exp(e),reveal_stmts(ss),[])
            case Expr(e):
                return Expr(reveal_exp(e))
            case Assign([Name(var)], e):
                return Assign([Name(var)],reveal_exp(e))
            case _:
                raise NotImplementedError('reveal_stmt, unexpected argument ', s)
    
    def reveal_exp(self,e:expr) -> expr:
        reveal_exp = self.reveal_exp
        match e:
            case Call(func=Name('print')|Name('input_int')|Name('len'), args=[*args]):
                func,args = e.func,e.args
                return Call(func, [reveal_exp(a) for a in args])
            case Call(f, [*args]):
                return Call(reveal_exp(f),[reveal_exp(a) for a in args])
            case Tuple([*es],Load()):
                return Tuple([reveal_exp(e) for e in es],Load())
            case Subscript(e,Constant(i),Load()):
                return Subscript(reveal_exp(e),Constant(i),Load())
            case IfExp(pred,conseq,alter):
                return IfExp(reveal_exp(pred),reveal_exp(conseq),reveal_exp(alter))
            case Compare(left,[op],[right]):
                return Compare(reveal_exp(left),[op],[reveal_exp(right)])
            case BinOp(left,op,right):
                return BinOp(reveal_exp(left),op,reveal_exp(right))
            case UnaryOp(op,_e):
                return UnaryOp(op,reveal_exp(_e))
            case Name(id) if self.is_global_function(id):
                return FunRef(id,-1)
            case Constant()|Name():
                return e
            case _:
                raise NotImplementedError('reveal_exp, unexpected argument ',e)
    
    ### limit_functions pass
    def limit_functions(self,p: Module) -> Module:
        return Module(self.limit_stmts(p.body, {}))
        
    def limit_stmts(self, ss: list[stmt], home: dict[str]) -> list[stmt]:
        return [self.limit_stmt(s, home) for s in ss]
    
    def limit_stmt(self, s: stmt, home:dict[str]) -> stmt:
        limit_stmts = lambda s: self.limit_stmts(s, home)
        limit_exp = lambda e: self.limit_exp(e, home)
        match s:
            case FunctionDef(var, [p1,p2,p3,p4,p5,p6,*ps], ss, None, type, None):
                _home = {p:Subscript(Name(p6[0]+'_tup'),Constant(i),Load()) 
                         for i,(p,t) in enumerate([p6,*ps])}
                p6 = p6[0]+'_tup',TupleType([t for p,t in [p6,*ps]])
                params = [p1,p2,p3,p4,p5,p6]
                return FunctionDef(var, params, self.limit_stmts(ss,_home),
                                   None, type, None)
            case FunctionDef(var, params, ss, None, type, None):
                return FunctionDef(var, params, limit_stmts(ss), 
                                   None, type, None)
            case Return(exp):
                return Return(limit_exp(exp))
            case If(test,body,orelse):
                return If(limit_exp(test),limit_stmts(body),limit_stmts(orelse))
            case While(e,[*ss],[]):
                return While(limit_exp(e),limit_stmts(ss),[])
            case Expr(e):
                return Expr(limit_exp(e))
            case Assign([Name(var)], e):
                match home.get(var,Name(var)):
                    case Subscript(Name(tup),Constant(i),Load()):
                        return Assign([Subscript(Name(tup),Constant(i),Store())],limit_exp(e))
                    case Name(var):
                        return Assign([Name(var)],limit_exp(e))
            case _:
                raise NotImplementedError('limit_stmt, unexpected argument ', s)
    
    def limit_exp(self, e: expr, home: dict[str]) -> expr:
        limit_exp = lambda e: self.limit_exp(e,home)
        match e:
            case Call(f,[e1,e2,e3,e4,e5,e6,*es]):
                return Call(limit_exp(f),
                            [*[limit_exp(e) for  e in [e1,e2,e3,e4,e5]],
                             Tuple([limit_exp(e) for e in [e6,*es]],Load())])
            case Call(f,[*es]):
                return Call(limit_exp(f),[limit_exp(e) for e in es])
            case FunRef(f,arity):
                return FunRef(f,arity)
            case Tuple([*es],Load()):
                return Tuple([limit_exp(e) for e in es],Load())
            case Subscript(e,Constant(i),Load()):
                return Subscript(limit_exp(e),Constant(i),Load())
            case IfExp(pred,conseq,alter):
                return IfExp(limit_exp(pred),limit_exp(conseq),limit_exp(alter))
            case Compare(left,[op],[right]):
                return Compare(limit_exp(left),[op],[limit_exp(right)])
            case BinOp(left,op,right):
                return BinOp(limit_exp(left),op,limit_exp(right))
            case UnaryOp(op,_e):
                return UnaryOp(op,limit_exp(_e))
            case Name(id):
                return home.get(id,Name(id))
            case Constant():
                return e
            case _:
                raise NotImplementedError('reveal_exp, unexpected argument ',e)
    
    ### expose_allocation
    def expose_exp(self, e):
        match e:
            case FunRef():
                return e
            case _:
                return super().expose_exp(e)
    
    def expose_stmt(self, s: stmt) -> stmt:
        match s:
            case FunctionDef(var, params, ss, None, type, None):
                return FunctionDef(var, params, self.expose_stmts(ss), None, type, None)
            case Return(e):
                return Return(self.expose_exp(e))
            case _:
                return super().expose_stmt(s)

    ### remove_complex_operands pass
    def rco_stmt(self, s: stmt) -> List[stmt]:
        match s:
            case FunctionDef(var, params, ss, None, type, None):
                return [FunctionDef(var, params, self.rco_stmts(ss), None, type, None)]
            case Return(e):
                e,bs = self.rco_exp(e,False)
                return make_assigns(bs) + [Return(e)]
            case _:
                return super().rco_stmt(s)
    
    def rco_exp(self, e: expr, need_atomic: bool) -> tuple[expr, Temporaries]:
        atomize = self.atomize
        rco_exp = self.rco_exp
        match e:
            case Call(f_exp,[*_es]):
                f_exp,bs = rco_exp(f_exp,True)
                es = []
                for _e in _es:
                    _e,_bs = self.rco_exp(_e,True)
                    es += [_e]
                    bs += _bs
                e = Call(f_exp,es)
                return atomize(e,bs,need_atomic)
            case FunRef(f,arity):
                e,bs = FunRef(f,arity),[]
                return atomize(e,bs,need_atomic)
            case _:
                return super().rco_exp(e, need_atomic)
    
    
    ### explicate_control pass
    def explicate_control(self, p: Module) -> CProgramDefs:
        match p:
            case Module([*defns]):
                return CProgramDefs([self.explicate_function(defn) for defn in defns])
            case _:
                raise NotImplementedError('explicate_control',p)
    
    def explicate_function(self, defn: FunctionDef) -> FunctionDef:
        match defn:
            case FunctionDef(var, params, ss, None, type, None):
                basic_block = dict()
                cont = self.explicate_stmts(ss,[],basic_block)
                basic_block[label_name(var)+'_start'] = cont
                return FunctionDef(label_name(var), params, basic_block, None, type, None)
            case _:
                raise NotImplementedError('explicate_function, bad arguments ', defn)
    
    def explicate_stmt(self, s, cont, basic_blocks) -> List[stmt]:
        match s:
            case Return(e):
                return self.explicate_tail(e,basic_blocks)
            case _:
                return super().explicate_stmt(s, cont, basic_blocks)
        
    def explicate_tail(self, e, basic_blocks) -> List[stmt]:
        explicate_tail = self.explicate_tail
        match e:
            case (Constant()
                  |Name()|Subscript()
                  |UnaryOp()|BinOp()|Compare()):
                return [Return(e)]
            case Call(Name('input_int'),[]):
                return [Return(e)]
            case Begin(ss,exp):
                tail = explicate_tail(exp,basic_blocks)
                return self.explicate_stmts(ss,tail,basic_blocks)
            case IfExp(pred,conseq,alter):
                conseq = explicate_tail(conseq,basic_blocks)
                alter = explicate_tail(alter,basic_blocks)
                return self.explicate_pred(pred,conseq,alter,basic_blocks)
            case Call(Name('len'),[atm]):
                return [Return(e)]
            case Call(FunRef(f,argc),[*atm]):
                return [TailCall(FunRef(f,argc),[*atm])]
            case Call(Name(id),[*atm]):
                return [TailCall(Name(id),[*atm])]
            case FunRef(f,argc):
                return [Return(FunRef(f,argc))]
            case _:
                raise NotImplementedError('explicate_tail, bad argument ', e)
    
    def explicate_assign(self, rhs, lhs, cont, basic_blocks) -> List[stmt]:
        match rhs:
            case Call(FunRef(f,arg_n), [*atm]):
                return [Assign([lhs], rhs)] + cont
            case _:
                return super().explicate_assign(rhs, lhs, cont, basic_blocks)
    
    def explicate_pred(self, cnd, thn: List[stmt], els: List[stmt], basic_blocks: Blocks) -> List[stmt]:
        create_block = self.create_block
        goto_thn = lambda: create_block(thn, basic_blocks)
        goto_els = lambda: create_block(els, basic_blocks)
        match cnd:
            case Call(Name(id),[*atm]):
                cnd,bs = self.rco_exp(cnd,True)
                return self.explicate_pred(make_begin(bs,cnd),thn,els,basic_blocks)
            case Call(FunRef(f,arg_n), [*atm]):
                cnd,bs = self.rco_exp(cnd,True)
                return self.explicate_pred(make_begin(bs,cnd),thn,els,basic_blocks)
            case _:
                return super().explicate_pred(cnd, thn, els, basic_blocks)
    
    ### select_instructions
    def select_instructions(self, p: CProgramDefs) -> X86ProgramDefs:
        match p:
            case CProgramDefs([*defns]):
                return X86ProgramDefs([self.select_fun(defn) for defn in defns])
            case _:
                raise NotImplementedError('select_instructions, bad argument ', p)
            
    def select_fun(self, defn:FunctionDef) -> FunctionDef:
        match defn:
            case FunctionDef(var, params, blocks, None, type, None):
                assert(len(params) <= 6)
                
                self.var_types = defn.var_types
                blocks = {lbl:self.select_stmts(ss,var) for lbl,ss in blocks.items()}
                pass_arg_instrs = [Instr('movq',[r,Variable(p)]) 
                                   for r,(p,t) in zip(self.arg_pass_ord,params)]
                
                # pass_arg_instrs = [ASM_COMMENT(f'{var}: moving arg for reg alloc')] + pass_arg_instrs + [ASM_COMMENT(f'{var}: end of reg alloc moving')]
                
                blocks[var+'_start'] =  pass_arg_instrs + blocks[var+'_start']
                res = FunctionDef(var,[],blocks,None,IntType(),None)
                res.var_types = defn.var_types
                return res
            case _:
                raise NotImplementedError('explicate_function, bad arguments ', defn)
    
    def select_stmts(self, ss: List[stmt], f = '') -> List[instr]:
        from functools import reduce
        from operator import add
        return reduce(add,[self.select_stmt(s,f) for s in ss])
         
    def select_stmt(self, s: stmt, f = '') -> List[instr]:
        select_arg = self.select_arg
        match s:
            case Assign([name],FunRef(f,arity)):
                return [
                    Instr('leaq',[Global(f),select_arg(name)])
                ]
            case (Expr(Call(Name('print')))
                  |Assign(value=Call(Name('input_int')))
                  |Assign(value=Call(Name('len')))
                ):
                return super().select_stmt(s)
            case Assign([name],Call(_f,[*atms])):
                return  [*[
                    Instr('movq',[select_arg(atm),reg])
                    for atm,reg in zip(atms,self.arg_pass_ord)],
                    IndirectCallq(select_arg(_f),len(atms)),
                    Instr('movq',[Reg('rax'),select_arg(name)]),
                    ]
            case TailCall(f,[*atms]):
                return [*[Instr('movq',[select_arg(atm),reg])
                          for atm,reg in zip(atms,self.arg_pass_ord)],
                        TailJump(select_arg(f),len(atms)),
                        ]
            case Return(value=FunRef()|Constant()
                        |Name()|Subscript()
                        |UnaryOp()|BinOp()|Compare()):
                return self.select_stmt(
                    Assign([Reg('rax')],s.value),f) + [
                    Jump(label_name(f + '_conclusion')),
                ]
            case Return(e):
                return [
                    Instr('movq',[select_arg(e),Reg('rax')]),
                    Jump(label_name(f + '_conclusion')),
                ]
            case _:
                return super().select_stmt(s)
    
    def select_arg(self, e:expr):
        match e:
            case Reg():
                return e
            case _:
                return super().select_arg(e)
    
    ### assign_homes
    def assign_homes(self, p :X86ProgramDefs):
        match p:
            case X86ProgramDefs([*defns]):
                return X86ProgramDefs([self.assign_fun(defn) for defn in defns])
            case _:
                raise NotImplementedError('select_instructions, bad argument ', p)
    
    
    def assign_fun(self, defn: FunctionDef) -> FunctionDef:
        match defn:
            case FunctionDef(var,[],blocks,None,IntType(),None):
                blocks = CProgram(blocks)
                blocks.var_types = defn.var_types
                blocks,home = super().assign_homes_spilled(blocks)
                defn = FunctionDef(var,[],blocks.body,None,IntType(),None)
                defn.spilled = set(home['var'].keys())
                defn.tuples = set(home['tuple'].keys())
                return defn
            case _:
                raise NotImplementedError()
    
    def assign_homes_instr(self, i: Instr, home: dict[Variable,arg]) -> Instr:
        assign_homes_arg = lambda a: self.assign_homes_arg(a,home)
        match i:
            case ASM_COMMENT():
                return i
            case TailJump(func,arity):
                return TailJump(assign_homes_arg(func),arity)
            case IndirectCallq(func,argc):
                return IndirectCallq(self.assign_homes_arg(func,home),argc)
            case _:
                return super().assign_homes_instr(i,home)
    
    # patch_instructions
    def patch_instructions(self, p: X86ProgramDefs) -> X86ProgramDefs:
        match p:
            case X86ProgramDefs([*defns]):
                return X86ProgramDefs([self.patch_fun(defn) for defn in defns])
            case _:
                raise NotImplementedError('patch_instructions')
            
    
    def patch_fun(self, defn: FunctionDef) -> FunctionDef:
        match defn:
            case FunctionDef(var,[],blocks,None,IntType(),None):
                blocks = super().patch_instructions(CProgram(blocks)).body
                _defn = FunctionDef(var,[],blocks,None,IntType(),None)
                _defn.spilled,_defn.tuples = defn.spilled,defn.tuples
                return _defn
            case _:
                raise NotImplementedError('patch_fun')
    
    def patch_instr(self, i: Instr) -> list[Instr]:
        match i:
            case ASM_COMMENT():
                return [i]
            case Instr('leaq',[a,Deref(reg,offset)]):
                return [
                    Instr('leaq',[a,Reg('rax')]),
                    Instr('movq',[Reg('rax'),Deref(reg,offset)]),
                ]
            case IndirectCallq():
                return [i]
            case TailJump(func,arity) if func != Reg('rax'):
                return [*self.patch_instr(Instr('movq',[func,Reg('rax')])),
                        TailJump(Reg('rax'),arity)]
            case _:
                return super().patch_instr(i)
    
    # prelude_and_conclusion
    def prelude_and_conclusion(self, p: X86ProgramDefs) -> X86Program:
        match p:
            case X86ProgramDefs([*defns]):
                from functools import reduce
                combine = lambda a,b : a|b
                return X86ProgramFun(reduce(combine,[self.prelude_and_conclusion_each(defn) for defn in defns]))
            case _:
                raise NotImplementedError("prelude_and_conclusion, unexpected", p)
    
    def prelude_and_conclusion_each(self, defn:FunctionDef) -> dict[str,list[instr]]:
        match defn:
            case FunctionDef(var,[],blocks,None,IntType(),None):
                sz = len(defn.spilled)*8
                sz = sz if sz%16 == 0 else sz+8
                root_stack_sz = len(defn.tuples)*8
                
                prelude = [
                    Instr('pushq',[Reg('rbp')]),
                    Instr('movq',[Reg('rsp'),Reg('rbp')]),
                    Instr('subq',[Immediate(sz),Reg('rsp')]), # room for spilled
                ]
                prelude_init_gc = [
                    Instr('movq',[Immediate(2**16),Reg("rdi")]),
                    Instr('movq',[Immediate(2**16),Reg("rsi")]),
                    Callq("initialize",2), # void initialize(uint64_t rootstack_size, uint64_t heap_size)
                    Instr('movq',[Global('rootstack_begin'),Reg('r15')]),
                    *[Instr('movq',[Immediate(0),Deref('r15',i*8)]) 
                      for i,*_ in enumerate(defn.tuples)],
                ] if var == 'main' else []
                prelude_init_gc += [
                    Instr('addq',[Immediate(root_stack_sz),Reg('r15')]),
                ]
                jmp = [Jump(label_name(var)+'_start'),]
                
                conclusion_gc = [
                    Instr('subq',[Immediate(root_stack_sz),Reg('r15')])
                ]
                conclusion = [
                    Instr('addq',[Immediate(sz),Reg('rsp')]),
                    Instr('popq',[Reg('rbp')]),
                    Instr('retq',[]),
                ]
                
                def translate_tail(i:instr):
                    match i:
                        case TailJump(f,arity):
                            return [
                                Instr('addq',[Immediate(sz),Reg('rsp')]),
                                Instr('popq',[Reg('rbp')]),
                                IndirectJump(f),
                            ]
                        case _:
                            return [i]
                
                from functools import reduce
                from operator import add
                blocks[label_name(var)] = prelude + prelude_init_gc + jmp
                blocks[label_name(var)+'_conclusion'] = conclusion_gc + conclusion
                blocks = {lbl:reduce(add,(translate_tail(i) for i in instrs)) for lbl,instrs in blocks.items()}
                
                return {label_name(var):blocks}
            case _:
                raise NotImplementedError("prelude_and_conclusion_each, unexpected ", defn)

class X86ProgramFun(X86Program):
    def __str__(self):
        result = ''
        for fn,blocks in self.body.items():
            for (l,ss) in blocks.items():
                if l == label_name('main'):
                    result += '\t.globl ' + label_name('main') + '\n'
                if fn == l:
                    result += '\t.align 8\n'
                result += l + ':\n'
                indent()
                result += ''.join([str(s) for s in ss]) + '\n'
                dedent()
        return result