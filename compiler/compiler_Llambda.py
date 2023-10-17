from ast import *
from ast import List, Module, expr, stmt
from typing import List
from compiler.compiler import Temporaries
from x86_ast import *
from utils import (
    FunRef,Uninitialized,UncheckedCast,Allocate,
    make_assigns, Begin,
    generate_name, Closure,
    AllocateClosure,
    Type,Bottom,TupleType,IntType,FunctionType,BoolType,VoidType,
    GlobalValue,Collect,
    )
from compiler.compiler_Lfun import Compiler as Compiler_Lfun
from compiler.compiler_regalloc_Ltup import Compiler as Compiler_Regalloc

class Compiler(Compiler_Lfun):
    # shrink pass
    def expand_exp(self, e):
        match e:
            case Lambda([*vars],exp):
                return Lambda([*vars],self.expand_exp(exp))
            case _:
                return super().expand_exp(e)
    
    # uniquify pass
    def uniquify(self,p:Module) -> Module:
        match p:
            case Module([*stmts]):
                return Module(self.uniquify_stmts(stmts,self.init_env(stmts)))
            case _:
                raise NotImplementedError("uniquify, bad argument: ", p)
            
    def init_env(self,ss: list[stmt] = ()) -> dict[str,str]:
        globals = ['print','input_int','len','arity']
        for s in ss:
            match s:
                case FunctionDef(var,_):
                    globals.append(var)
                case _:
                    pass
        return {v:v for v in globals}
    
    def uniquify_stmts(self,ss:list[stmt],env:dict[str,str]) -> list[stmt]:
        return [self.uniquify_stmt(s,env) for s in ss]
    
    def uniquify_stmt(self,s:stmt,env:dict[str,str]) -> stmt:
        uniquify_exp = self.uniquify_exp
        uniquify_stmts = self.uniquify_stmts
        match s:
            case Expr(exp):
                return Expr(uniquify_exp(exp,env))
            case Assign([Name(var)],exp):
                # extend the env, propagate this effect
                env[var] = generate_name(var) if var not in env else env[var]
                return Assign([Name(env[var])],uniquify_exp(exp,env))
            case If(pred,[*conseq],[*alter]):
                return If(uniquify_exp(pred,env),uniquify_stmts(conseq,env),uniquify_stmts(alter,env))
            case While(pred,[*ss],[]):
                return While(uniquify_exp(pred,env),uniquify_stmts(ss,env),[])
            case FunctionDef(var,params,[*ss],None,type,None):
                # TODO
                new_params = [(generate_name(v),t) for v,t in params]
                env[var] = var
                env = {**env,**{v:w for (v,_),(w,_) in zip(params,new_params)}}
                return FunctionDef(var,new_params,uniquify_stmts(ss,env),None,type,None)
            case Return(exp):
                return Return(uniquify_exp(exp,env))
            case AnnAssign(Name(var),type,exp,simple):
                # extend the env, propagate this effect
                env[var] = generate_name(var) if var not in env else env[var]
                return AnnAssign(Name(env[var]),type,uniquify_exp(exp,env),simple)
            case _:
                raise NotImplementedError("uniquify_stmt, bad argument: ", s)
    
    def uniquify_exp(self,e:expr,env:dict) -> expr:
        uniquify_exp = self.uniquify_exp
        uniquify_e = lambda e: uniquify_exp(e,env)
        uniquify_exps = lambda es,env: [uniquify_exp(e,env) for e in es]
        match e:
            case Constant():
                return e
            case Name(var):
                return Name(env[var])
            case BinOp(left,op,right):
                return BinOp(uniquify_e(left),op,uniquify_e(right))
            case UnaryOp(op,exp):
                return UnaryOp(op,uniquify_e(exp))
            case BoolOp(op,[*es]):
                return BoolOp(op,uniquify_exps(es,env))
            case Compare(left,[cmp],[*es]):
                return Compare(uniquify_e(left),
                               [cmp],
                               [uniquify_e(e) for e in es])
            case IfExp(pred,conseq,alter):
                return IfExp(uniquify_e(pred),
                             uniquify_e(conseq),
                             uniquify_e(alter))
            case Tuple([*es],Load()):
                return Tuple(uniquify_exps(es,env),Load())
            case Subscript(exp,Constant(i),Load()):
                return Subscript(uniquify_e(exp),Constant(i),Load())
            case Call(exp,[*es]):
                return Call(uniquify_e(exp),uniquify_exps(es,env))
            case Lambda([*vars],exp):
                new_vars  = [generate_name(v) for v in vars]
                # the copy is safe and effect need not propagate from the exp
                # because assignment is a statement cannot be a part of expression
                # in lambda body
                env = {**env,**{v:w for v,w in zip(vars,new_vars)}}
                return Lambda([*new_vars],uniquify_exp(exp,env))
            case _:
                raise NotImplementedError("uniquify_exp, bad argument: ", e)
    
    # reveal_functions pass
    def reveal_stmt(self, s: stmt) -> stmt:
        match s:
            case AnnAssign(var,type,exp,simple):
                return AnnAssign(var,type,self.reveal_exp(exp),simple)
            case _:
                return super().reveal_stmt(s)
    
    def reveal_exp(self, e: expr) -> expr:
        reveal_exp = self.reveal_exp
        match e:
            case Call(Name('arity'),[*es]):
                return Call(Name('arity'), [reveal_exp(e) for e in es])
            case Lambda([*vars],exp):
                return Lambda([*vars],reveal_exp(exp))
            case _:
                return super().reveal_exp(e)
    
    def init_global_functions(self, defns:list[FunctionDef]):
        super().init_global_functions(defns)
        self._global_functions['arity'] = 1
    
    # convert_assignments pass
    def convert_assignments(self, p: Module) -> Module:
        match p:
            case Module([*stmts]):
                return Module([self.convert_assign_fun(s) for s in stmts])
            case _:
                raise NotImplementedError('convert_assignments, bad argument: ', p)
    
    def convert_assign_fun(self, s:FunctionDef) -> FunctionDef:
        match s:
            case FunctionDef(var,params,[*ss],None,type,None):
                assigned_vars = set.intersection(
                    self.assigned_vars_stmts(ss),
                    self.free_in_lambda(ss)
                )
                    
                init = make_assigns([(Name(v.id),Tuple([Uninitialized(v.has_type)],Load())) for v in assigned_vars])
                # prevent clashed
                new_params = [((p,t) if Name(p) not in assigned_vars else (generate_name(p),t)) for (p,t) in params]
                assgn = make_assigns([(Subscript(Name(p),Constant(0),Store()),Name(q)) 
                                      for (p,_),(q,_) in zip(params,new_params) 
                                      if p != q])
                ss = self.convert_assign_stmts(ss,assigned_vars)
                return FunctionDef(var,new_params,init+assgn+ss,None,type,None)
            case _:
                raise NotImplementedError('convert_assign_fun, bad argument: ', s)
    
    def convert_assign_stmts(self, ss: list[stmt], assigned_vars:set[Name]) -> list[stmt]:
        from functools import reduce
        from operator import add
        return reduce(add,[self.convert_assign_stmt(s,assigned_vars) for s in ss])    
        
    def convert_assign_stmt(self, s: stmt, assigned_vars:set[Name]) -> list[stmt]:
        convert_stmts = lambda ss: self.convert_assign_stmts(ss,assigned_vars)
        convert_exp = lambda e: self.convert_assign_exp(e,assigned_vars)
        match s:
            case Assign([Name(var)],exp) if Name(var) in assigned_vars:
                assert(hasattr(s.targets[0],'has_type'))
                return [Assign([Subscript(Name(var), Constant(0), Store())],
                               convert_exp(exp))]
            case AnnAssign(Name(var),type,exp,simple)\
                if Name(var) in assigned_vars:
                return [AnnAssign(Subscript(Name(var), Constant(0), Store()),
                                 type,convert_exp(exp),simple)]
            case Expr(exp):
                return [Expr(convert_exp(exp))]
            case Assign([Name(var)],exp):
                return [Assign([Name(var)],convert_exp(exp))]
            case AnnAssign(Name(var),type,exp,simple):
                return [AnnAssign(Name(var),type,convert_exp(exp),simple)]
            case While(pred,[*ss],[]):
                return [While(convert_exp(pred),convert_stmts(ss),[])]
            case If(pred,[*conseq],[*alter]):
                return [If(convert_exp(pred),
                          convert_stmts(conseq),
                          convert_stmts(alter))]
            case FunctionDef(var,params,[*ss],None,type,None):
                raise NotImplementedError('todo')
            case Return(exp):
                return [Return(convert_exp(exp))]
            case _:
                raise NotImplementedError('convert_assign_stmt, bad arg: ', s)
    
    def convert_assign_exp(self, e: expr, assigned_vars:set[Name]) -> expr:
        convert_exp = lambda e: self.convert_assign_exp(e, assigned_vars)
        match e:
            case Name(var) if Name(var) in assigned_vars:
                return Subscript(Name(var), Constant(0), Load())
            case Constant()|Name():
                return e
            case BinOp(left,op,right):
                return BinOp(convert_exp(left),op,convert_exp(right))
            case BoolOp(op,[left,right]):
                return BoolOp(op,[convert_exp(left),convert_exp(right)])
            case Compare(left,[op],[right]):
                return Compare(convert_exp(left),[op],[convert_exp(right)])
            case UnaryOp(op,exp):
                return UnaryOp(op,convert_exp(exp))
            case IfExp(pred,conseq,alter):
                return IfExp(convert_exp(pred),
                             convert_exp(conseq),
                             convert_exp(alter))
            case Tuple([*es],Load()):
                return Tuple([convert_exp(e) for e in es],Load())
            case Subscript(exp,Constant(i),Load()):
                return Subscript(convert_exp(exp),Constant(i),Load())
            case Call(exp,[*es]):
                return Call(convert_exp(exp),[convert_exp(e) for e in es])
            case Lambda([*vars],exp):
                assert(hasattr(e,'has_type'))
                _e = Lambda([*vars],convert_exp(exp))
                _e.has_type  = e.has_type
                return _e
            case FunRef(str(f),arity):
                return FunRef(str(f),arity)
            case _:
                raise NotImplementedError('convert_assign_exp, bad arg: ', e)
    
    # auxiliary function
    def free_variables(self,e: expr) -> set[Name]:
        from functools import reduce
        free_variables = self.free_variables
        match e:
            case Constant():
                return set()
            case Name():
                return {e}
            case Lambda([*vars],exp):
                return self.free_variables(exp) - set([Name(v) for v in vars])
            case (BinOp(left,op,right)
                  |BoolOp(op,[left,right])
                  |Compare(left,[op],[right])):
                return free_variables(left)|free_variables(right)
            case UnaryOp(op,exp):
                return free_variables(exp)
            case IfExp(pred,conseq,alter):
                return (free_variables(pred)
                        |free_variables(conseq)
                        |free_variables(alter))
            case Tuple([*es],Load()):
                return reduce(set.union,[free_variables(e) for e  in es])
            case Subscript(exp,Constant(i),Load()):
                return free_variables(exp)
            case Call(exp,[*es]):
                return reduce(set.union,[free_variables(e) for e  in es],free_variables(exp))
            case _:
                raise NotImplementedError('free_variables, bad expr: ', e)
    
    # auxiliary function of convert_assignment
    def free_in_lambda(self, ss: list[stmt]) -> set[Name]:
        '''collects all the variables that are free in any of the lambda expressions'''
        from functools import reduce
        return reduce(set.union,map(self.free_variables,self.find_lambda_in_stmts(ss)),set())
    
    def find_lambda_in_stmts(self, ss: list[stmt]) -> list[Lambda]:
        from functools import reduce
        from operator import add
        return reduce(add,map(self.find_lambda_in_stmt,ss))
    
    def find_lambda_in_stmt(self, s:stmt) -> list[Lambda]:
        find_in_exp = self.find_lambda_in_exp
        find_in_stmts = self.find_lambda_in_stmts
        match s:
            case (Assign(_,exp)|Return(exp)
                  |Expr(exp)|AnnAssign(_,_,exp,_)):
                return self.find_lambda_in_exp(exp)
            case If(pred,[*conseq],[*alter]):
                return (find_in_exp(pred) 
                        + find_in_stmts(conseq) 
                        + find_in_stmts(alter))
            case While(pred,[*ss]):
                return find_in_exp(pred) + find_in_stmts(ss)
            case FunctionDef(var,params,[*ss],None,type,None):
                return find_in_stmts(ss)
            case _:
                raise NotImplementedError('assigned_vars_stmt, bad argument: ', s)
    
    def find_lambda_in_exp(self, e: expr) -> list[Lambda]:
        from functools import reduce
        from operator import add
        find = self.find_lambda_in_exp
        match e:
            case Lambda([*vars],exp):
                return [e]
            case Constant()|Name()|FunRef():
                return []
            case (BinOp(left,op,right)
                  | BoolOp(op,[left,right])
                  | Compare(left,[op],[right])):
                return find(left) + find(right)
            case UnaryOp(op,exp):
                return find(exp)
            case IfExp(pred,conseq,alter):
                return reduce(add,map(find,[pred,conseq,alter]))
            case Tuple([*es],Load()):
                return reduce(add,map(find,es))
            case Subscript(exp,Constant(i),Load()):
                return find(exp)
            case Call(exp,[*es]):
                return reduce(add,map(find,es),find(exp))
            case _:
                raise NotImplementedError('find_lambda_in_exp',e)
    
    # auxiliary function of convert_assignment
    def assigned_vars_stmts(self, ss: list[stmt]) -> set[Name]:
        from functools import reduce
        return reduce(set.union,map(self.assigned_vars_stmt,ss))
    
    def assigned_vars_stmt(self,s: stmt) -> set[Name]:
        assigned_vars_stmts = self.assigned_vars_stmts
        match s:
            case Assign([Name(var)],exp):
                return {s.targets[0]}
            case AnnAssign(Name(var),type,exp,simple):
                return {s.target}
            case If(pred,[*conseq],[*alter]):
                return assigned_vars_stmts(conseq)|assigned_vars_stmts(alter)
            case While(pred,[*ss]):
                return assigned_vars_stmts(ss)
            case Return()|Expr():
                return set()
            case _:
                raise NotImplementedError('assigned_vars_stmt, bad argument: ', s)
     
    # convert_to_closures
    def convert_to_closures(self, p: Module) -> Module:
        match p:
            case Module([*stmts]):
                defns = []
                p = self.convert_closure_stmts(stmts,defns)
                return Module(defns + p)
            case _:
                raise NotImplementedError('convert_assignments, bad argument: ', p)
    
    def convert_closure_stmts(self, ss: list[stmt], lambdas:list[FunctionDef]) -> list[stmt]:
        convert_stmt = lambda s: self.convert_closure_stmt(s,lambdas)
        return list(map(convert_stmt,ss))
    
    def convert_closure_stmt(self, s: stmt, lambdas:list[FunctionDef]) -> stmt:
        convert_exp = lambda e: self.convert_closure_exp(e,lambdas)
        convert_stmts = lambda ss: self.convert_closure_stmts(ss,lambdas)
        match s:
            case (Assign([Name(var)],exp)
                  |AnnAssign(Name(var),_,exp,_)):
                return Assign([Name(var)],convert_exp(exp))
            case Assign([Subscript(Name(var), Constant(i), Store())],exp):
                return Assign([Subscript(Name(var), Constant(i), Store())],convert_exp(exp))
            case Expr(exp):
                return Expr(convert_exp(exp))
            case If(pred,[*conseq],[*alter]):
                return If(convert_exp(pred),
                          convert_stmts(conseq),
                          convert_stmts(alter))
            case FunctionDef():
                return self.convert_closure_fun(s,lambdas)
            case Return(exp):
                return Return(convert_exp(exp))
            case While(pred,[*ss],[]):
                return While(convert_exp(pred),convert_stmts(ss),[])
            case _:
                raise NotImplementedError('convert_closure_stmt, bad stmt: ', s)
    
    def convert_closure_exp(self, e: expr, lambdas:list[FunctionDef]) -> expr:
        convert_exp = lambda e: self.convert_closure_exp(e,lambdas)
        match e:
            case Name(var):
                return Name(var)
            case Constant(i):
                return Constant(i)
            case BinOp(left,op,right):
                return BinOp(convert_exp(left),op,convert_exp(right))
            case BoolOp(op,[left,right]):
                return BoolOp(op,[convert_exp(left),convert_exp(right)])
            case Compare(left,[op],[right]):
                return Compare(convert_exp(left),[op],[convert_exp(right)])
            case UnaryOp(op,exp):
                return UnaryOp(op,convert_exp(exp))
            case IfExp(pred,conseq,alter):
                return IfExp(convert_exp(pred),convert_exp(conseq),convert_exp(alter))
            case Tuple([*es],Load()):
                return Tuple([convert_exp(e) for e in es],Load())
            case Subscript(exp,Constant(i),Load()):
                return Subscript(convert_exp(exp),Constant(i),Load())
            case Call(Name('print'|'input_int'|'len'|'arity'),[*es]):
                return Call(e.func,[convert_exp(e) for e in es])
            case Call(exp,[*es]):
                tmp = generate_name('clos.tmp')
                return Begin([Assign([Name(tmp)],convert_exp(exp))],
                             Call(Subscript(Name(tmp),Constant(0),Load()),
                                  [Name(tmp),*[convert_exp(e) for e in es]]))
            case FunRef(str(f),n):
                return Closure(n,[FunRef(str(f),n)])
            case Lambda():
                assert(hasattr(e,'has_type'))
                return self.convert_lambda(e,lambdas)
            case Uninitialized(ty):
                return Uninitialized(self.convert_type(ty))
            case _:
                raise NotImplementedError('convert_closure_exp, bad expr: ', e)

    def convert_closure_fun(self, s:FunctionDef, lambdas: list[FunctionDef]) -> FunctionDef:
        match s:
            case FunctionDef(var,params,[*ss],None,type,None):
                convert_type = self.convert_type
                return FunctionDef(
                    var,
                    [('clos',TupleType([Bottom()])),*((x,convert_type(t)) for x,t in params)],
                    self.convert_closure_stmts(ss,lambdas),
                    None,convert_type(type),None
                )
            case _:
                raise NotImplementedError('convert_closure_fun: ', s)
    
    def convert_lambda(self, e:Lambda, lambdas:list[FunctionDef]) -> Closure:
        convert_type = self.convert_type
        convert_exp = lambda e: self.convert_closure_exp(e,lambdas)
        match e:
            case Lambda([*vars],exp):
                exp = convert_exp(exp)
                free_vars = [*self.free_variables(e)]
                closure_T = convert_type(TupleType([Bottom(),*(v.has_type for v in free_vars)]))
                arity = len(vars)
                
                t:TupleType = convert_type(e.has_type)
                function_t:FunctionType = t.types[0]
                type = function_t.ret_type
                params = [('clos',closure_T),
                          *[(x,convert_type(t)) 
                            for x,t in zip(vars,function_t.param_types[1:])]]
                body = make_assigns([
                    (v,Subscript(Name('clos'),Constant(i),Load()))
                    for i,v in enumerate(free_vars,1)
                    ]) + [Return(exp)]
                
                name = generate_name('lambda')
                lambdas.append(FunctionDef(name,params,body,None,type,None))
                return Closure(arity,[FunRef(name,arity),*free_vars])
            case _:
                raise NotImplementedError()
    
    def convert_type(self, t:Type) -> Type:
        convert_type = self.convert_type
        match t:
            case FunctionType([*ts],rt):
                return TupleType([
                    FunctionType([TupleType([]),*[convert_type(t) for t in ts]],convert_type(rt))
                ])
            case TupleType([*ts]):
                return TupleType([convert_type(t) for t in ts])
            case IntType()|BoolType()|VoidType()|Bottom():
                return t
            case _:
                raise NotImplementedError(t)
    # limit_functions pass
    def limit_exp(self, e: expr, home: dict[str]) -> expr:
        limit_exp = lambda e: self.limit_exp(e,home)
        match e:
            case Begin([*ss],exp):
                return Begin(self.limit_stmts(ss,home),
                             limit_exp(exp))
            case Uninitialized():
                return e
            case Closure(arity,[f,*es]):
                return Closure(arity,[limit_exp(e) for e in [f,*es]])
            case _:
                return super().limit_exp(e, home)
    
    # expose_allocation pass
    def expose_allocation(self,p):
        p = super().expose_allocation(p)
        # print(p)
        return p
           
    def expose_exp(self, e):
        expose_exp = self.expose_exp
        expose_stmts = self.expose_stmts
        match e:
            case Uninitialized(ty):
                return e
            case Closure(arity,[FunRef(str(name),int(artiy)),*_es]):
                es = e.args
                init = [(Name(generate_name('init')),expose_exp(e)) for e in es]
                bytes_v = 8*(len(es)+1)
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
                tup_var = Name(generate_name('clos'))
                # BUG: UNCHECKED CAST
                ty = TupleType([e.has_type for e in es])
                alloc = [(tup_var,AllocateClosure(len(es),ty,arity)),
                        *[(Subscript(tup_var,Constant(i),Store()),
                           UncheckedCast(v,e.has_type)) for i,((v,_),e) in enumerate(zip(init,es))]
                        ]
                init = make_assigns(init)
                alloc = make_assigns(alloc)
                return Begin(init + collect + alloc, tup_var)
            case Begin([*ss],exp):
                return Begin(expose_stmts(ss),expose_exp(exp))
            case _:
                return super().expose_exp(e)
    
    # remove_complex_operands
    def rco_exp(self, e: expr, need_atomic: bool) -> tuple[expr, Temporaries]:
        atomize = self.atomize
        match e:
            case UncheckedCast(exp,ty):
                exp,bs = self.rco_exp(exp,need_atomic)
                return atomize(UncheckedCast(exp,ty),bs,need_atomic)
            case Uninitialized(ty):
                return atomize(e,[],need_atomic)
            case AllocateClosure(length,ty,arity):
                return atomize(e,[],need_atomic)
            case _:
                return super().rco_exp(e, need_atomic)
    
    # select instruction
    def closure_tag(self,size:int,ts:TupleType,arity:int) -> int:
        tag = super().attach_tag(size,ts)
        arity = arity << 58
        return arity | tag
                
    def select_stmt(self, s: stmt, f='') -> List[instr]:
        select_arg = self.select_arg
        match s:
            case Assign(var,UncheckedCast(exp,ty)):
                return self.select_stmt(Assign(var,exp))
            case Assign([name],Call(Name('arity'),[clos])):
                # (2**6-1) & (*tup >> 58) 
                var = select_arg(name)
                tup_exp = select_arg(clos)
                return [
                    Instr('movq',[tup_exp,Reg('r11')]),
                    Instr('movq',[Deref('r11',0),var]),
                    Instr('sarq',[Immediate(58),var]),
                    Instr('andq',[Immediate(2**6-1),var]),
                ]
            case Assign([name],AllocateClosure(length,ty,arity)):
                var = self.select_arg(name)
                return [
                    Instr('movq',[Global('free_ptr'),Reg('r11')]),
                    Instr('addq',[Immediate(8*(length+1)),Global('free_ptr')]),
                    Instr('movq',[Immediate(self.closure_tag(length,ty,arity)), Deref('r11',0)]),
                    Instr('movq',[Reg('r11'),var]),
                ]
            case Assign([name],Uninitialized(ty)):
                return []
            case _:
                return super().select_stmt(s, f)