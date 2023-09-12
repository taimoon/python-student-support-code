from ast import *
from x86_ast import *
from compiler.compiler import Temporaries
from utils import (CProgram, stmt)
from typing import (List, Set,Tuple,Dict)

from compiler.compiler_Lif import Compiler as Compiler_Lif

class Compiler(Compiler_Lif):
    def expose_allocation(self,p: Module) -> Module:
        return Module(self.expose_stmts(p.body))
    
    def expose_stmts(self, ss: list[stmt]) -> list[stmt]:
        return [self.expose_stmt(s) for s in ss]
    
    def expose_stmt(self, s: stmt) -> list[stmt]:
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
            case BinOp(_,[e1,e2]):
                return e
            case Tuple([*es],Load()):
                raise NotImplementedError()
            case Subscript(tup_exp,idnex,Load()):
                return e
            case _:
                raise NotImplementedError('expose_exp, unexpected ', e)
            