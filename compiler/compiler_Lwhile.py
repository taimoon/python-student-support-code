from ast import *
from x86_ast import *
from utils import (stmt, make_begin, Goto)

from compiler.compiler_Lif import Compiler as Compiler_Lif
class Compiler(Compiler_Lif):
    def expand_stmt(self,s:stmt) -> stmt:
        expand_exp = self.expand_exp
        expand_stmt = self.expand_stmt
        match s:
            case While(exp,[*ss],[]):
                return While(expand_exp(exp),[expand_stmt(s) for s in ss],[])
            case _:
                return super().expand_stmt(s)
            
    def rco_stmt(self, s: stmt) -> list[stmt]:
        match s:
            case While(e,[*ss],[]):
                e,bs = self.rco_exp(e,True)
                e = make_begin(bs,e)
                ss = self.rco_stmts(ss)
                return [While(e,ss,[])]
            case _:
                return super().rco_stmt(s)
    
    def explicate_stmt(self, s, cont, basic_blocks) -> list[stmt]:
        match s:
            case While(e,[*ss],[]):
                # workaround
                # create placeholder first
                loop = self.create_block([],basic_blocks)
                match loop:
                    case [Goto(label)]:
                        thn = self.explicate_stmts(ss,loop,basic_blocks)
                        els = cont
                        basic_blocks[label] = self.explicate_pred(e,thn,els,basic_blocks)
                        return loop
                    case _:
                        raise NotImplementedError()
            case _:
                return super().explicate_stmt(s, cont, basic_blocks)