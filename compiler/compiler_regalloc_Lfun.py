from ast import *
from graph import UndirectedAdjList
from x86_ast import *
from utils import (IntType,TupleType)
from compiler.compiler_Lfun import Compiler as compiler_Lfun
from compiler.compiler_regalloc_Ltup import Compiler as Compiler_Regalloc

class Compiler(compiler_Lfun,Compiler_Regalloc):
    ### Uncover live
    def read_vars(self, i: instr) -> set[location]:
        filter_immediate = self.filter_immediate
        match i:
            case Callq(f,arity):
                assert(not isinstance(f,Variable))
                return set(Compiler_Regalloc.arg_pass_ord[:arity])
            case IndirectCallq(f,arity)|TailJump(f,arity):
                f = set([f])
                return set(Compiler_Regalloc.arg_pass_ord[:arity]).union(f)
            case Instr('leaq',[r,w]):
                return filter_immediate({r})
            case _:
                return super().read_vars(i)
    
    def write_vars(self, i: instr) -> set[location]:
        filter_immediate = self.filter_immediate
        match i:
            case Callq()|IndirectCallq()|TailJump():
                return set(Compiler_Regalloc.caller_save)
            case Instr('leaq',[r,w]):
                return filter_immediate({w})
            case _:
                return super().write_vars(i)
    
    def is_endblock(self, label: str) -> bool:
        return label.endswith('_conclusion')
    
    ### build_interference
    def add_interference(self, i: instr, live_after: dict[instr, set[location]], adj: UndirectedAdjList) -> None:
        '''
        https://iu.mediaspace.kaltura.com/media/Compiler+Course%2C+October+15%2C+2020/1_hy383s9a
        Note: 53:30
        https://github.com/xc42/nanopass-compiler/blob/master/compiler.rkt
        '''
        match i:
            case Callq(f,arity)|TailJump(f,arity)|IndirectCallq(f,arity):
                if f == 'collect' or isinstance(i,IndirectCallq):
                    for loc in live_after[i]:
                        if (isinstance(loc,Variable) 
                            and isinstance(self.get_var_type(loc.id),TupleType)):
                            for d in self.callee_save:
                                adj.add_edge(d,loc)
                
                for v in live_after[i]:
                    adj.add_vertex(v)
                    for d in self.caller_save:
                        if v != d:
                            adj.add_edge(d,v)
            case _:
                return super().add_interference(i, live_after, adj)
    
    ### allocate_register & assign_home
    def assign_fun(self, defn: FunctionDef) -> FunctionDef:
        match defn:
            case FunctionDef(var,[],blocks,None,IntType(),None):
                p = X86Program(blocks)
                p.var_types = defn.var_types
                p = Compiler_Regalloc.assign_homes(self,p)
                defn = FunctionDef(var,[],p.body,None,IntType(),None)
                defn.spilled, defn.tuples, defn.used_callee = p.spilled, p.tuples, p.used_callee
                return defn
            case _:
                raise NotImplementedError()
    
    def allocate_register(self, i: instr, colors: dict[location, int]) -> instr:
        match i:
            case Callq():
                return i
            case IndirectCallq(f,arity):
                return IndirectCallq(colors[f],arity)
            case TailJump(f,arity):
                return TailJump(colors[f],arity)
            case _:
                return super().allocate_register(i, colors)
    
    # patch_instructions
    def patch_instructions(self, p: X86ProgramDefs) -> X86ProgramDefs:
        return super().patch_instructions(p)