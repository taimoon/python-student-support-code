from ast import *
from ast import Set
from graph import UndirectedAdjList
from x86_ast import *
from typing import (List, Set,Tuple,Dict)
from compiler.compiler_Ltup import Compiler as Compiler_Ltup
from compiler.compiler_regalloc_Lwhile import Compiler as Compiler_Regalloc
from utils import TupleType,CProgram
from x86_ast import instr, location


class Compiler(Compiler_Ltup,Compiler_Regalloc):
    ###
    def write_vars(self, i: instr) -> Set[location]:
        match i:
            case Instr('sarq',[Immediate(),w]):
                return self.filter_immediate({w})
            case _:
                return super().write_vars(i)
    def read_vars(self, i: instr) -> Set[location]:
        match i:
            case Instr('sarq',[Immediate(),w]):
                return set()
            case _:
                return super().read_vars(i)
    ###
    def add_interference(self, i: Instr, live_after: Dict[instr, Set[location]], adj: UndirectedAdjList) -> None:
        match i:
            case Instr('sarq',[Immediate(),v]):
                pass
            case Callq('collect'):
                for loc in live_after[i]:
                    if isinstance(loc,Variable) and isinstance(self.get_var_type(loc.id),TupleType):
                        for d in self.callee_save:
                            adj.add_edge(d,loc)
                
                for d in self.caller_save:
                    adj.add_vertex(d)
                    for v in live_after[i]:
                        if v != d:
                            adj.add_edge(d,v)
            case _:
                return super().add_interference(i, live_after, adj)
    
    def assign_homes(self, p: CProgram) -> CProgram:
        self.var_types:dict[str] = p.var_types
        self.tuples = set()
        return Compiler_Regalloc.assign_homes(self,p)

    # TODO: WORKAROUND
    def allocate_registers(self, p: list,
                           graph: UndirectedAdjList,
                           used_callee: Set[Reg]) -> list:
        colors,spilled = self.color_graph(graph=graph,
                                          variables=graph.vertices())
        assign_map = {}
        reg_n = max(self.int2reg.keys())
        
        for loc,n in colors.items():
            if isinstance(loc,Global|Deref):
                assign_map[loc] = loc
            elif n in self.int2reg:
                assign_map[loc] = self.int2reg[n]
            elif isinstance(loc,Variable) and isinstance(self.get_var_type(loc.id),TupleType):
                assign_map[loc] = Deref('r15',-8*(n-reg_n+1))
            elif isinstance(loc,Variable):
                assign_map[loc] = Deref('rbp',-8*(n-reg_n+1))
            else:
                raise NotImplementedError('allocate_registers ',loc,n)
            
        
        var_tups = set(v for v in graph.vertices() 
                       if isinstance(v,Variable) 
                       and isinstance(self.get_var_type(v.id),TupleType))
        self.tuples = set.union(self.tuples,var_tups)
        alloc_reg = set(assign_map.values())
        self.spilled = frozenset(spilled)
        used_callee.update(set.intersection(alloc_reg,self.callee_save))
        return [self.allocate_register(i,assign_map) for i in p]
    
    def prelude_and_conclusion(self, p: CProgram) -> X86Program:
        align = lambda n : n+(16-n%16)
        C = len(self.used_callee)
        S = len(self.spilled)
        A = align(8*S + 8*C) - 8*C
        
        A = align(A) if A%16 != 0 else A
        root_stack_sz = len(self.tuples)*8
        prelude = [
            Instr('pushq',[Reg('rbp')]),
            Instr('movq',[Reg('rsp'),Reg('rbp')]),
            Instr('subq',[Immediate(A),Reg('rsp')]),
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
            Instr('addq',[Immediate(A),Reg('rsp')]),
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