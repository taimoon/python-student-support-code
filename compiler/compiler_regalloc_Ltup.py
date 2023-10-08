from ast import *
from ast import Set
from graph import UndirectedAdjList
from x86_ast import *
from typing import (List, Set,Tuple,Dict)
from compiler.compiler_Ltup import Compiler as Compiler_Ltup
from compiler.compiler_regalloc_Lwhile import Compiler as Compiler_Regalloc
from utils import TupleType,CProgram
from x86_ast import Reg, instr, location


class Compiler(Compiler_Ltup,Compiler_Regalloc):
    ### Uncover live
    def write_vars(self, i: instr) -> set[location]:
        match i:
            case Instr('sarq',[Immediate(),w]):
                return self.filter_immediate({w})
            case _:
                return super().write_vars(i)
    def read_vars(self, i: instr) -> set[location]:
        match i:
            case Instr('sarq',[Immediate(),w]):
                return self.filter_immediate({w})
            case _:
                return super().read_vars(i)
    
    def add_interference(self, i: instr, live_after: dict[instr, set[location]], adj: UndirectedAdjList) -> None:
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
    
    ### assign_homes
    def assign_homes(self, p: X86Program) -> X86Program:
        match p:
            case X86Program({**body}):
                self.var_types:dict[str] = p.var_types
                live_after = self.uncover_live(p)
                graph = self.build_interference(p,live_after)
                acm_used_callee = set()
                spilled_hist = []
                tuples_hist = []
                for lbl,ss in body.items():
                    used_callee = set()
                    body[lbl],used_callee,spilled,tuples = self.allocate_instrs(ss,graph)
                    acm_used_callee = set.union(acm_used_callee,used_callee)
                    spilled_hist += [spilled]
                    tuples_hist += [tuples]
                
                assert(all(l==r for l,r in zip(spilled_hist[:-1],spilled_hist[1:])))
                assert(all(l==r for l,r in zip(tuples_hist[:-1],tuples_hist[1:])))
                
                p = X86Program(body)
                p.spilled = spilled
                p.used_callee = acm_used_callee
                p.tuples = tuples
                return p
            case _:
                raise NotImplementedError('assign_homes, unexpected argument ', p)

    # TODO: WORKAROUND
    def allocate_instrs(self, instrs: list[instr], graph: UndirectedAdjList) -> tuple[list[instr], set[Reg], set[location], set[location]]:
        '''returns: allocated instrustions,used_callee,spilled,tuples'''
        colors,spilled = self.color_graph(graph=graph,
                                          variables={v for v in graph.vertices() 
                                                     if isinstance(v,Variable)})
        
        reg_n = max(self.int2reg)
        f = lambda n : self.int2reg.get(n,Deref('rbp',-8*(n-reg_n+1)))
        assign_map = {}
        reg_n = max(self.int2reg.keys())
        
        for loc,n in colors.items():
            if isinstance(loc,Global|Deref):
                assign_map[loc] = loc
            elif n in self.int2reg:
                assign_map[loc] = self.int2reg[n]
            elif isinstance(loc,Variable):
                reg = 'r15' if isinstance(self.get_var_type(loc.id),TupleType) else 'rbp'
                assign_map[loc] = Deref(reg,-8*(n-reg_n+1))
            else:
                raise NotImplementedError('allocate_registers ',loc,n)
            
        
        tuples = set(v for v in graph.vertices() 
                       if isinstance(v,Variable) 
                       and isinstance(self.get_var_type(v.id),TupleType))
        alloc_reg = set(assign_map.values())
        p = [self.allocate_register(i,assign_map) for i in instrs]
        used_callee = set.intersection(alloc_reg,self.callee_save)
        spilled = set(spilled)
        
        return p,used_callee,spilled,tuples
    
    
    def prelude_and_conclusion(self, p: X86Program) -> X86Program:
        align = lambda n : n+(16-n%16)
        C = len(p.used_callee)
        S = len(p.spilled)
        A = align(8*S + 8*C) - 8*C
        
        A = align(A) if A%16 != 0 else A
        root_stack_sz = len(p.tuples)*8
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
            case X86Program({**body}):
                body[label_name('main')] = prelude + prelude_init_gc + jmp
                body[label_name('conclusion')] = conclusion_gc + conclusion
                return X86Program(body)
            case _:
                raise NotImplementedError("prelude_and_conclusion unexpected",p)