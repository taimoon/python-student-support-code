from ast import *
from ast import Dict, Set
from compiler.compiler import Temporaries
from utils import (
    generate_name, Begin, CProgram,
    make_assigns, make_begin, Goto, stmt,
    )
from graph import UndirectedAdjList, topological_sort, transpose, DirectedAdjList
from x86_ast import *
from typing import (List, Set,Tuple,Dict)

from x86_ast import Instr, Reg, X86Program, instr, location

Blocks = Dict[str,List[stmt]]
from compiler.compiler_Lif import Compiler as Compiler_Lif
from compiler.compiler_register_allocator import Compiler as Compiler_Reg_Allocator

class Compiler(Compiler_Lif,Compiler_Reg_Allocator):
    def control_flow_graph_from(self, p: CProgram) -> DirectedAdjList:
        cfg=DirectedAdjList()
        for lbl,ss in p.body.items():
            cfg.add_vertex(lbl)
            for s in ss:
                match s:
                    case Jump(dest)|JumpIf(_,dest):
                        cfg.add_edge(lbl,dest)
        return cfg
    
    def uncover_live(self, p: CProgram) -> Dict[str,Dict[instr, Set[location]]]:
        cfg = self.control_flow_graph_from(p)
        cfg = transpose(cfg)
        blocks = topological_sort(cfg)
        
        live_b4_block = {blocks[0]:set()}        
        res = {lbl:dict() for lbl in blocks}
        
        def before(cur_label,i):
            match i:
                case Jump(label=lbl)|JumpIf(label=lbl):
                    return live_b4_block[lbl]
                case _:
                    return set.union(set.difference(res[cur_label][i],self.write_vars(i)),
                                     self.read_vars(i),)
        
        after_set = set()
        for lbl,ss in ((lbl,p.body[lbl]) for lbl in blocks if lbl != 'conclusion'):
            for i in reversed(ss):
                match i:
                    case Jump(label=_lbl):
                        res[lbl][i] = after_set
                        after_set = before(_lbl,i)
                    case JumpIf(label=_lbl):
                        res[lbl][i] = after_set
                        after_set = set.union(after_set,before(_lbl,i))
                    case _:
                        res[lbl][i] = after_set
                        after_set = before(lbl,i)
            live_b4_block[lbl] = after_set
        return res
    
    def read_vars(self, i: instr) -> Set[location]:
        match i:
            case Instr(set,[a]) if set[:3] == 'set':
                return {a}
            case Instr('cmpq',[x,y]):
                return {x,y}
            case Instr('movzbq',[r,w]):
                return {r}
            case Jump()|JumpIf():
                return {}
            case _:
                return super().read_vars(i)
    
    def write_vars(self, i: instr) -> Set[location]:
        match i:
            case Instr(set,[a]) if set[:3] == 'set':
                return {a}
            case Instr('cmpq',_):
                return {Reg('al')}
            case Instr('movzbq',[r,w]):
                return {w}
            case Jump()|JumpIf():
                return {}
            case _:
                return super().write_vars(i)
    
    def assign_homes(self, p: CProgram) -> CProgram:
        live_after = self.uncover_live(p)
        self.used_callee = set()
        body:dict = p.body
        
        for lbl,ss in body.items():
            used_callee = set()
            graph = self.build_interference(ss,live_after[lbl])
            body[lbl] = self.allocate_registers(ss,graph,used_callee=used_callee)
            self.used_callee = set.union(used_callee)
        
        return CProgram(body)
    
    def add_interference(self, i: Instr, live_after: Dict[instr, Set[location]], adj: UndirectedAdjList) -> None:
        match i:
            case Instr('cmpq',[left,right]):
                pass
            case Instr(set,[d]) if set[:3] == 'set':
                adj.add_vertex(d)
                for v in live_after[i]:
                    if v != d:
                        adj.add_edge(d,v)
            case Jump()|JumpIf():
                pass
            case _:
                return super().add_interference(i, live_after, adj)
    
    def allocate_registers(self, p: list[instr], graph: UndirectedAdjList, used_callee: Set[Reg]) -> list[instr]:
        return super().allocate_registers(X86Program(p), graph, used_callee).body
    
    def allocate_register(self, i: instr, colors: Dict[location, int]) -> instr:
        match i:
            case Jump()|JumpIf():
                return i
            case _:
                return super().allocate_register(i, colors)
    
    
    def prelude_and_conclusion(self, p: CProgram) -> X86Program:
        super().prelude_and_conclusion
        align = lambda n : n+(16-n%16)
        C = len(self.used_callee)
        S = len(self.spilled)
        A = align(8*S + 8*C) - 8*C
        
        A = align(A) if A%16 != 0 else A
        
        prelude = [
            Instr('pushq',[Reg('rbp')]),
            Instr('movq',[Reg('rsp'),Reg('rbp')]),
            Instr('subq',[Immediate(A),Reg('rsp')]),
            Jump('start'),
        ]
        conclusion = [
            Instr('addq',[Immediate(A),Reg('rsp')]),
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