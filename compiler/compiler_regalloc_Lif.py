from ast import *
from ast import Dict, Set
from utils import (CProgram,stmt)
from graph import UndirectedAdjList, topological_sort, transpose, DirectedAdjList
from x86_ast import *
from typing import (List, Set,Tuple,Dict)

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
    
    def live_before(self, i: instr, live_after: dict[instr, set], live_b4_block: dict[str,set]) -> set[instr]:
        live_before = lambda i: Compiler_Reg_Allocator.live_before(self,i,live_after)
        match i:
            case Jump(label=label):
                return live_b4_block[label]
            case JumpIf(label=label):
                return set.union(live_b4_block[label],live_before(i))
            case _:
                return live_before(i)
        
    def uncover_block(self,instrs,live_after,live_b4_block) -> dict[instr,set[location]]:
        live_before = lambda i: self.live_before(i,live_after,live_b4_block)
        after_set = set()
        for i in reversed(instrs):
            live_after[i] = after_set
            after_set = live_before(i)
        return live_after
        
    def uncover_live(self, p: CProgram) -> Dict[str,Dict[instr, Set[location]]]:
        cfg = self.control_flow_graph_from(p)
        cfg = transpose(cfg)
        blocks = topological_sort(cfg)
        
        live_b4_block = {blocks[0]:set()}
        live_after = {lbl:dict() for lbl in blocks}
        
        live_before = self.live_before
        
        labels = (lbl for lbl in blocks if lbl != 'conclusion')
        for lbl in labels:
            ss = p.body[lbl]
            live_after[lbl] = self.uncover_block(ss,live_after[lbl],live_b4_block)
            live_b4_block[lbl] = live_before(ss[0],live_after[lbl],live_b4_block)
        
        assert(live_before(p.body['start'][0],live_after['start'],live_b4_block) == set())
        return live_after
    
    def read_vars(self, i: instr) -> Set[location]:
        filter_immediate = self.filter_immediate
        match i:
            case Instr(set,[a]) if set[:3] == 'set':
                return filter_immediate({a})
            case Instr('cmpq',[x,y]):
                return filter_immediate([x,y])
            case Instr('movzbq',[r,w]):
                return filter_immediate({r})
            case Jump()|JumpIf():
                return {}
            case _:
                return super().read_vars(i)
    
    def write_vars(self, i: instr) -> Set[location]:
        filter_immediate = self.filter_immediate
        match i:
            case Instr(set,[a]) if set[:3] == 'set':
                return filter_immediate({a})
            case Instr('cmpq',_):
                return {ByteReg('al')}
            case Instr('movzbq',[r,w]):
                return filter_immediate({w})
            case Jump()|JumpIf():
                return {}
            case _:
                return super().write_vars(i)
    
    def assign_homes(self, p: CProgram) -> CProgram:
        live_after = self.uncover_live(p)
        self.used_callee = set()
        body:dict = p.body
        
        self.used_callee = set()
        graph = self.build_interference(p.body,live_after)
        for lbl,ss in body.items():
            used_callee = set()
            body[lbl] = self.allocate_registers(ss,graph,used_callee=used_callee)
            self.used_callee = set.union(self.used_callee,used_callee)
        return CProgram(body)
    
    def build_interference(self, instrs: dict[str, list[instr]], live_after: Dict[str,Dict[instr, Set[location]]]) -> UndirectedAdjList:
        adj = UndirectedAdjList()
        for lbl,ss in instrs.items():
            for i in ss:
                self.add_interference(i,live_after[lbl],adj)
        return adj
    
    def add_interference(self, i: Instr, live_after: Dict[instr, Set[location]], adj: UndirectedAdjList) -> None:
        match i:
            case Instr('cmpq',[s,d]):
                'The second argument of the cmpq instruction must not be an immediate value (such as an integer).'
                if not isinstance(d,Immediate): adj.add_vertex(d)
                if not isinstance(s,Immediate): adj.add_vertex(s)
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
    