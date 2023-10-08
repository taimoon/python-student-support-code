from ast import *
from graph import UndirectedAdjList, topological_sort, transpose, DirectedAdjList
from x86_ast import *
from compiler.compiler_Lif import Compiler as Compiler_Lif
from compiler.compiler_register_allocator import Compiler as Compiler_Reg_Allocator
from x86_ast import X86Program

class Compiler(Compiler_Lif,Compiler_Reg_Allocator):
    def control_flow_graph_from(self, p: X86Program) -> DirectedAdjList:
        cfg=DirectedAdjList()
        for lbl,ss in p.body.items():
            cfg.add_vertex(lbl)
            for s in ss:
                match s:
                    case Jump(dest)|JumpIf(_,dest):
                        cfg.add_edge(lbl,dest)
        return cfg
    
    def live_before(self, i: instr, after: set[location], live_b4_block: dict[str,set]) -> set[instr]:
        live_before = lambda i: Compiler_Reg_Allocator.live_before(self,i,after)
        match i:
            case Jump(label=label):
                return live_b4_block[label]
            case JumpIf(label=label):
                return set.union(live_b4_block[label],live_before(i))
            case _:
                return live_before(i)
        
    def uncover_block(self,instrs,live_b4_block) -> dict[instr,set[location]]:
        live_before = lambda i,live_after:self.live_before(i,live_after,live_b4_block)
        return Compiler_Reg_Allocator.uncover_instrs(self,instrs,live_before)
        
    def uncover_live(self, p: X86Program) -> dict[str,dict[instr, set[location]]]:
        cfg = self.control_flow_graph_from(p)
        cfg = transpose(cfg)
        blocks = topological_sort(cfg)
        
        live_b4_block = {blocks[0]:set()}
        live_after = {lbl:None for lbl in blocks}
        
        live_before = self.live_before
        
        labels = (lbl for lbl in blocks if lbl != 'conclusion')
        for lbl in labels:
            ss = p.body[lbl]
            live_after[lbl] = self.uncover_block(ss,live_b4_block)
            s,*_ = ss
            live_b4_block[lbl] = live_before(s,live_after[lbl][s],live_b4_block)
        s,*_ = p.body['start']
        assert(live_before(s,live_after['start'][s],live_b4_block) == set())
        return live_after
    
    def read_vars(self, i: instr) -> set[location]:
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
    
    def write_vars(self, i: instr) -> set[location]:
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
    
    def build_interference(self, p: X86Program, live_after: dict[instr, set[location]]) -> UndirectedAdjList:
        return self.build_interference_instrs(p.body,live_after)
    
    def build_interference_instrs(self, instrs: dict[str, list[instr]], live_after: dict[str,dict[instr, set[location]]]) -> UndirectedAdjList:
        adj = UndirectedAdjList()
        for lbl,ss in instrs.items():
            for i in ss:
                self.add_interference(i,live_after[lbl],adj)
        return adj
    
    def add_interference(self, i: instr, live_after: dict[instr, set[location]], adj: UndirectedAdjList) -> None:
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
    
    ### assign_homes
    def assign_homes(self, p: X86Program) -> X86Program:
        match p:
            case X86Program({**body}):
                live_after = self.uncover_live(p)
                graph = self.build_interference(p,live_after)
                acm_used_callee = set()
                spilled_hist = []
                for lbl,ss in body.items():
                    used_callee = set()
                    body[lbl],used_callee,spilled = self.allocate_instrs(ss,graph)
                    acm_used_callee = set.union(acm_used_callee,used_callee)
                    spilled_hist += [spilled]
                
                assert(all(l==r for l,r in zip(spilled_hist[:-1],spilled_hist[1:])))
                
                p = X86Program(body)
                p.spilled = spilled
                p.used_callee = acm_used_callee
                return p
            case _:
                raise NotImplementedError('TODO')
    
    def allocate_register(self, i: instr, colors: dict[location, int]) -> instr:
        match i:
            case Jump()|JumpIf():
                return i
            case _:
                return super().allocate_register(i, colors)
    
    ### patch_instructions
    def patch_instructions(self, p: X86Program) -> X86Program:
        p,p.used_callee = super().patch_instructions(p),p.used_callee
        return p
    
    def prelude_and_conclusion(self, p: X86Program) -> X86Program:
        align = lambda n : n+(16-n%16)
        C = len(p.used_callee)
        S = len(p.spilled)
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
            case X86Program({**body}):
                body[label_name('main')] = prelude
                body[label_name('conclusion')] = conclusion
                return X86Program(body)
            case _:
                raise NotImplementedError("prelude_and_conclusion unexpected",p)
    