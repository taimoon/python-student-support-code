import compiler.compiler as compiler
from graph import UndirectedAdjList
from ast import *
from x86_ast import *
from typing import Callable

'''
The difference from previous Compiler is made in assign_home and prelude_and_generate
The new pass for assign_home
liveness_analysis,uncover_live -> build_interference_graph -> coloring -> allocate_register -> assign_home (end)

'''

class Compiler(compiler.Compiler):
    # freely used by called function (the callee)
    caller_save = {Reg("rax"),Reg("rcx"),Reg("rdx"),
                   Reg("rsi"),Reg("rdi"),Reg("r8"),
                   Reg("r9"),Reg("r10"),Reg("r11")}
    
    # these values are preserved when calling the function; called function must restore these values
    callee_save = {Reg("rsp"),Reg("rbp"),Reg("rbx"),
                   Reg("r12"),Reg("r13"),Reg("r14"),Reg("r15")}
    
    caller_save = frozenset(caller_save)
    callee_save = frozenset(callee_save)
    
    int2reg = ['rcx', 'rdx', 'rsi', 'rdi', 'r8', 'r9', 'r10', 'rbx', 'r12', 'r13', 'r14']
    int2reg = [Reg(id) for id in int2reg]
    int2reg = {i:r for i,r in enumerate(int2reg)}
    
    bytereg = ['al', 'bl', 'cl', 'dl', 'ah', 'bh', 'ch', 'dh']
    reg_not_used = ['rax','rsp','rbp','r11','r15']
    
    reg_not_used = [Reg(id) for id in reg_not_used] + [ByteReg(r) for r in bytereg]
    reg_not_used = {-i:r for i,r in enumerate(reg_not_used,1)}
    
    int2reg = {**int2reg,**reg_not_used}
    reg2int = {r:i for i,r in int2reg.items()}
    
    arg_pass_ord = [Reg("rdi"),Reg("rsi"),Reg("rdx"),Reg("rcx"),Reg("r8"),Reg("r9")]
    
    ###########################################################################
    # Uncover Live
    ###########################################################################
    def uncover_live(self, p: X86Program) -> dict[instr, set[location]]:
        '''
        Return a dictionary that maps each instruction to its live-after set.
        '''
        return self.uncover_instrs(p.body,self.live_before)
    
    @staticmethod
    def filter_immediate(xs:set) -> set[location]:
        return {x for x in xs if isinstance(x,location)}
    
    def read_vars(self, i: instr) -> set[location]:
        # YOUR CODE HERE
        filter_immediate = self.filter_immediate
        match i:
            case Instr('movq',[r,w]):
                return filter_immediate({r})
            case Instr('subq'|'addq'|'xorq'|'andq',[r,w]):
                return filter_immediate({r,w})
            case Instr('negq'|'pushq'|'popq',[a]):
                return filter_immediate({a})
            case Callq(_,arity):
                return set(self.arg_pass_ord[:arity])
            case Instr('retq',_):
                return set()
            case _:
                raise NotImplementedError('read_vars',i)

    
    def write_vars(self, i: instr) -> set[location]:
        filter_immediate = self.filter_immediate
        match i:
            case Instr('movq'|'subq'|'addq'|'xorq'|'andq',[r,w]):
                return filter_immediate({w})
            case Instr('negq'|'pushq'|'popq',[a]):
                return filter_immediate({a})
            case Callq():
                return set(self.caller_save)
            case Instr('retq',_):
                return {Reg('rax')}
            case _:
                raise NotImplementedError('write_vars, unexpected: ',i)
    
    def live_before(self,i:instr,after:set[location]) -> set[location]:
        return set.union(
            set.difference(after,self.write_vars(i)),
            self.read_vars(i)
        )
    
    def uncover_instrs(self, instrs: list[instr], live_before:Callable[[instr,set[location]],set[location]]) -> dict[instr, set[location]]:
        '''
        Return a dictionary that maps each instruction to its live-after set.
        '''
        live_after = dict()
        after_set = set()
        
        for i in reversed(instrs):
            if i in live_after: raise Exception('uncover live, repeated instr',i)
            live_after[i] = after_set
            after_set = live_before(i,after_set)
        
        return live_after

    ############################################################################
    # Build Interference
    ############################################################################
    
    def build_interference(self, 
                           p: X86Program,
                           live_after: dict[instr, set[location]]) -> UndirectedAdjList:
        return self.build_interference_instrs(p.body,live_after)
    
    def build_interference_instrs(self, instrs:list[instr],
                           live_after: dict[instr, set[location]]) -> UndirectedAdjList:
        adj = UndirectedAdjList()
        for i in instrs:
            self.add_interference(i,live_after,adj)
        return adj
    
    def add_interference(self, i: instr,
                         live_after: dict[instr, set[location]], 
                         adj: UndirectedAdjList) -> None:
        match i:
            case Instr('movq'|'movzbq'|'leaq',[s,d]):
                adj.add_vertex(d)
                if not isinstance(s,Immediate): adj.add_vertex(s)
                for v in live_after[i]:
                    if v!=s and v!=d:
                        adj.add_edge(d,v)
            case Instr('subq'|'addq'|'xorq'|'andq',[s,d]):
                adj.add_vertex(d)
                if not isinstance(s,Immediate): adj.add_vertex(s)
                for v in live_after[i]:
                    if v!=d:
                        adj.add_edge(d,v)
            case Instr('negq'|'pushq'|'popq',[d]):
                adj.add_vertex(d)
                for v in live_after[i]:
                    if v != d:
                        adj.add_edge(d,v)
            case Callq():
                for d in self.caller_save:
                    adj.add_vertex(d)
                    for v in live_after[i]:
                        if v != d:
                            adj.add_edge(d,v)
            case Instr('retq',_):
                d = Reg('rax')
                adj.add_vertex(d)
                for v in live_after[i]:
                    if v != d:
                        adj.add_edge(d,v)
            case _:
                raise NotImplementedError('add_interference, unexpected',i)
    ############################################################################
    # Allocate Registers
    ############################################################################
    
    def saturation(self,G:UndirectedAdjList,colors:dict[location,int],u) -> set[int]:
        'the set of numbers that are no longer available'
        # workaround
        if hasattr(u,'key'):
            u = u.key
        if u not in G.vertices(): raise Exception(u)
        return set(colors[v] for v in G.adjacent(u))
    
    def color_graph(self, 
                    graph: UndirectedAdjList,
                    variables: set[location]) \
        -> tuple[dict[location, int], set[location]]:
        '''
        Returns the coloring and the set of spilled variables
        This function should return a mapping of variables to their colors (represented as natural numbers).
        '''

        reg2int = lambda v: self.reg2int.get(v,None)
        colors = {v:reg2int(v) for v in graph.vertices()}
        saturation = lambda u : self.saturation(graph,colors,u)
        
        def less(u,v):
            return len(saturation(u)) <= len(saturation(v))
        
        dom = [v for v in graph.vertices() if colors[v] is None or colors[v] >= 0]
        dom = set(range(len(dom)))
        
        from priority_queue import PriorityQueue
        W = PriorityQueue(less)
        for v in graph.vertices():
            W.push(v)
        
        while not W.empty():
            u = W.pop()
            if colors[u] is None:
                # pick the smallest possible number
                colors[u] = min(dom.difference(saturation(u)))
            
            for v in graph.adjacent(u):
                W.increase_key(v)
            
        reg_n = max(self.int2reg.keys())
        spilled = set(v for v in variables if colors[v] >= reg_n)
        
        return colors,spilled
    
    def allocate_register(self,i: instr, colors: dict[location, int]) -> instr:
        match i:
            case Instr(op,args):
                return Instr(op,[(a if isinstance(a,Immediate) else colors[a]) for a in args])
            case Callq():
                return i
            case _:
                raise NotImplementedError('allocate_register',i)
    
    def allocate_instrs(self, instrs: list[instr], graph: UndirectedAdjList) -> tuple[list[instr], set[Reg], set[location]]:
        '''returns: allocated instrustions,used_callee,spilled'''
        colors,spilled = self.color_graph(graph=graph,
                                          variables={v for v in graph.vertices() 
                                                     if isinstance(v,Variable)})
        
        reg_n = max(self.int2reg)
        f = lambda n : self.int2reg.get(n,Deref('rbp',-8*(n-reg_n+1)))
        
        colors = {loc:f(n) for loc,n in colors.items()}
        alloc_reg = set(colors.values())
        p = [self.allocate_register(i,colors) for i in instrs]
        used_callee = set.intersection(alloc_reg,self.callee_save)
        spilled = set(spilled)
        
        return p,used_callee,spilled
    
    def allocate_registers(self, p: X86Program,
                           graph: UndirectedAdjList,
                           used_callee: set[Reg]) -> X86Program:
        p,used_callee,spilled = self.allocate_instrs(p.body,graph)
        p,p.used_callee,p.spilled = X86Program(p),used_callee,spilled
        return p
        
        

    ############################################################################
    # Assign Homes
    ############################################################################

    def assign_homes(self, pseudo_x86: X86Program) -> X86Program:
        live_after = self.uncover_live(pseudo_x86)
        graph = self.build_interference(pseudo_x86,live_after)
        self.used_callee = set()
        return self.allocate_registers(pseudo_x86,graph,used_callee=self.used_callee)
        

    ###########################################################################
    # Patch Instructions
    ###########################################################################
    def patch_instructions(self,p: X86Program) -> X86Program:
        # what da hail? like this also can??
        # propagate the info
        p,p.used_callee,p.spilled = super().patch_instructions(p),p.used_callee,p.spilled
        return p
    
    def patch_instr(self, i: instr) -> list[instr]:
        match i:
            case Instr('movq',[r,w]) if r == w:
                return []
            case _:
                return super().patch_instr(i)

    ###########################################################################
    # Prelude & Conclusion
    ###########################################################################
    def prelude_and_conclusion(self, p: X86Program) -> X86Program:
        # TODO
        align = lambda n : n+(16-n%16)
        C = len(p.used_callee)
        S = len(p.spilled)
        A = align(8*S + 8*C) - 8*C
        A = align(A) if A%16 != 0 else A
        
        prelude = [
            Instr('pushq',[Reg('rbp')]),
            Instr('movq',[Reg('rsp'),Reg('rbp')]),
            Instr('subq',[Immediate(A),Reg('rsp')]),
        ]
        conclusion = [
            Instr('addq',[Immediate(A),Reg('rsp')]),
            Instr('popq',[Reg('rbp')]),
            Instr('retq',[]),
        ]
        
        match p:
            case X86Program(body):
                return X86Program(prelude + body + conclusion)
            case _:
                raise NotImplementedError("prelude_and_conclusion")
