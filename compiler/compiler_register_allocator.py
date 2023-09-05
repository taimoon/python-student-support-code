import compiler.compiler as compiler
from graph import UndirectedAdjList
from typing import List, Tuple, Set, Dict
from ast import *
from x86_ast import *
from typing import Set, Dict, Tuple

# Skeleton code for the chapter on Register Allocation

class Compiler(compiler.Compiler):
    caller_save = {Reg("rax"),Reg("rcx"),Reg("rdx"),
                   Reg("rsi"),Reg("rdi"),Reg("r8"),
                   Reg("r9"),Reg("r10"),Reg("r11")}
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

    def read_vars(self, i: instr) -> Set[location]:
        # YOUR CODE HERE
        match i:
            case Instr('movq'|'subq'|'addq',[r,w]):
                return {} if isinstance(r,Immediate) else {r}
            case Instr('negq'|'pushq'|'popq',[a]):
                return {a}
            case Callq(_,argc):
                return set(self.arg_pass_ord[:argc])
            case Instr('retq',_):
                return set()
            case _:
                raise NotImplementedError('read_vars',i)

    def write_vars(self, i: instr) -> Set[location]:
        match i:
            case Instr('movq'|'subq'|'addq',[r,w]):
                return {w}
            case Instr('negq'|'pushq'|'popq',[a]):
                return {a}
            case i if isinstance(i,Callq):
                return set(self.caller_save)
            case Instr('retq',_):
                return {Reg('rax')}
            case _:
                raise NotImplementedError('write_vars, unexpected: ',i)

    def uncover_live(self, p: X86Program) -> Dict[instr, Set[location]]:
        # YOUR CODE HERE
        res = dict()
        after_set = set()
        before = lambda i: set.union(
            set.difference(res[i],self.write_vars(i)),
            self.read_vars(i),
        )
        for i in reversed(p.body):
            if i in res:
                raise Exception('uncover live, repeated instr',i,p)
            res[i] = after_set
            
            after_set = before(i)
        return res

    ############################################################################
    # Build Interference
    ############################################################################
    
    def build_interference(self, instrs:list[instr],
                           live_after: Dict[Instr, Set[location]]) -> UndirectedAdjList:
        adj = UndirectedAdjList()
        for i in instrs:
            self.add_interference(i,live_after,adj)
        return adj

    def add_interference(self, i: Instr,
                           live_after: Dict[instr, Set[location]], adj: UndirectedAdjList) -> None:
        match i:
            case Instr('movq'|'movzbq',[s,d]):
                adj.add_vertex(d)
                if not isinstance(s,Immediate): adj.add_vertex(s)
                for v in live_after[i]:
                    if v!=s and v!=d:
                        adj.add_edge(d,v)
            case Instr('subq'|'addq',[s,d]):
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
            case Callq(): # case i if isinstance(i,Callq):
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
    
    def saturation(self,G,colors,u):
        'the set of numbers that are no longer available'
        if hasattr(u,'key'):
            u = u.key
        if u not in G.vertices(): raise Exception(u)
        return set(colors[v] for v in G.adjacent(u))
    
    def color_graph(self, 
                    graph: UndirectedAdjList,
                    variables: Set[location]) \
        -> Tuple[Dict[location, int], Set[location]]:
        '''
        Returns the coloring and the set of spilled variables
        '''

        reg2int = lambda v: self.reg2int.get(v,None)
        colors = {v:reg2int(v) for v in graph.vertices()}
        saturation = lambda u : self.saturation(graph,colors,u)
        
        def less(u,v):
            return len(saturation(u)) <= len(saturation(v))
        
        variables = frozenset(variables) # variables can change
        dom = set(range(len(variables)))
        
        from priority_queue import PriorityQueue
        W = PriorityQueue(less)
        for v in tuple(graph.vertices()):
            W.push(v)
        
        while not W.empty():
            u = W.pop()
            if colors[u] is None:
                colors[u] = min(dom.difference(saturation(u)))
            for v in graph.adjacent(u):
                W.increase_key(v)
            
        reg_n = len(self.int2reg)
        spilled = set(v for v in variables if colors[v] >= reg_n)
        
        return colors,spilled
    
    def allocate_register(self,i: instr, colors: Dict[location, int]) -> instr:
        match i:
            case Instr(op,args):
                return Instr(op,[a if isinstance(a,Immediate) else colors[a] for a in args])
            case Callq():
                return i
            case _:
                raise NotImplementedError('allocate_register',i)
    
    def allocate_registers(self, p: X86Program,
                           graph: UndirectedAdjList,
                           used_callee: Set[Reg]) -> X86Program:
        colors,spilled = self.color_graph(graph=graph,
                                          variables=graph.vertices())
        
        reg_n = max(self.int2reg)
        f = lambda n : self.int2reg.get(n,Deref('rbp',-8*(n-reg_n+1)))
        
        assign_map = {loc:f(n) for loc,n in colors.items()}
        
        alloc_reg = set(assign_map.values())
        self.spilled = frozenset(spilled)
        used_callee.update(set.intersection(alloc_reg,self.callee_save))
        return X86Program(
            [self.allocate_register(i,assign_map) for i in p.body]
        )
        
        

    ############################################################################
    # Assign Homes
    ############################################################################

    def assign_homes(self, pseudo_x86: X86Program) -> X86Program:
        live_after = self.uncover_live(pseudo_x86)
        graph = self.build_interference(pseudo_x86.body,live_after)
        
        self.used_callee = set()
        return self.allocate_registers(pseudo_x86,graph,used_callee=self.used_callee)
        

    ###########################################################################
    # Patch Instructions
    ###########################################################################

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
        align = lambda n : n+(16-n%16)
        C = len(self.used_callee)
        S = len(self.spilled)
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
