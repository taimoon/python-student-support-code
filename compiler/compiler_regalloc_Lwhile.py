# from ast import *
# from compiler.compiler import Temporaries
# from utils import (CProgram, stmt,)
# from graph import UndirectedAdjList, topological_sort, transpose, DirectedAdjList
# from x86_ast import *
# from typing import (List, Set,Tuple,Dict)

# from compiler.compiler_Lwhile import Compiler as Compiler_Lwhile
# from compiler.compiler_regalloc_Lif import Compiler as Compiler_Reg_Alloc
# from compiler.compiler_regalloc_Lif import Blocks

# def analyze_dataflow(G, transfer, bottom, join):
#     from graph import deque
#     from functools import reduce
#     trans_G = transpose(G)
#     mapping = dict((v, bottom) for v in G.vertices())
#     worklist = deque(G.vertices)
#     while worklist:
#         node = worklist.pop()
#         inputs = [mapping[v] for v in trans_G.adjacent(node)]
#         input = reduce(join, inputs, bottom)
#         output = transfer(node, input)
#         if output != mapping[node]:
#             mapping[node] = output
#             worklist.extend(G.adjacent(node))

# class Compiler(Compiler_Lwhile,Compiler_Reg_Alloc):
#     ...
#     Compiler_Reg_Alloc.uncover_live

#     def uncover_live(self, p: CProgram) -> Dict[str,Dict[instr, Set[location]]]:
#         super().uncover_live
#         cfg = self.control_flow_graph_from(p)
#         cfg = transpose(cfg)
        
#         live_bf_block = {lbl:set() for lbl in p.body.keys()}
#         live_af_block = {lbl:set() for lbl in p.body.keys()}
#         res = {lbl:dict() for lbl in p.body.keys()}
        
        
#         def before(cur_label,i):
#             match i:
#                 case Jump(label=lbl)|JumpIf(label=lbl):
#                     return live_bf_block[lbl]
#                 case _:
#                     return set.union(set.difference(res[cur_label][i],self.write_vars(i)),
#                                      self.read_vars(i),)
        
#         def transfer(label,live_af_block):
#             super
#             for i in p.body[label]:
#                 ...
#             return live_bf_block[label]
        
#         analyze_dataflow(
#             G=cfg,
#             transfer=transfer,
#             bottom={},
#             join=set.union
#         )
#         return res