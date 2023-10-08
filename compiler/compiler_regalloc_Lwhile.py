from ast import *
from graph import transpose 
from x86_ast import *
from dataflow_analysis import analyze_dataflow
from compiler.compiler_Lwhile import Compiler as Compiler_Lwhile
from compiler.compiler_regalloc_Lif import Compiler as Compiler_Regalloc


class Compiler(Compiler_Lwhile,Compiler_Regalloc):
    def uncover_live(self, p: X86Program) -> dict[str,dict[instr, set[location]]]:
        cfg = self.control_flow_graph_from(p)
        cfg = transpose(cfg)
        live_bf_block = {lbl:set() for lbl in cfg.vertices()}
        live_after = {lbl:dict() for lbl in p.body.keys()}
        
        def transfer(label,live_after_blk):
            '''
            The second parameter transfer should be passed a function that applies liveness analysis to a basic block. It takes two parameters: the label for the block to analyze and the live-after set for that block. The transfer function should return the live-before set for the block. Also, as a side effect, it should update the live-before and live-after sets for each instruction. To implement the transfer function, you should be able to reuse the code you already have for analyzing basic blocks.
            '''
            if label != 'conclusion':
                live_after[label] = {p.body[label][-1]:live_after_blk}
                live_after[label] = self.uncover_block(p.body[label],live_bf_block)
                s,*_ = p.body[label]
                live_bf_block[label] = self.live_before(s,live_after[label][s],live_bf_block)
            
            return live_bf_block[label]
        
        analyze_dataflow(
            G=cfg,
            transfer=transfer,
            bottom=set(),
            join=set.union
        )
        return live_after