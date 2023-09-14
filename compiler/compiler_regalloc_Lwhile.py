from ast import *
from utils import (CProgram)
from graph import transpose 
from x86_ast import *
from typing import (List, Set,Tuple,Dict)
from dataflow_analysis import analyze_dataflow
from compiler.compiler_Lwhile import Compiler as Compiler_Lwhile
from compiler.compiler_regalloc_Lif import Compiler as Compiler_Regalloc


class Compiler(Compiler_Lwhile,Compiler_Regalloc):
    def uncover_live(self, p: CProgram) -> Dict[str,Dict[instr, Set[location]]]:
        cfg = self.control_flow_graph_from(p)
        cfg = transpose(cfg)
        live_bf_block = {lbl:set() for lbl in cfg.vertices()}
        live_af_block = {lbl:set() for lbl in cfg.vertices()}
        live_after = {lbl:dict() for lbl in p.body.keys()}
        
        
        def transfer(label,live_after_blk):
            '''
            The second parameter transfer should be passed a function that applies liveness analysis to a basic block. It takes two parameters: the label for the block to analyze and the live-after set for that block. The transfer function should return the live-before set for the block. Also, as a side effect, it should update the live-before and live-after sets for each instruction. To implement the transfer function, you should be able to reuse the code you already have for analyzing basic blocks.
            '''
            if label == 'conclusion':
                return set()
            live_after[label] = {p.body[label][-1]:live_after_blk}
            live_after[label] = self.uncover_block(p.body[label],live_after[label],live_bf_block)
            live_bf_cur_block = self.live_before(p.body[label][0],live_after[label],live_bf_block)
            
            live_bf_block[label] = live_bf_cur_block
            live_af_block[label] = live_after[label][p.body[label][-1]]
            
            return live_bf_cur_block
        
        analyze_dataflow(
            G=cfg,
            transfer=transfer,
            bottom=set(),
            join=set.union
        )
        return live_after