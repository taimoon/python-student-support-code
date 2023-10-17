from ast import *
from x86_ast import *
from compiler.compiler_Llambda import Compiler as Compiler_Llambda
from compiler.compiler_regalloc_Lfun import Compiler as Compiler_Regalloc

class Compiler(Compiler_Llambda,Compiler_Regalloc):
    pass