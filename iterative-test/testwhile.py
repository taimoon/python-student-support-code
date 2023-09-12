prog = '''
n = 10
x = 0
while n > 0:
    x = x + n
    n = n - 1
print(x)
'''
import sys
sys.path.append('../python-student-support-code')
from compiler.compiler_Lwhile import Compiler
from interp_Lwhile import InterpLwhile
from interp_Cif import InterpCif
def compile(prog):
    compiler = Compiler()
    passes = [
        ('shrink',compiler.shrink,InterpLwhile().interp),
        ('remove_complex_operands',compiler.remove_complex_operands,InterpLwhile().interp),
        ('explicate_control',compiler.explicate_control,InterpCif().interp)
    ]
    for _,p,interp in passes:
        prog = p(prog)
        print(prog)
        print('*'*100)
        interp(prog)
        print()
        print('*'*100)
    return prog
import ast
obj = compile(ast.parse(prog))
print(obj)