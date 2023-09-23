import os
import sys
sys.setrecursionlimit(5000)

sys.path.append('../python-student-support-code')
sys.path.append('../python-student-support-code/interp_x86')

from utils import (run_tests, run_one_test, enable_tracing)
from interp_x86.eval_x86 import interp_x86

from type_check_Ltup import TypeCheckLtup
from type_check_Ctup import TypeCheckCtup
type_check_Ctup = TypeCheckCtup().type_check
type_check = TypeCheckLtup().type_check

typecheck_dict = {
    'source': type_check,
    'shrink':type_check, # get the tuple_types
    'expose_allocation':type_check,
    'remove_complex_operands': type_check,
    'explicate_control':type_check_Ctup, # get the var_types
}
from interp_Ctup import InterpCtup,InterpCif
from interp_Ltup import InterpLtup,InterpLwhile
interpL = InterpLtup().interp
interpC = InterpCtup().interp

interp_dict = {
    'shrink':interpL,
    'expose_allocation':interpL,
    'remove_complex_operands': interpL,
    'explicate_control':interpC,
    # 'select_instructions': interp_x86,
    # 'assign_homes': interp_x86,
    # 'patch_instructions': interp_x86,
    'prelude_and_conclusion':interp_x86,
}


def run_all_tests(names,compiler):
    for name in names:
        run_tests(name, compiler, name,
                typecheck_dict,
                interp_dict)
    print('*'*100)

def test_Lvar():
    from compiler.compiler import Compiler
    compiler = Compiler()
    print('compiler_Lvar')
    names = ['var']
    run_all_tests(names,compiler)

def test_Lvar_regalloc():
    from compiler.compiler_register_allocator import Compiler
    compiler = Compiler()
    print('compiler_regalloc_Lvar')
    names = ['var']
    run_all_tests(names,compiler)

def test_Lif():
    from compiler.compiler_Lif import Compiler
    compiler = Compiler()
    print('compiler_Lif')
    names = ['var','if']
    run_all_tests(names,compiler)

def test_Lif_regalloc():
    from compiler.compiler_regalloc_Lif import Compiler
    compiler = Compiler()
    print('compiler_regalloc_Lif')
    names = ['var','if']
    run_all_tests(names,compiler)

def test_Lwhile():
    from compiler.compiler_Lwhile import Compiler
    compiler = Compiler()
    print('compiler_Lwhile')
    names = ['var','if','while']
    run_all_tests(names,compiler)

def test_Lwhile_regalloc():
    from compiler.compiler_regalloc_Lwhile import Compiler
    compiler = Compiler()
    print('compiler_regalloc_Lwhile')
    names = ['var','if','while']
    run_all_tests(names,compiler)

def test_Ltup():
    from compiler.compiler_Ltup import Compiler
    compiler = Compiler()
    print('compiler_Ltup')
    names = ['var','if','while','tuple']
    run_all_tests(names,compiler)

if __name__ == '__main__':
    test_Lvar()
    test_Lvar_regalloc()
    test_Lif()
    test_Lif_regalloc()
    test_Lwhile()
    test_Lwhile_regalloc()
    test_Ltup()