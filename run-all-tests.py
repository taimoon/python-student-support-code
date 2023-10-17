import os
import sys
sys.setrecursionlimit(5000)

sys.path.append('../python-student-support-code')
sys.path.append('../python-student-support-code/interp_x86')

from utils import (run_tests, run_one_test, enable_tracing)
from interp_x86.eval_x86 import interp_x86

from type_check_Llambda import TypeCheckLlambda as TypeCheckL
from type_check_Clambda import TypeCheckClambda as TypeCheckC
type_checkC = TypeCheckC().type_check
type_checkL = TypeCheckL().type_check

def init_typecheck_dict(type_checkL=type_checkL,type_checkC=type_checkC):
    typecheck_dict = {
        'source': type_checkL,
        'shrink':type_checkL, # get the tuple_types
        'limit_functions':type_checkL,
        'reveal_functions':type_checkL, # get the has_type
        'convert_assignments':type_checkL,
        'convert_to_closures':type_checkL,
        'expose_allocation':type_checkL,
        'remove_complex_operands': type_checkL,
        'explicate_control':type_checkC, # get the var_types
    }
    return typecheck_dict

def init_interp_dict():
    from interp_Cfun import InterpCfun as InterpC
    from interp_Lfun import InterpLfun as InterpL

    interpL = InterpL().interp
    interpC = InterpC().interp

    interp_dict = {
        # 'shrink':interpL,
        # 'expose_allocation':interpL,
        # 'remove_complex_operands': interpL,
        # 'explicate_control':interpC, # get var_types
        # 'select_instructions': interp_x86,
        # 'assign_homes': interp_x86,
        # 'patch_instructions': interp_x86,
        # 'prelude_and_conclusion':interp_x86,
    }
    return interp_dict

def run_all_tests(names,compiler
    ,typecheck_dict=init_typecheck_dict()
    ,interp_dict=init_interp_dict()
    ):
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
    from type_check_Ctup import TypeCheckCtup as TypeCheckC
    typecheck_dict = init_typecheck_dict(type_checkC=TypeCheckC().type_check)
    run_all_tests(names,compiler,typecheck_dict)

def test_Lif_regalloc():
    from compiler.compiler_regalloc_Lif import Compiler
    compiler = Compiler()
    print('compiler_regalloc_Lif')
    names = ['var','if']
    from type_check_Ctup import TypeCheckCtup as TypeCheckC
    typecheck_dict = init_typecheck_dict(type_checkC=TypeCheckC().type_check)
    run_all_tests(names,compiler,typecheck_dict)

def test_Lwhile():
    from compiler.compiler_Lwhile import Compiler
    compiler = Compiler()
    print('compiler_Lwhile')
    names = ['var','if','while']
    from type_check_Ctup import TypeCheckCtup as TypeCheckC
    typecheck_dict = init_typecheck_dict(type_checkC=TypeCheckC().type_check)
    run_all_tests(names,compiler,typecheck_dict)

def test_Lwhile_regalloc():
    from compiler.compiler_regalloc_Lwhile import Compiler
    compiler = Compiler()
    print('compiler_regalloc_Lwhile')
    names = ['var','if','while']
    from type_check_Ctup import TypeCheckCtup as TypeCheckC
    typecheck_dict = init_typecheck_dict(type_checkC=TypeCheckC().type_check)
    run_all_tests(names,compiler,typecheck_dict)
    
def test_Ltup():
    from compiler.compiler_Ltup import Compiler
    compiler = Compiler()
    print('compiler_Ltup')
    names = ['var','if','while','tuple']
    from type_check_Ctup import TypeCheckCtup as TypeCheckC
    typecheck_dict = init_typecheck_dict(type_checkC=TypeCheckC().type_check)
    run_all_tests(names,compiler,typecheck_dict)


def test_Ltup_regalloc():
    from compiler.compiler_regalloc_Ltup import Compiler
    compiler = Compiler()
    print('compiler_Ltup_regalloc')
    names = ['var','if','while','tuple']
    from type_check_Ctup import TypeCheckCtup as TypeCheckC
    typecheck_dict = init_typecheck_dict(type_checkC=TypeCheckC().type_check)
    run_all_tests(names,compiler,typecheck_dict)

def test_Lfun():
    from compiler.compiler_Lfun import Compiler
    compiler = Compiler()
    print('compiler_Lfun')
    names = ['var','if','while','tuple','fun']
    run_all_tests(names,compiler)
    
def test_Lfun_regalloc():
    from compiler.compiler_regalloc_Lfun import Compiler
    compiler = Compiler()
    print('compiler_Lfun_regalloc')
    names = ['var','if','while','tuple','fun']
    run_all_tests(names,compiler)

def test_Llambda():
    from compiler.compiler_Llambda import Compiler
    compiler = Compiler()
    print('compiler_Lfun_regalloc')
    names = ['var','if','while','tuple','fun','lambda']
    run_all_tests(names,compiler)

if __name__ == '__main__':
    test_Lvar()
    test_Lvar_regalloc()
    test_Lif()
    test_Lif_regalloc()
    test_Lwhile()
    test_Lwhile_regalloc()
    test_Ltup()
    test_Ltup_regalloc()
    test_Lfun()
    test_Lfun_regalloc()
    test_Llambda()