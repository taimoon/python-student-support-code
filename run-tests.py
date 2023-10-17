import os
import sys

sys.path.append('../python-student-support-code')
sys.path.append('../python-student-support-code/interp_x86')


from utils import run_tests, run_one_test, enable_tracing

from type_check_Llambda import TypeCheckLlambda as TypeCheckL
from type_check_Clambda import TypeCheckClambda as TypeCheckC
type_checkL = TypeCheckL().type_check
type_checkC = TypeCheckC().type_check

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
    # 'select_instructions': interp_x86, # early stop
    # 'assign_homes': interp_x86,
    # 'patch_instructions': interp_x86,
    # 'prelude_and_conclusion':interp_x86,
}

from interp_Llambda import InterpLlambda as InterpL
from interp_Clambda import InterpClambda as InterpC

interpL = InterpL().interp
interpC = InterpC().interp

interp_dict = {
    # 'source':interpL,
    # 'shrink':interpL,
    # 'reveal_functions':interpL,
    # 'convert_assignments':interpL,
    # 'convert_to_closures':interpL,
    # 'limit_functions':interpL,
    # 'expose_allocation':interpL,
    # 'remove_complex_operands': interpL,
    # 'explicate_control':interpC,
    # 'select_instructions': interp_x86, # early stop
    # 'assign_homes': interp_x86,
    # 'patch_instructions': interp_x86,
    # 'prelude_and_conclusion':interp_x86,
}

def test_iftype_error():
    from glob import glob
    for f in glob(os.getcwd() + '/tests/if_typecheck_error/*.py'): 
        try:
            run_one_test(f,'if',None,'if',typecheck_dict,interp_dict)
        except Exception as e:
            print(e)

# from compiler.compiler import Compiler
# from compiler.compiler_regalloc_Lif import Compiler
# from compiler.compiler_Lif import Compiler
# from compiler.compiler_regalloc_Lwhile import Compiler
# from compiler.compiler_Ltup import Compiler
# from compiler.compiler_regalloc_Ltup import Compiler
# from compiler.compiler_Lfun import Compiler
# from compiler.compiler_regalloc_Lfun import Compiler
from compiler.compiler_Llambda import Compiler
compiler = Compiler()

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--enable_tracing',action='store_true')
    parser.add_argument('--run_once',action='store_true')
    # sys.setrecursionlimit(10000)
    args = parser.parse_args()
    if args.enable_tracing is True:
        enable_tracing()
    
    if args.run_once is True:
        path = os.getcwd() + '/tests/lambda/primesum.py'
        run_one_test(path,
                    'fun',
                    compiler,
                    'fun',
                    typecheck_dict,
                    interp_dict)
    else:
        name = 'lambda'
        run_tests(name, compiler, name,
                typecheck_dict,
                interp_dict)

