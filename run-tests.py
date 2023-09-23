import os
import sys

sys.path.append('../python-student-support-code')
sys.path.append('../python-student-support-code/interp_x86')


from utils import run_tests, run_one_test, enable_tracing
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
    # 'select_instructions': interp_x86, # early stop
    # 'assign_homes': interp_x86,
    # 'patch_instructions': interp_x86,
    # 'prelude_and_conclusion':interp_x86,
}

from interp_Ltup import InterpLtup
from interp_Ctup import InterpCtup
interpLtup = InterpLtup().interp
interpCtup = InterpCtup().interp

interp_dict = {
    'shrink':interpLtup,
    'expose_allocation':interpLtup,
    'remove_complex_operands': interpLtup,
    'explicate_control':interpCtup,
    'select_instructions': interp_x86, # early stop
    'assign_homes': interp_x86,
    'patch_instructions': interp_x86,
    'prelude_and_conclusion':interp_x86,
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
from compiler.compiler_Ltup import Compiler

compiler = Compiler()

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--enable_tracing',action='store_true')
    parser.add_argument('--run_once',action='store_true')

    args = parser.parse_args()
    if args.enable_tracing is True:
        enable_tracing()
    
    if args.run_once is True:
        path = os.getcwd() + '/tests/tuple/eg.py'
        # path = os.getcwd() + '/tests/var/add.py'
        run_one_test(path,
                    'if',
                    compiler,
                    'if',
                    typecheck_dict,
                    interp_dict)
    else:
        name = 'var'
        run_tests(name, compiler, name,
                typecheck_dict,
                interp_dict)

