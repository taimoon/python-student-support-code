import os
import sys

sys.path.append('../python-student-support-code')
sys.path.append('../python-student-support-code/interp_x86')


from utils import run_tests, run_one_test, enable_tracing
from interp_x86.eval_x86 import interp_x86

from type_check_Cfun import TypeCheckCfun as TypeCheckC
from type_check_Lfun import TypeCheckLfun as TypeCheckL
type_check_C = TypeCheckC().type_check
type_checkL = TypeCheckL().type_check

typecheck_dict = {
    'source': type_checkL,
    'shrink':type_checkL, # get the tuple_types
    'limit_functions':type_checkL,
    'reveal_functions':type_checkL,
    'expose_allocation':type_checkL,
    'remove_complex_operands': type_checkL,
    'explicate_control':type_check_C, # get the var_types
    # 'select_instructions': interp_x86, # early stop
    # 'assign_homes': interp_x86,
    # 'patch_instructions': interp_x86,
    # 'prelude_and_conclusion':interp_x86,
}

# from interp_Lif import InterpLif as InterpL
# from interp_Cif import InterpCif as InterpC

from interp_Lfun import InterpLfun as InterpL
from interp_Cfun import InterpCfun as InterpC

interpL = InterpL().interp
interpC = InterpC().interp

interp_dict = {
    # 'source':interpL,
    # 'shrink':interpL,
    # 'limit_functions':interpL,
    # 'reveal_functions':interpL,
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
from compiler.compiler_Lfun import Compiler

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
        path = os.getcwd() + '/tests/fun/primesum.py'
        
        run_one_test(path,
                    'fun',
                    compiler,
                    'fun',
                    typecheck_dict,
                    interp_dict)
    else:
        sys.setrecursionlimit(5000)
        name = 'fun'
        run_tests(name, compiler, name,
                typecheck_dict,
                interp_dict)

