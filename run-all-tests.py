import os
import sys

sys.path.append('../python-student-support-code')
sys.path.append('../python-student-support-code/interp_x86')

from utils import (run_tests, run_one_test, enable_tracing)
from interp_x86.eval_x86 import interp_x86

import type_check_Lwhile
type_check = type_check_Lwhile.TypeCheckLwhile().type_check

typecheck_dict = {
    'source': type_check,
    'remove_complex_operands': type_check,
}

from interp_Cif import InterpCif
from interp_Lwhile import InterpLwhile
interpLwhile = InterpLwhile().interp
interpCif = InterpCif().interp
interp_dict = {
    'shrink':interpLwhile,
    'remove_complex_operands': interpLwhile,
    'explicate_control':interpCif,
    'select_instructions': interp_x86,
    'assign_homes': interp_x86,
    'patch_instructions': interp_x86,
    'prelude_and_conclusion':interp_x86,
}

from compiler.compiler_register_allocator import Compiler
compiler = Compiler()
names = ['var']
for name in names:
    run_tests(name, compiler, name,
            typecheck_dict,
            interp_dict)
    print('*'*100)

from compiler.compiler_regalloc_Lif import Compiler as Compiler_Lif
compiler = Compiler_Lif()
names = ['var','if']
for name in names:
    run_tests(name, compiler, name,
            typecheck_dict,
            interp_dict)
    print('*'*100)


from compiler.compiler_Lwhile import Compiler as Compiler_Lwhile
compiler = Compiler_Lwhile()
names = ['var','if','while']
for name in names:
    run_tests(name, compiler, name,
            typecheck_dict,
            interp_dict)
    print('*'*100)
