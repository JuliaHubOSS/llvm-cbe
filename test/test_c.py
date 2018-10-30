#!/usr/bin/env python3
import os
from glob import glob
from subprocess import call, check_output, STDOUT
import pytest


TEST_DIR = os.path.dirname(os.path.abspath(__file__))

LLVM_TOOL_DIR = os.environ.get(
    'LLVMToolDir',
    os.path.join(TEST_DIR, '..', '..', '..', 'build', 'bin'))

LLVM_CBE_PATH = os.path.join(LLVM_TOOL_DIR, 'llvm-cbe')


COMMON_CFLAGS = [
    '-Iinclude/',
    '-Wall',
    '-Wno-unused-function',
    '-Wno-unused-variable',
    '-Wno-error=attributes',
    '-Wno-attributes',
    '-Wno-main',
    '-Wno-error=main',
    '-Werror',
]

GCC = 'gcc'
GCCFLAGS = COMMON_CFLAGS + [
    '-Wno-error=unused-but-set-variable',
    '-Wno-unused-but-set-variable',
]

CLANG = 'clang'
CLANGFLAGS = COMMON_CFLAGS + [
    '-Wno-error=unused-variable',
    '-Wno-unused-variable',
]

# exit code used by tests to indicate success
TEST_SUCCESS_EXIT_CODE = 6
# exit code used by tests to indicate xfail
TEST_XFAIL_EXIT_CODE = 25


def check_no_output(args):
    output = check_output(args, stderr=STDOUT)
    assert not output


def _compile_c(compiler, flags, c_filename, output_filename):
    check_no_output([compiler, c_filename, '-o', output_filename] + flags)
    return output_filename


def compile_gcc(c_filename, output_filename, flags=None):
    flags = flags or []
    return _compile_c(GCC, GCCFLAGS + flags, c_filename, output_filename)


def compile_clang(c_filename, output_filename, flags=None):
    flags = flags or []
    return _compile_c(CLANG, CLANGFLAGS + flags, c_filename, output_filename)


def compile_to_ir(c_filename, ir_filename, flags=None):
    flags = list(flags or [])
    flags += ['-S', '-emit-llvm']
    return compile_clang(c_filename, ir_filename, flags=flags)


def run_llvm_cbe(ir_filename, c_filename):
    check_no_output([LLVM_CBE_PATH, ir_filename, '-o', c_filename])
    return c_filename


def get_c_test_desc(c_file):
    return os.path.splitext(os.path.basename(c_file))[0]


@pytest.mark.parametrize(
    'cflags',
    [['-O0'], ['-O1'], ['-O2'], ['-O3']],
    ids=lambda flags: ' '.join(flags)
)
@pytest.mark.parametrize(
    'c_file',
    glob(os.path.join(TEST_DIR, 'test*.c')),
    ids=get_c_test_desc,
)
def test_consistent_return_value(c_file, tmpdir, cflags):
    regular_exe = compile_clang(c_file, tmpdir / 'regular.exe', flags=cflags)
    regular_retval = call([regular_exe])
    print('regular executable returned', regular_retval)
    assert regular_retval in [TEST_SUCCESS_EXIT_CODE, TEST_XFAIL_EXIT_CODE]

    ir = compile_to_ir(c_file, tmpdir / 'ir.ll', flags=cflags)
    cbe_c = run_llvm_cbe(ir, tmpdir / 'cbe.c')
    cbe_exe = compile_gcc(cbe_c, tmpdir / 'cbe.exe', flags=cflags)
    cbe_retval = call([cbe_exe])
    print('cbe output returned', cbe_retval)
    assert cbe_retval == regular_retval