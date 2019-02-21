#!/bin/bash
set -x
pushd ${ICD_HOME:-$HOME/icd/src}
files=$(find . -name '*.c' -o -name '*.h' -o \( -name '*.cpp' -a -not -name 'RcppExports.cpp' \) ) |
	xargs clang-format -i
popd

