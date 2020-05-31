#!/usr/bin/env bash
set -x
cd "${ICD_HOME:-$HOME/icd/src}" || { echo "failed to cd to icd home" >&2; exit 1; }
find . -name '*.c' -o -name '*.h' -o \( -name '*.cpp' -a -not -name 'RcppExports.cpp' \) |
    while read -r f; do
        xargs clang-format-9 "$f";
    done
