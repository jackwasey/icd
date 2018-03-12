#!/bin/bash
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

ICD_HOME=${ICD_HOME:-"$HOME/rprojects/icd"}
pushd "$ICD_HOME"

RSNIP=${1:-icd:::icd9ChildrenShortCppUnordered(c("001", 100:115), onlyReal = TRUE)}
RCODE=${RCODE:-library("microbenchmark"); library("testthat"); library("icd"); invisible($RSNIP)}

#RCODE=${RCODE:-devtools::load_all(); valgrindCallgrindStart(TRUE); invisible($RSNIP); valgrindCallgrindStop()}

INSTR_ATSTART="no"
VALGRIND_CMD="valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=$INSTR_ATSTART --separate-threads=no"
R --vanilla --slave -d "$VALGRIND_CMD" -e "$RCODE"
popd

