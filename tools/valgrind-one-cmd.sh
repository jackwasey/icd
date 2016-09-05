#!/bin/bash
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

ICD_HOME=${ICD_HOME:-"$HOME/Documents/RProjects/icd"}
pushd "$ICD_HOME"

RSNIP=${1:-icd:::icd9ChildrenShortCpp(c("001", 100:105), onlyReal = TRUE)}
RCODE=${RCODE:-devtools::load_all(); invisible($RSNIP)}
#RCODE=${RCODE:-devtools::load_all(); valgrindCallgrindStart(TRUE); invisible($RSNIP); valgrindCallgrindStop()}
INSTR_ATSTART="yes"
VALGRIND_CMD="valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=$INSTR_ATSTART --separate-threads=no"

R --vanilla --slave -d "$VALGRIND_CMD" -e "$RCODE"

# then 'callgrind_control -i' or prograammatically start instrumentation
popd
