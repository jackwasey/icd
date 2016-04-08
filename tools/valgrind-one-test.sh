#!/bin/bash
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

ICD_HOME=${ICD_HOME:-"$HOME/Documents/RProjects/icd"}
cd $ICD_HOME

TESTFILE=${1:-test-current-comorbid-calcs-icd10.R}
RCODE=${RCODE:-"library(testthat); devtools::load_all(); message('Testing: $TESTFILE'); test_file('tests/testthat/$TESTFILE')"}
INSTR_ATSTART="yes"
VALGRIND_CMD="valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=$INSTR_ATSTART --separate-threads=yes"

R --vanilla --slave -d "$VALGRIND_CMD" -e "$RCODE"

# use 'callgrind_control -i' or prograammatically start instrumentation
