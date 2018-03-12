#!/bin/bash
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'


set -x
VERBOSE=""

# $1 is a filter to select which test(s) to run
# $2 is "yes" or "no" whether to instrument from beginning
TESTFILTER=${1:-comorbid-calcs-icd9}
INSTR_ATSTART=${2:-no}

ICD_HOME=${ICD_HOME:-"$HOME/rprojects/icd"}
cd $ICD_HOME

RCODE=${RCODE:-"library(testthat); message('Testing with valgrind: $TESTFILTER'); devtools::test(filter = '$TESTFILTER')"}
VALGRIND_CMD="valgrind $VERBOSE --tool=callgrind --simulate-cache=yes --instr-atstart=$INSTR_ATSTART" # --log-file=/tmp/callgrind.%p-%n" # --separate-threads=yes"

R --vanilla --slave -d "$VALGRIND_CMD" -e "$RCODE"

# use 'callgrind_control -i' or prograammatically start instrumentation
