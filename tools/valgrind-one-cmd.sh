#!/bin/bash
ICD9_HOME=$HOME/Documents/RProjects/icd9
cd $ICD9_HOME
#R --vanilla --slave -d "valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=no -v" -e "library(devtools); library(testthat); load_all(); icd9ComorbidQuanDeyo(icd9RandomShort(1000))"

RCODE="devnull <- icd9ComorbidQuanDeyo(icd9:::randomPatients(1000000))"
INSTR_ATSTART="no"
VALGRIND_CMD="valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=$INSTR_ATSTART --separate-threads=yes"

R --vanilla --slave -d "$VALGRIND_CMD" -e "library(icd9); message(\"ready to valgrind\"); $RCODE"


# other useful options: --instr-atstart=no --separate-threads=yes
# then 'callgrind_control -i' or prograammatically start instrumentation
