#!/bin/bash
ICD_HOME=$HOME/Documents/RProjects/icd
cd $ICD_HOME
#R --vanilla --slave -d "valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=no -v" -e "library(devtools); library(testthat); load_all(); icd_comorbid_quan_deyo(icd:::generate_random_unordered_pts(1000))"

${RCODE:="library(testthat); devtools::load_all(); message('Testing: $1'); source('$ICD_HOME/tests/testthat/helper-base.R'); test_file('$1')"}
INSTR_ATSTART="no"
VALGRIND_CMD="valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=$INSTR_ATSTART --separate-threads=yes"

R --vanilla --slave -d "$VALGRIND_CMD" -e "message(\"ready to valgrind\"); $RCODE"


# other useful options: --instr-atstart=no --separate-threads=yes
# then 'callgrind_control -i' or prograammatically start instrumentation
