#!/bin/bash
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

ICD_HOME=${ICD_HOME:-"$HOME/Documents/RProjects/icd"}
cd $ICD_HOME


${RCODE:="devnull <- icd_comorbid_quan_deyo(icd:::generate_random_unordered_pts(1000000))"}
INSTR_ATSTART="yes"
VALGRIND_CMD="valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=$INSTR_ATSTART --separate-threads=yes"

R --vanilla --slave -d "$VALGRIND_CMD" -e '$RCODE'

# then 'callgrind_control -i' or prograammatically start instrumentation
