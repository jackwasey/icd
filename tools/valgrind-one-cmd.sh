#!/bin/bash
ICD9_HOME=$HOME/Documents/RProjects/icd9
cd $ICD9_HOME
#R --vanilla --slave -d "valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=no -v" -e "library(devtools); library(testthat); load_all(); icd9ComorbidQuanDeyo(icd9RandomShort(1000))"
R --vanilla --slave -d "valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=no" -e "library(devtools); library(testthat); load_all(); message(\"ready\"); devnull <- icd9ComorbidQuanDeyo(randomPatients(1000000))"

# other useful options: --instr-atstart=no
# then 'callgrind_control -i' or prograammatically start instrumentation
