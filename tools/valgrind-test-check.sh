#!/bin/bash
ICD_HOME=$HOME/Documents/RProjects/icd
cd $ICD_HOME
R -d "valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all -v" -e "library(devtools); library(testthat); test()"
#R -d "valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all -v" -e "library(devtools); library(testthat); check(cran = TRUE)"

# other useful options: --instr-atstart=no
# then 'callgrind_control -i' or prograammatically start instrumentation
