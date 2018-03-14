#!/bin/bash
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

ICD_HOME=$HOME/Documents/RProjects/icd
pushd $ICD_HOME
R -d "valgrind --tool=callgrind --leak-check=full --track-origins=yes --show-leak-kinds=all -v" -e "library(devtools); library(testthat); test()"
#R -d "valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all -v" -e "library(devtools); library(testthat); check(cran = TRUE)"

# other useful options: --instr-atstart=no
# then 'callgrind_control -i' or prograammatically start instrumentation

popd
