#!/bin/bash
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

ICD_HOME=$HOME/Documents/RProjects/icd
cd $ICD_HOME
R --vanilla --slave -e "library(devtools); load_all(); pts<-generate_random_pts(1e3); Rprofr(\"profr.out\", devnull <- icd_comorbid_quan_deyo(pts))"

# other useful options: --instr-atstart=no
# then 'callgrind_control -i' or prograammatically start instrumentation
