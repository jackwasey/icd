#!/bin/bash
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

ICD_HOME=$HOME/icd
pushd $ICD_HOME
Rscript --with-default-packages=icd -e "pts<-icd:::generate_random_pts(1e3); Rprof(\"profr.out\"); x<-icd_comorbid_quan_deyo(pts); Rprof(NULL)"
popd
