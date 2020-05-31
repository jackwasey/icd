#!/usr/bin/env bash
set -eu
IFS=$'\n\t'

ICD_HOME=$HOME/icd
cd "$ICD_HOME}" || { echo "cannot cd to ${ICD_HOME}" >&2; exit 1; }
Rscript --with-default-packages=icd \
    -e "pts<-icd:::generate_random_pts(1e3);
    Rprof(\"profr.out\");
    x <- icd_comorbid_quan_deyo(pts);
    Rprof(NULL)"
