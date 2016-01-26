#!/bin/bash
ICD9_HOME=$HOME/Documents/RProjects/icd9
cd $ICD9_HOME
#R --vanilla --slave -d "valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=no -v" -e "library(devtools); library(testthat); load_all(); icd9ComorbidQuanDeyo(icd9RandomShort(1000))"
#R --vanilla --slave -e "library(devtools); load_all(); pts<-generate_random_pts(1e3); devnull <- icd9ComorbiditiesQuanDeyo(pts)"
# getting bizarre OpenCL ddply segfaults in profr: R --vanilla --slave -e "library(devtools); load_all(); pts<-generate_random_pts(1e3); profr::profr(devnull <- icd9ComorbiditiesQuanDeyo(pts))"
R --vanilla --slave -e "library(devtools); load_all(); pts<-generate_random_pts(1e3); Rprofr(\"profr.out\", devnull <- icd9ComorbiditiesQuanDeyo(pts))"

# other useful options: --instr-atstart=no
# then 'callgrind_control -i' or prograammatically start instrumentation
