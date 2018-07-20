#!/bin/bash
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'
CALLGRIND="valgrind --tool=callgrind --simulate-cache=yes"
PKG_ARG="--default-packages=icd"

# R -d valgrind -e "library(icd); library(testthat); source('tests/testthat/helper-base.R');
R -d "valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all -v" -e "library(icd); library(testthat); test_package('icd')"
# R -d valgrind -e "icd::comorbid_ahrq(data.frame(visit_id = c('a','a'), icd = c('39891','00321')), short_code = TRUE)"

R -d "$CALLGRIND" -e "library(icd); icd:::icd_bench_comorbid_parallel()"
# then run:
# callgrind_annotate --auto=yes callgrind.out.#PID filename.cpp

# test with real data, if available!
R -d "$CALLGRIND" -e "library(icd); load('i9test.RData'); comorbid_ahrq(i9test, visit_id = 'patcom', icd_name = 'i9diag')"

# stress with fabricated data on parallel implementation
R -d "$CALLGRIND" -e "library(icd); devnull<-icd:::sc(1000000)"

# valgrind parallel code? yes, can use flag to split out threads.
# another useful option I put in .valgrindrc is --instr-atstart=no, so setup runs fast, then use ifdefs to enable valgrind in the key code areas.

R -d "$CALLGRIND" -e "library(icd); library(microbenchmark); bench_vary_n()"

R CMD INSTALL ~/icd && Rscript $PKG_ARG -d "$CALLGRIND" -e "pts <- generate_random_ordered_pts(1000, 10); long_to_wide(pts)"

# other useful options: --instr-atstart=no
# then callgrind_control -i (or prograammatically) start instrumentation
