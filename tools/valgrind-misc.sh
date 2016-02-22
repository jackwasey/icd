#!/bin/bash
# R -d valgrind -e "library(icd); library(testthat); source('tests/testthat/helper-base.R');
# R -d valgrind -e "library(icd); library(testthat); test_package('icd9', reporter=VeryVerboseReporter())"
R -d "valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all -v" -e "library(icd); library(testthat); test_package('icd9')"
# R -d valgrind -e "icd::icd_comorbid_ahrq(data.frame(visit_id = c('a','a'), icd9 = c('39891','00321')), short_code = TRUE)"
# R -d valgrind -e "source('tests/testthat/helper-base.R'); icd::icd9ComorbidAhrq(ahrqTestDat, isShort = T, abbrevNames = F, applyHierarchy = T)"

R -d "valgrind --tool=callgrind --simulate-cache=yes" -e "library(icd); icd:::icd9BenchComorbid(10000)"
# then run:
# callgrind_annotate --auto=yes callgrind.out.#PID filename.cpp

# test with real data
R -d "valgrind --tool=callgrind --simulate-cache=yes" -e "library(icd); load('i9test.RData'); icd_comorbid_ahrq(i9test, visit_id = 'patcom', icd_name = 'i9diag')"

# stress with fabricated data on parallel implementation
R -d "valgrind --tool=callgrind --simulate-cache=yes" -e "library(icd); devnull<-icd:::sc(1000000)"

# valgrind parallel code? yes, can use flag to split out threads.
# another useful option I put in .valgrindrc is --instr-atstart=no, so setup runs fast, then use ifdefs to enable valgrind in the key code areas.
R -d "valgrind --tool=callgrind --simulate-cache=yes" -e "library(icd); pts<-icd:::generate_random_pts(5e6,10); j<-icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid, threads = 1)"

R -d "valgrind --tool=callgrind --simulate-cache=yes" -e "library(icd); library(microbenchmark); benchVaryn()"

# useful to install first...
R CMD INSTALL icd9 && R -d "valgrind --tool=callgrind"  -e "library(icd); pts <- icd:::generate_random_ordered_pts(1000, 10); icd:::icd9LongToWideMatrix(pts)"


# other useful options: --instr-atstart=no
# then callgrind_control -i (or prograammatically) start instrumentation
