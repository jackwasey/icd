#!/bin/bash
# R -d valgrind -e "library(icd9); library(testthat); source('tests/testthat/helper-base.R');
# R -d valgrind -e "library(icd9); library(testthat); test_package('icd9', reporter=VeryVerboseReporter())"
R -d "valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all -v" -e "library(icd9); library(testthat); test_package('icd9')"
# R -d valgrind -e "icd9::icd9ComorbidAhrq(data.frame(visitId = c('a','a'), icd9 = c('39891','00321')), isShort = TRUE)"
# R -d valgrind -e "source('tests/testthat/helper-base.R'); icd9::icd9ComorbidAhrq(ahrqTestDat, isShort = T, abbrevNames = F, applyHierarchy = T)"

R -d "valgrind --tool=callgrind --simulate-cache=yes" -e "library(icd9); icd9:::icd9BenchComorbid(10000)"
# then run:
# callgrind_annotate --auto=yes callgrind.out.#PID filename.cpp

# test with real data
R -d "valgrind --tool=callgrind --simulate-cache=yes" -e "library(icd9); load('i9test.RData'); icd9ComorbidAhrq(i9test, visitId='patcom',icd9Field='i9diag')"

# stress with fabricated data on parallel implementation
R -d "valgrind --tool=callgrind --simulate-cache=yes" -e "library(icd9); devnull<-icd9:::sc(1000000)"

# valgrind parallel code? yes, can use flag to split out threads.
# another useful option I put in .valgrindrc is --instr-atstart=no, so setup runs fast, then use ifdefs to enable valgrind in the key code areas.
R -d "valgrind --tool=callgrind --simulate-cache=yes" -e "library(icd9); pts<-icd9:::randomPatients(5e6,10); j<-icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid, threads = 1)"
