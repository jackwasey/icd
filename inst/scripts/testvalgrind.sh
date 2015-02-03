#!/bin/bash
# R -d valgrind -e "library(icd9); library(testthat); source('tests/testthat/helper-base.R');
# R -d valgrind -e "library(icd9); library(testthat); test_package('icd9', reporter=VeryVerboseReporter())"
R -d "valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all -v" -e "library(icd9); library(testthat); test_package('icd9')"
# R -d valgrind -e "icd9::icd9ComorbidAhrq(data.frame(visitId = c('a','a'), icd9 = c('39891','00321')), isShort = TRUE)"
# R -d valgrind -e "source('tests/testthat/helper-base.R'); icd9::icd9ComorbidAhrq(ahrqTestDat, isShort = T, abbrevNames = F, applyHierarchy = T)"

R -d "valgrind --tool=callgrind --simulate-cache=yes" -e "library(icd9); icd9:::icd9BenchComorbid(100)"
