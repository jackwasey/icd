#!/bin/bash
R -d valgrind -e "library(icd9); library(testthat); source('tests/testthat/helper-base.R');/
test_file('tests/testthat/test-slow-comorbid.R')"
