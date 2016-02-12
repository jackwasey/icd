#!/bin/sh
R --slave -d valgrind -e 'library(testthat); library(icd); test_file("~/Documents/Projects/icd9/tests/testthat/test-cpp.R")'
R --slave -d valgrind -e 'library(testthat); library(icd); test_package("~/Documents/Projects/icd9")'
