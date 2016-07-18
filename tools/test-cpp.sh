#!/bin/sh
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

R --slave -d valgrind -e 'library(testthat); library(icd); test_file("~/Documents/Projects/icd/tests/testthat/test-cpp.R")'
R --slave -d valgrind -e 'library(testthat); library(icd); test_package("~/Documents/Projects/icd")'
