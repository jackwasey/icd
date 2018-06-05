#!/bin/bash
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

R -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-cpp.R")'
R --slave -d valgrind -e 'library(icd); testthat::test_file("~/Documents/Projects/icd/tests/testthat/test-cpp.R")'
