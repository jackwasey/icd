#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

# set environment here for slow and online tests
pushd /tmp
R CMD build --no-build-vignettes --no-manual --resave-data=no ~/icd

# for all environment variable options see here:
# https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Tools
_R_CHECK_CODETOOLS_PROFILE_="suppressLocalUnused=FALSE" \
 _R_CHECK_VIGNETTES_NLINES_=0 \
 _R_CHECK_RD_EXAMPLES_T_AND_F_=TRUE \
 _R_CHECK_CODE_ASSIGN_TO_GLOBALENV_=TRUE \
 _R_CHECK_EXIT_ON_FIRST_ERROR_=TRUE \
 _R_CHECK_TESTS_NLINES_=0 \
 _R_CHECK_NATIVE_ROUTINE_REGISTRATION_=TRUE \
 R CMD check --no-build-vignettes --no-manual --ignore-vignettes --no-install "$(ls -t /tmp/icd*.tar.gz | head -1)"
popd
