#!/bin/bash
R_INC=$(Rscript -e "cat(Sys.getenv(\"R_INCLUDE_DIR\"))")
RCPP_INC=$(Rscript --silent -e "Rcpp:::CxxFlags()")
RCPPEIGEN_INC=$(Rscript --silent -e "RcppEigen:::CxxFlags()")
TT_INC="${RCPP_INC}/../../testthat/include"
ICD_HOME=${ICD_HOME:-$HOME/icd}

pushd /tmp
clang-tidy "${ICD_HOME}"/src/*.cpp -- \
  -I"${R_INC}" \
  "${RCPP_INC}" \
  "${RCPPEIGEN_INC}" \
  "${TT_INC}" \
  -I"${ICD_HOME}/src" \
  -std=c++11
popd
