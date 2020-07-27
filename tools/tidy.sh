#!/usr/bin/env bash
set -x
R_inc_path="$(Rscript -e "cat(Sys.getenv(\"R_INCLUDE_DIR\"))")"
Rcpp_inc_path="$(Rscript -e 'cat(tools::file_path_as_absolute(base::system.file(package = "Rcpp")))')/include"
RcppEigen_inc_path="$(Rscript -e 'cat(tools::file_path_as_absolute(base::system.file(package = "RcppEigen")))')/include"
#RCPP_INC=$(Rscript --silent -e "Rcpp:::CxxFlags()")
#RCPPEIGEN_INC=$(Rscript --silent -e "RcppEigen:::CxxFlags()")
#RCPPEIGEN_INC="$(Rscript --silent -e 'cat(paste0("-I", system.file("include", package = "RcppEigen")))')"
#RCPPEIGEN_INC=-I/usr/local/lib/R/site-library/RcppEigen/include
#TT_INC="${RCPP_INC}/../../testthat/include"
ICD_HOME=${ICD_HOME:-${HOME}/icd}

cd "${ICD_HOME}" || { echo "unable to cd to ${ICD_HOME}" >&2; exit 1; }

# test with one file for principle of this:
for f in src/comorbidMatMul.cpp src/*.cpp
do
    clang-tidy-10 "$@" "${f}" -- \
        -isystem "${R_inc_path}" \
        -isystem "${Rcpp_inc_path}" \
        -isystem "${RcppEigen_inc_path}" \
        --system-header-prefix=Eigen \
        --system-header-prefix=Eigen/src \
        --system-header-prefix=Eigen/src/SparseCore \
        --system-header-prefix=Rcpp \
        --system-header-prefix=internal \
        --system-header-prefix="${Rcpp_inc_path}" \
        --system-header-prefix="${RcppEigen_inc_path}" \
        --system-header-prefix=Rcpp/internal \
        -DICD_CLANG_TIDY \
        -DNDEBUG \
        -fpic -pipe -w -std=gnu++11

    done


# clang-tidy src/*.cpp -- \
#     -I"${R_INC}" \
#     "${RCPP_INC}" \
#     "${RCPPEIGEN_INC}" \
#     -I"${ICD_HOME}/src" \
#     -I"${ICD_HOME}/inst/include" \
#     -std=c++11
