#!/usr/bin/env bash

## call with --fix

declare R_inc_path Rcpp_inc_path RcppEigen_inc_path ct
r_pkg_inc_path() {
  local rcmd
  rcmd="$(printf \
    'cat(tools::file_path_as_absolute(base::system.file(package = "%s")), "/include", sep="")' \
    "${1?}")"
  Rscript -e "${rcmd}"
}

R_inc_path="$(Rscript -e 'cat(Sys.getenv("R_INCLUDE_DIR"))')"
Rcpp_inc_path="$(r_pkg_inc_path Rcpp)"
RcppEigen_inc_path="$(r_pkg_inc_path RcppEigen)"

#RCPP_INC=$(Rscript --silent -e "Rcpp:::CxxFlags()")
#RCPPEIGEN_INC=$(Rscript --silent -e "RcppEigen:::CxxFlags()")
#RCPPEIGEN_INC="$(Rscript --silent -e 'cat(paste0("-I", system.file("include", package = "RcppEigen")))')"
#RCPPEIGEN_INC=-I/usr/local/lib/R/site-library/RcppEigen/include
#TT_INC="${RCPP_INC}/../../testthat/include"

while ! [[ -r DESCRIPTION ]]; do
  cd .. 2>/dev/null || break
done
ICD_HOME="${ICD_HOME:-${PWD}}"
grep -q -e "^Package: icd$" "${ICD_HOME}/DESCRIPTION" || {
  echo "${ICD_HOME} has a DESCRIPTION for a different package." \
    'Set ICD_HOME or enter icd source.' >&2
  exit 1
}

cd "${ICD_HOME}" || {
  echo "unable to cd to ${ICD_HOME}" >&2
  exit 1
}

for _n in -{15..10} ''; do
  ct="clang-tidy${_n}"
  command -v "${ct}" >/dev/null && break
done

echo "Using clang-tidy: ${ct}..."

## see src/.clang-tidy
for f in src/comorbidMatMul.cpp src/*.cpp; do
  "${ct}" "$@" "${f}" \
-- \
    -isystem "${R_inc_path}" \
    -isystem "${Rcpp_inc_path}" \
    -isystem "${RcppEigen_inc_path}" \
    -I"${R_inc_path}" \
    -I"${Rcpp_inc_path}" \
    -I"${RcppEigen_inc_path}" \
    --system-header-prefix=R_ext/ \
    --system-header-prefix=Rcpp/ \
    --system-header-prefix=RcppEigen/ \
    --system-header-prefix=unsupported/ \
    --system-header-prefix=internal/ \
    --system-header-prefix=Eigen/ \
    --system-header-prefix=Eigen/src/ \
    --system-header-prefix=Eigen/src/SparseCore/ \
    --system-header-prefix=SparseCore/ \
    --system-header-prefix=src/SparseCore/ \
    --system-header-prefix="${R_inc_path}" \
    --system-header-prefix="${Rcpp_inc_path}" \
    --system-header-prefix="${RcppEigen_inc_path}" \
    --system-header-prefix=Rcpp/internal \
    -DICD_CLANG_TIDY \
    -DNDEBUG \
    -fpic -pipe -w -std=gnu++17

done

# clang-tidy src/*.cpp -- \
#     -I"${R_INC}" \
#     "${RCPP_INC}" \
#     "${RCPPEIGEN_INC}" \
#     -I"${ICD_HOME}/src" \
#     -I"${ICD_HOME}/inst/include" \
#     -std=c++11
