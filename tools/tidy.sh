#!/bin/bash
R_INC=$(Rscript -e "cat(Sys.getenv(\"R_INCLUDE_DIR\"))")
RCPP_INC=$(Rscript --silent -e "Rcpp:::CxxFlags()")
TT_INC="${RCPP_INC}/../../testthat/include"
ICD_HOME=${ICD_HOME:-~/icd}

# temporarily drop (restored on next ./configure)
# -i for mac compatibility
sed -i.bak '/OPENMP/d' "${ICD_HOME}/src/config.h"
rm -f "${ICD_HOME}/src/config.h.bak"
#sed -i.bak '/TESTTHAT/d' "${ICD_HOME}/src/config.h"
#rm -f "${ICD_HOME}/src/config.h.bak"

pushd /tmp
clang-tidy "${ICD_HOME}"/src/*.cpp -- -I"${R_INC}" "${RCPP_INC}" "${TT_INC}" -I"${ICD_HOME}/src" -std=c++11
popd
