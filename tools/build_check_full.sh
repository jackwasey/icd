#!/bin/bash
set -euo pipefail
IFS=$'\n\t'
tmpd=$(mktemp -d /tmp/icdcheckfull.XXXXXXXXXXX)
function finish {
#	  rm -rf "$tmpd"
  echo "Finished with $tmpd"
}
trap finish EXIT
#rsync -r --exclude=".git" "${ICD_HOME:-$HOME/rprojects/icd}" "$tmpd"
pushd "$tmpd"

# build with standard release options, i.e. compacting vignettes.
${ICD_HOME:-$HOME/rprojects/icd}/tools/build.sh

# for all environment variable options see here:
# https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Tools
# R_MAKEVARS_USER="$HOME/.R/Makevars.mac.quick" \
MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN) \
  ICD_TEST_SLOW=true \
  ICD_TEST_BUILD_DATA=true \
  ICD_TEST_DEPRECATED=true \
  _R_CHECK_ALL_NON_ISO_C_=TRUE \
  _R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_=TRUE \
  _R_CHECK_CODE_ASSIGN_TO_GLOBALENV_=TRUE \
  _R_CHECK_CODETOOLS_PROFILE_="suppressLocalUnused=FALSE" \
  _R_CHECK_CRAN_INCOMING_=false \
  _R_CHECK_CRAN_INCOMING_REMOTE_=false \
  _R_CHECK_EXIT_ON_FIRST_ERROR_=TRUE \
  _R_CHECK_NATIVE_ROUTINE_REGISTRATION_=TRUE \
  _R_CHECK_RD_EXAMPLES_T_AND_F_=TRUE \
  _R_CHECK_RD_LINE_WIDTHS_=true \
  _R_CHECK_TESTS_NLINES_=0 \
  _R_CHECK_USE_INSTALL_LOG_=TRUE \
  _R_CHECK_VIGNETTES_NLINES_=0 \
 R CMD check "$(ls -t $tmpd/icd*.tar.gz | head -1)"
popd
