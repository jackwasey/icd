#!/bin/bash
set -euo pipefail
IFS=$'\n\t'
tmpd=$(mktemp -d /tmp/icdcheckcranplain.XXXXXXXXXXX)
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
#R_MAKEVARS_USER="$HOME/.R/Makevars.clang" \
MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN) \
  #_R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_=TRUE \
  _R_CHECK_USE_INSTALL_LOG_=TRUE \
  _R_CHECK_LENGTH_1_CONDITION_="verbose,abort" \
  _R_CHECK_LENGTH_1_LOGIC2_="verbose,abort" \
  R CMD check --as-cran "$(ls -t $tmpd/icd*.tar.gz | head -1)"
popd
