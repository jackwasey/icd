#!/bin/bash
#shellcheck disable=SC2012
set -euo pipefail
whereami="$(dirname "${BASH_SOURCE[0]}")"
#shellcheck source=find-icd-home.sh
source "${whereami}/find-icd-home.sh"
# ICD_HOME=$(find_icd_home)
IFS=$'\n\t'
tmpd=$(mktemp -d /tmp/icdcheckcranplain.XXXXXXXXXXX)
function finish {
    #	  rm -rf "$tmpd"
    echo "Finished with $tmpd"
}
trap finish EXIT
#rsync -r --exclude=".git" "${ICD_HOME:-$HOME/rprojects/icd}" "$tmpd"
cd "$tmpd"
# build with standard release options, i.e. compacting vignettes.
"${whereami}"/build.sh
# for all environment variable options see here:
# https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Tools
#R_MAKEVARS_USER="$HOME/.R/Makevars.clang" \

# some flags are set by R-devel, so added here when using R-release

# also, CRAN sets it's own compilation flags, so we don't need to check them now and produce a spurious warning, and I think --as-cran overwrites what I might set here, anyway. see tools/check_plus.sh to simulate CRAN and have more control about avoiding development warnings.

MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN) \
    _R_CHECK_USE_INSTALL_LOG_=TRUE \
    _R_CHECK_LENGTH_1_CONDITION_="verbose,abort" \
    _R_CHECK_LENGTH_1_LOGIC2_="verbose,abort" \
    _R_CHECK_NO_STOP_ON_TEST_ERROR_=TRUE \
    _R_CHECK_COMPILATION_FLAGS_=FALSE \
    _R_CHECK_PKG_SIZES_=false \
    R CMD check --as-cran "$(ls -t "$tmpd"/icd*.tar.gz | head -1)"
