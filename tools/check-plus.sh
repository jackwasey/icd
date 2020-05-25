#!/usr/bin/env bash
#shellcheck disable=SC2012
set -eu
IFS=$'\n\t'
ICD_HOME="${ICD_HOME:-${HOME}/icd}"
tmpd=$(mktemp -d /tmp/icdcheckplus.XXXXXXXXXXX)
function finish {
    #	  rm -rf "$tmpd"
    echo "Finished with $tmpd" >&2
}
trap finish EXIT
cd "$tmpd"
# build with standard release options, including compacting vignettes
"${ICD_HOME}"/tools/build-full.sh
# for all environment variable options see here:
# https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Tools
#R_MAKEVARS_USER="$HOME/.R/Makevars.clang" \
#R_CHECK_CONSTANTS=5 \
# N.b. R_CHECK_CONSTANTS and R_JIT_STRATEGY work together, but can make examples and tests run very slowly.
# using --as-cran, I think, over-rides any user environment for check
tarball="$(ls -t "$tmpd"/icd*.tar.gz | head -1)"
R_CHECK_ENVIRON="${ICD_HOME}/env/plus" \
    R CMD check "${tarball}"
