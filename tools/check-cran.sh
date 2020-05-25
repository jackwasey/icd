#!/usr/bin/env bash
#shellcheck disable=SC2012
set -eu
IFS=$'\n\t'
ICD_HOME="${ICD_HOME:-${HOME}/icd}"
tmpd=$(mktemp -d /tmp/icdcheckcran.XXXXXXXXXXX)
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

# using --as-cran, I think, over-rides any user environment for check
tarball="$(ls -t "$tmpd"/icd*.tar.gz | head -1)"
R_CHECK_ENVIRON="${ICD_HOME}/env/cran" \
    R CMD check "${tarball}"
