#!/usr/bin/env bash
#shellcheck disable=SC2012
set -euo pipefail
IFS=$'\n\t'
ICD_HOME="${ICD_HOME:-${HOME}/icd}"

tmpd="$(mktemp -d "/tmp/${0##*/}.XXXXXXXXXXX")"
function finish {
    #	  rm -rf "$tmpd"
    echo "Finished with $tmpd"
}
trap finish EXIT
#rsync -r --exclude=".git" "${ICD_HOME:-$HOME/rprojects/icd}" "$tmpd"
cd "$tmpd"
"${ICD_HOME}/tools/build-quick.sh"

# try to unset debug flag, so ccache caches the results regardless of original path,
# or configure ccache to do this

# for all environment variable options see here:
# https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Tools
# R_MAKEVARS_USER="$HOME/.R/Makevars.mac.quick" \
tarball="$(ls -t "$tmpd"/icd*.tar.gz | head -1)"
R_CHECK_ENVIRON="${ICD_HOME}/tools/env/quick" \
    R_MAKEVARS_USER=${HOME}/.R/Makevars.quick \
    R CMD check \
    --no-build-vignettes \
    --ignore-vignettes \
    "${tarball}"
