#!/usr/bin/env bash
#shellcheck disable=SC2012
set -eu
IFS=$'\n\t'

declare IH

set -x
if [[ -z ${IH-} ]]; then
    for d in "${PWD}" "${PWD}"/.. "${PWD}"/../..; do
        [[ -e "${d}/DESCRIPTION" ]] && IH="${d}" && break
    done
fi
cd "${IH?}" || { echo "cannot cd to ${IH?}" >&2; exit 1; }
[[ -e "DESCRIPTION" ]] || {
    echo "still cannot find R package source root" >&2
    exit 1
}

tmpd="$(mktemp -d "/tmp/${0##*/}.XXXXXXXXXXX")"
function finish {
    #	  rm -rf "$tmpd"
    echo "Finished with $tmpd"
}
trap finish EXIT
#rsync -r --exclude=".git" "${IH?}" "$tmpd"
cd "$tmpd"
#shellcheck disable=SC2097,SC2098
IH="${IH?}" "${IH?}"/tools/build-quick.sh

# try to unset debug flag, so ccache caches the results regardless of original path,
# or configure ccache to do this

# for all environment variable options see here:
# https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Tools
# R_MAKEVARS_USER="$HOME/.R/Makevars.mac.quick" \
tarball="$(ls -t "$tmpd"/icd*.tar.gz | head -1)"

# Makevars is irrelevant if we don't compile anything.
# harder to set R_MAKEVARS_USER in the environment file, because not clear what current directory, or make/shell environment is there.
R_CHECK_ENVIRON="${IH?}/tools/env/quick" \
R_MAKEVARS_USER=${IH?}/tools/mk/Makevars.quick \
    R CMD check \
    --no-codoc \
    --no-manual \
    --run-donttest \
    --timings \
    --no-build-vignettes \
    --ignore-vignettes \
    "${tarball}"
