#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

# now do something like:
# MAKEFLAGS="-k CCACHE= CXX11=/usr/bin/iwyu" tools/build-binary-quick.sh --preclean

src="${ICD_HOME:-${HOME}/icd}"

[[ -d "${src}" ]] || { echo "${src} not found."; exit 1; }

tmp_lib="$(mktemp -d)"
cd "${tmp_lib}" || { echo "cannot cd to ${tmp_lib}."; exit 1;}

# quickly build the pacakge, and put it in the current directory
R CMD INSTALL \
    -d \
    --build \
    --library="${tmp_lib}" \
    --libs-only \
    --no-docs \
    --no-data \
    --no-test-load \
    --no-clean-on-error \
    "$@" \
    "${src}"

echo "Output in ${tmp_lib}"
