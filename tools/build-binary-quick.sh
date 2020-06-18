#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

src="${ICD_HOME:-${HOME}/icd}"

[[ -d "${src}" ]] || { echo "${src} not found."; exit 1; }

#w quickly build the pacakge, and put it in the current directory
R CMD INSTALL \
    --build \
    --no-docs \
    --no-data \
    --no-test-load \
    "$@" \
    "${src}"
