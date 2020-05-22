#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

# quickly build the pacakge, and put it in the current directory
R CMD build \
    --log \
    --no-build-vignettes \
    --no-manual \
    --no-resave-data \
    "$@" \
    "${ICD_HOME?}"

