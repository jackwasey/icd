#!/usr/bin/env bash
set -u
# quickly build the pacakge, and put it in the current directory
R CMD build \
    --log \
    --no-build-vignettes \
    --no-manual \
    --no-resave-data \
    --compression=none \
    "$@" \
    "${ICD_HOME?}"

