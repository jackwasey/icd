#!/usr/bin/env bash
set -eu
IFS=$'\n\t'

set -x
declare IH="${PWD}"
for d in "${PWD}" "${PWD}"/.. "${PWD}"/../..; do
[[ -e "${d}/DESCRIPTION" ]] && IH="${d}" && break
done

cd "${IH}" || { echo "cannot cd to ${IH}" >&2; exit 1; }

# quickly build the pacakge, and put it in the current directory
R CMD build \
    --log \
    --no-build-vignettes \
    --no-manual \
    --no-resave-data \
    "$@" \
    "${IH}"
