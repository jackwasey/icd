#!/bin/bash
set -uo pipefail
IFS=$'\n\t'
set -x
#https://stackoverflow.com/questions/14719349/error-c-stack-usage-is-too-close-to-the-limit#14719448
# C stack limit problems hopefully fixed by this. Can also be set to "unlimited" Default is 8192k on my pc.
# fails on mac
old_ulimit=$(ulimit -s)
ulimit -s unlimited || true
function finish {
        ulimit -s "$old_ulimit" || true
}
trap finish EXIT
echo "Working directory: ${ICD_HOME:=$HOME/icd}"
R_CMD_ERR="RD" R_CMD="RD" ${ICD_HOME}/tools/docker/rockicd.sh rocker/r-devel-ubsan-clang

