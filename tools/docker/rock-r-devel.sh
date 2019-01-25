#!/bin/bash
set -euo pipefail
IFS=$'\n\t'
set -x
echo "Working directory: ${ICD_HOME:=$HOME/icd}"
R_CMD="RD" ${ICD_HOME}/tools/docker/rockicd.sh rocker/r-devel
