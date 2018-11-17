#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

R CMD build --compact-vignettes=qpdf "$@" "${ICD_HOME:-$HOME/rprojects/icd}"

