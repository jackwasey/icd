#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

R CMD build --compact-vignettes=gs+qpdf "$@" "${ICD_HOME:-$HOME/rprojects/icd}"

