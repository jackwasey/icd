#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

whereami="$(dirname "${BASH_SOURCE[0]}")"
#shellcheck source=find-icd-home.sh
source "${whereami}/find-icd-home.sh"
ICD_HOME=$(find_icd_home)

R CMD build \
    --log \
    --compact-vignettes=gs+qpdf \
    "$@" \
    "${ICD_HOME:?}"

