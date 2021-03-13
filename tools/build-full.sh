#!/usr/bin/env bash
set -eu

R CMD build \
    --log \
    --resave-data=xz \
    --compact-vignettes=gs+qpdf \
    "$@" \
    "${ICD_HOME?}"

# to shave 50k off icd10cm2019.rda:
#xz --decompress icd10cm2019.rda | xz --compress -9 --extreme --memlimit=1GiB > icd10cm2019.xz9e.rda
