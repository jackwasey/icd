#!/usr/bin/env bash
set -eu
IFS=$'\n\t'

ICD_HOME="${ICD_HOME:-${HOME}/icd}"

R CMD build \
    --log \
    --resave-data=xz \
    --compact-vignettes=gs+qpdf \
    "$@" \
    "${ICD_HOME}"

