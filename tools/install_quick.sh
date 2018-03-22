#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

source ${ICD_HOME:$HOME/rprojects/icd}/tools/install_shared.sh

R CMD build --no-build-vignettes --no-resave-data icd
R CMD INSTALL -d \
  --no-clean-on-error \
  --library="$install_dir" \
  --install-tests \
  --no-docs \
  "$(ls -t icd*.tar.gz icd*.tgz | head -1)"
