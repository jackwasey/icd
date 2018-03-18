#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

pushd "${ICD_HOME:-$HOME/rprojects}"
R CMD build --no-build-vignettes --no-resave-data icd
R CMD INSTALL --install-tests --no-docs "$(ls -t /tmp/icd*.tar.gz | head -1)"
popd
