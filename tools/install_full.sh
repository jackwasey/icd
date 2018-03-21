#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

pushd "$HOME/rprojects"
R CMD build --no-resave-data icd
R CMD INSTALL --install-tests "$(ls -t icd*.tar.gz | head -1)"
popd

