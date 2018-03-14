#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

pushd "$HOME/rprojects"
R CMD build --no-resave-data icd
R CMD INSTALL --install-tests "$(ls -t /tmp/icd*.tar.gz | head -1)"
popd

