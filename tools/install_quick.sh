#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

set -x

icd_install_tmp=$(mktemp -d)

function finish {
        rm -rf "$icd_install_tmp"
        popd
}
trap finish EXIT

cp -r "${ICD_HOME:-$HOME/rprojects/icd}" "$icd_install_tmp"
pushd "$icd_install_tmp"
mkdir "install"
R CMD build --no-build-vignettes --no-resave-data icd
R CMD INSTALL -d --library="install" --install-tests --no-docs "$(ls -t icd*.tar.gz | head -1)"
