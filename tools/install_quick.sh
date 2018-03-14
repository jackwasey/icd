#!/bin/bash

# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

tmpd=$(mktemp -d /tmp/icdinstall.XXXXXXXXXXX)
function finish {
  rm -rf "$tmpd"
  echo "Not cleaning $tmpd"
}
trap finish EXIT

cp -r "${ICD_HOME:-$HOME/rprojects/icd}" "$tmpd"
pushd "$tmpd"

R CMD build --no-build-vignettes --no-resave-data icd
R CMD INSTALL icd*.tar.gz
popd
