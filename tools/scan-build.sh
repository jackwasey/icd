#!/bin/bash
set -euo pipefail
IFS=$'\n\t'
tmpd=$(mktemp -d /tmp/icdscanbuild.XXXXXXXXXXX)
function finish {
#	  rm -rf "$tmpd"
  echo "Finished with $tmpd"
}
trap finish EXIT
cp -r "${ICD_HOME:-$HOME/rprojects/icd}" "$tmpd"
pushd "$tmpd"
mkdir "library"
# or try --configure-args="CXXFLAGS=-O0"
R_MAKEVARS_USER="$HOME/.R/Makevars.scan-build" \
  MAKEFLAGS="-j1" \
  R CMD INSTALL \
    --build \
    -d \
    --no-byte-compile \
    --no-clean-on-error \
    --library="$tmpd/library" \
    --install-tests \
    --no-docs \
    --no-data \
    icd
popd
