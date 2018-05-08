#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

set -x

ICD_HOME=${ICD_HOME:-"$HOME/icd"}
GH_PAGES="$HOME/rprojects/icd-gh-pages"

pushd "$ICD_HOME"

function finish {
  popd
}
trap finish EXIT

#Rscript --vanilla --default-packages=jwutil -e 'jwutil::reqinst("pkgdown"); pkgdown::build_site()'
Rscript --vanilla -e 'jwutil::reqinst("pkgdown"); pkgdown::build_site()'
mkdir -p "$GH_PAGES"
cp -r docs/* "$GH_PAGES" 
git -C "$GH_PAGES" status
git commit -am "pkgdown rerun"
git -C "$GH_PAGES" push origin gh-pages

