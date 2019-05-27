#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

#TODO: use rsync --delete

set -x

ICD_HOME=${ICD_HOME:-"$HOME/rprojects/icd"}
GH_PAGES="$HOME/rprojects/icd-gh-pages"

pushd "$ICD_HOME"

function finish {
  popd
}
trap finish EXIT

#Rscript --vanilla --default-packages=jwutil -e 'jwutil::reqinst("pkgdown"); pkgdown::build_site()'
Rscript --vanilla -e 'jwutil::reqinst("pkgdown"); devtools::document(); pkgdown::build_site()'
mkdir -p "$GH_PAGES"
rsync -r --delete --filter='P .git' --filter='P .gitignore' "$ICD_HOME/docs/" "$GH_PAGES"
popd
pushd "$GH_PAGES"
git status
git commit -am "pkgdown rerun"
git push origin gh-pages

