#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

#TODO: use rsync --delete

set -x
# tinytex has to be able to find its texlive root and can get confused.
#PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

ICD_HOME=${ICD_HOME:-"$HOME/rprojects/icd"}
GH_PAGES="$HOME/rprojects/icd-gh-pages"

pushd "$ICD_HOME"

#Rscript --vanilla --default-packages=jwutil -e 'jwutil::reqinst("pkgdown"); pkgdown::build_site()'
Rscript --vanilla -e 'jwutil::reqinst(c("magick", "tinytex", "pkgdown"); codemetar::write_codemeta(); devtools::document(); pkgdown::build_site()'
mkdir -p "$GH_PAGES"
rsync -r --delete --filter='P .git' --filter='P .gitignore' "$ICD_HOME/docs/" "$GH_PAGES"
popd
cd "$GH_PAGES" || { echo "cannot cd to ${GH_PAGES}"; exit 1; }
git status
git commit -am "pkgdown rerun"
git push origin gh-pages
