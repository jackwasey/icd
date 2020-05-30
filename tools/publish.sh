#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

#TODO: use rsync --delete? but keep robots.txt

# tinytex has to be able to find its texlive root and can get confused.
#PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

ICD_HOME=${ICD_HOME:-"$HOME/icd"}
GH_PAGES="$HOME/rprojects/icd-gh-pages"

cd "$ICD_HOME" || { echo "cannot cd to $ICD_HOME" >&2; exit 1; }

#Rscript --vanilla --default-packages=jwutil -e 'jwutil::reqinst("pkgdown"); pkgdown::build_site()'
Rscript --vanilla -e 'jwutil::reqinst(c("magick", "tinytex", "pkgdown")); codemetar::write_codemeta(); devtools::document(); pkgdown::build_site()'
mkdir -p "$GH_PAGES"
rsync -r --delete --filter='P .git' --filter='P .gitignore' "$ICD_HOME/docs/" "$GH_PAGES"
cd "$GH_PAGES" || { echo "cannot cd to ${GH_PAGES}"; exit 1; }
git status
git add -A
git commit -m "pkgdown rerun"
git push origin gh-pages

echo "Now sleep for github.io to rebuilt, then check links all work:"
# shellcheck disable=SC2034
for i in {1..60}; do
    echo -n '.'
    sleep 1
done
echo ""
rm -rf /tmp/github.io/*
mkdir -p /tmp/github.io
cd /tmp/github.io || { echo "cannot cd to temp download dir" >&2; exit 1; }
wget --mirror -p -o logfile.log https://jackwasey.github.io/icd/
set +eo pipefail
set +x
if grep -q -e '404' -e 'ERROR' logfile.log ; then
    grep --before-context=3 -e 404 /tmp/github.io/logfile.log  | grep http > badlinks.txt
    sed -i '/robots.txt/d' badlinks.txt
    declare -i nbad
    nbad=$(wc -l < badlinks.txt)
    if (( nbad != 0 )); then
        echo "Found ${nbad} link(s) on github.io site that are broken" >&2
    fi
fi
echo "No broken links found."
