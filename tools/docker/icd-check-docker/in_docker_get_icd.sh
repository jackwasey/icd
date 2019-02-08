#!/bin/bash
set -euo pipefail
IFS=$'\n\t'
set -x
# or download zip from: https://github.com/jackwasey/icd/archive/master.zip
echo "Downloading/cloning '${GIT_BRANCH:-omp-taskloop}' branch from '${GIT_URL:-https://github.com/jackwasey/icd.git}'"
#git clone --depth=1 -b $GIT_BRANCH $GIT_URL
echo "Using github repo name '${GITHUB_REPO:-icd}'"
wget "https://github.com/jackwasey/$GITHUB_REPO/archive/$GIT_BRANCH.zip"
# if the branch is of the format v1.2.3 then the zip contains a directory called icd-1.2.3
tempdest=$(mktemp -d)
unzip -d "$tempdest" "$GIT_BRANCH.zip"
#zipdir=$(ls -r "$GITHUB_REPO")
zipdir=("$tempdest"/*)
mkdir -p "$GITHUB_REPO"
ls "$zipdir"
ls "$GITHUB_REPO"
ls
rm -rf "$GITHUB_REPO"
mv "$zipdir" "$GITHUB_REPO"
rmdir "$tempdest"


ls "$GITHUB_REPO"
# the auto-generated Rcpp code is always changing order, if not content. Rstudio generates automatically, but we have to do manually here:
pushd "$GITHUB_REPO"
# "${R_CMD}script" -e 'Rcpp::compileAttributes()'
popd
