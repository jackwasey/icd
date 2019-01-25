#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# or download zip from: https://github.com/jackwasey/icd/archive/master.zip
echo "Cloning '${GIT_BRANCH:-omp-taskloop}' branch from '${GIT_URL:-https://github.com/jackwasey/icd.git}'"
git clone --depth=1 -b $GIT_BRANCH $GIT_URL

# the auto-generated Rcpp code is always changing order, if not content. Rstudio generates automatically, but we have to do manually here:
echo "Using github repo name '${GITHUB_REPO:-icd}'"
pushd $GITHUB_REPO
${R_CMD}script -e 'Rcpp::compileAttributes()'
popd
