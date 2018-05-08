#!/bin/bash
# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

set -euo pipefail
IFS=$'\n\t'

# or download zip from: https://github.com/jackwasey/icd/archive/master.zip
echo "Cloning '${GIT_BRANCH:-omp-taskloop}' branch from '${GIT_URL:-https://github.com/jackwasey/icd.git}'"
#git clone --depth=1 -b $GIT_BRANCH $GIT_URL
echo "Using github repo name '${GITHUB_REPO:-icd}'"
wget "https://github.com/jackwasey/$GITHUB_REPO/archive/$GIT_BRANCH.zip"
# if the branch is of the format v1.2.3 then the zip contains a directory called icd-1.2.3
unzip "$GIT_BRANCH.zip" -d "$GITHUB_REPO"
#zipdir=$(find "$GITHUB_REPO" -maxdepth 0 -type d)
zipdir=$(ls -r "$GITHUB_REPO")
mv "$GITHUB_REPO/$zipdir"/* "$GITHUB_REPO"
rmdir "$GITHUB_REPO/$zipdir"

# the auto-generated Rcpp code is always changing order, if not content. Rstudio generates automatically, but we have to do manually here:
pushd "$GITHUB_REPO"
"${R_CMD}script" -e 'Rcpp::compileAttributes()'
popd

