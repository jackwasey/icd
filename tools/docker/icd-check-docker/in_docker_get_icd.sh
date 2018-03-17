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
git clone --depth=1 -b $GIT_BRANCH $GIT_URL

# the auto-generated Rcpp code is always changing order, if not content. Rstudio generates automatically, but we have to do manually here:
echo "Using github repo name '${GITHUB_REPO:-icd}'"
pushd $GITHUB_REPO
${R_CMD}script -e 'Rcpp::compileAttributes()'
popd

