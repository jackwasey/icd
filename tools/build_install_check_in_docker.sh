#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# Copyright (C) 2014 - 2016  Jack O. Wasey
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

# This file should be run within the container. It is placed there by rocker-icd9.sh

: ${ICD_PROJECT_NAME:=icd}
: ${R_PKG_NAME:=$ICD_PROJECT_NAME}
: ${GITHUB_URL:=https://github.com}
: ${GITHUB_USER:=jackwasey}
: ${GITHUB_REPO:=$ICD_PROJECT_NAME}
: ${GIT_BRANCH:=master}
: ${GIT_URL:=$GITHUB_URL/$GITHUB_USER/$GITHUB_REPO.git}
: ${R_CMD:=R}

pushd /tmp
echo "Cloning '$GIT_BRANCH' branch from '$GIT_URL'"
git clone -b $GIT_BRANCH $GIT_URL

# actually, we need to build based on the directory name, not the package name:
$R_CMD CMD build $GITHUB_REPO # --no-build-vignettes (without build, errors more visible at install step)
R_PKG_TAR_GZ=`ls -t $R_PKG_NAME*tar.gz | tail -1`
$R_CMD CMD INSTALL $R_PKG_TAR_GZ
$R_CMD CMD check --as-cran $R_PKG_TAR_GZ
# potentially just do testthat tests and run examples instead of full package check?

popd
