#!/bin/bash

# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

# This file should be run within the container. It is placed there by rocker-icd9.sh

: ${R_PKG_NAME:=icd9}
: ${GIT_URL:=https://github.com}
: ${GITHUB_USER:=jackwasey}
: ${GITHUB_REPO:=$R_PKG_NAME}
: ${GIT_BRANCH:=master}
: ${R_CMD:=R}

cd /tmp
git clone -b $GIT_BRANCH $GIT_URL/$GITHUB_USER/$GITHUB_REPO.git
$R_CMD CMD build $R_PKG_NAME # --no-build-vignettes (without build, errors more visible at install step)
R_PKG_TAR_GZ=`ls -t /tmp/$R_PKG_NAME*tar.gz | tail -1`
$R_CMD CMD INSTALL $R_PKG_TAR_GZ
$R_CMD CMD check --as-cran $R_PKG_TAR_GZ
# potentially just do testthat tests and run examples instead of full package check?
