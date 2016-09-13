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

echo "Cloning '$GIT_BRANCH' branch from '$GIT_URL'"
pushd /tmp
git clone --depth=1 -b $GIT_BRANCH $GIT_URL

# actually, we need to build based on the directory name, not the package name:
CXXFLAGS="-O0 -Wall -Wno-unused" $R_CMD CMD build $GITHUB_REPO # --no-build-vignettes (without build, errors more visible at install step)
R_PKG_TAR_GZ=$(ls -t $R_PKG_NAME*tar.gz | tail -1)
_R_CHECK_CRAN_INCOMING_=false $R_CMD CMD check --as-cran $R_PKG_TAR_GZ
popd
