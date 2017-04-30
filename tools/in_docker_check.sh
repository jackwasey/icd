#!/bin/bash
# Copyright (C) 2014 - 2017  Jack O. Wasey
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

# echo expanded commands
set -x

echo "Cloning '$GIT_BRANCH' branch from '$GIT_URL'"
pushd /tmp

echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections
apt-get update -qq || true
apt-get upgrade -qq -y || true
apt-get install -y -qq libssl-dev libxml2-dev curl libcurl4-openssl-dev unixodbc-dev qpdf pandoc pandoc-citeproc # libssh2-1-dev (optional for git but has debian version problem at least in April 2017)
# install debian packaged R packages to avoid compiling. may NEED to compile if USBAN or different compiler?
apt-get install -y -qq r-cran-rodbc r-cran-rcpp r-cran-knitr r-cran-testthat r-cran-checkmate r-cran-xml2

# check whether git exists (it may not e.g. in rocker/r-devel)
# https://stackoverflow.com/questions/592620/check-if-a-program-exists-from-a-bash-script

if hash git 2>/dev/null; then
  echo "git found"
else
  apt-get update
  apt-get install -y -qq git
fi

git clone --depth=1 -b $GIT_BRANCH $GIT_URL

# shorter if pre-installed debian packages, but these are not seen by RD as it has a different library?
if [[ "${R_CMD}" =~ .*RD$ ]]; then
  $R_CMD -e 'install.packages(c("knitr", "Rcpp", "testthat", "checkmate", "RODBC", "xml2"))'
fi
# these two aren't on debian anyway:
$R_CMD -e 'install.packages(c("roxygen2", "rmarkdown"))'

# actually, we need to build based on the directory name, not the package name:
$R_CMD CMD build $GITHUB_REPO # --no-build-vignettes (without build, errors more visible at install step)
R_PKG_TAR_GZ=$(ls -t ${R_PKG_NAME}*.tar.gz | tail -1)
# for all the flags
# https://cran.r-project.org/doc/manuals/r-release/R-ints.html
# not sure what happens if I us --as-cran and turn off a feature: seems like qpdf is still sought.
_R_CHECK_DOC_SIZES_=false _R_CHECK_CRAN_INCOMING_=false _R_CHECK_FORCE_SUGGESTS_=false $R_CMD CMD check --as-cran $R_PKG_TAR_GZ
popd
