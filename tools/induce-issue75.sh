#!/bin/bash
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

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

# run a thousand identical icd9 codes
# git pull && Rscript -e 'devtools::clean_dll(); devtools::load_all(); mydf <- data.frame(visitId = rep("a", 1000), icd9 = rep("441", 1000)); cmb <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE, applyHierarchy = TRUE)'

# run a single icd9 code
# git pull && Rscript -e 'devtools::clean_dll(); devtools::load_all(); mydf <- data.frame(visitId = "a", icd9 = "441"); cmb <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE, applyHierarchy = TRUE)'

# pull, build and stress, without docs for speed and to avoid premature errors
#if [ -d "icd9" ]; then
#  cd icd9
#fi
git --git-dir=icd9/.git pull \
&& R CMD build --no-build-vignettes \
&& R CMD INSTALL --no-docs --install-tests `ls -tr | grep icd9.*gz$ | tail -1` \
&& Rscript -e 'library(icd); icd9ComorbidQuanDeyo(data.frame(visitId = "a", icd9 = "441"), isShort = F, applyHierarchy = T)'


# same but use openmp tests and devtools
cd icd9
git pull
Rscript -e "library(devtools); clean_dll(); load_all(); library(testthat); print(getwd()); test_file('tests/testthat/test-openmp.R')"

