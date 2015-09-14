#!/bin/bash

# run a thousand identical icd9 codes
# git pull && Rscript -e 'devtools::clean_dll(); devtools::load_all(); mydf <- data.frame(visitId = rep("a", 1000), icd9 = rep("441", 1000)); cmb <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE, applyHierarchy = TRUE)'

# run a single icd9 code
# git pull && Rscript -e 'devtools::clean_dll(); devtools::load_all(); mydf <- data.frame(visitId = "a", icd9 = "441"); cmb <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE, applyHierarchy = TRUE)'

# pull, build and stress, without docs for speed and to avoid premature errors
#if [ -d "icd9" ]; then
#  cd icd9
#fi
git --git-dir=icd9/.git pull
R CMD build --no-build-vignettes
R CMD INSTALL --no-docs --install-tests `ls -tr | grep icd9.*gz$ | tail -1`
Rscript -e 'library(icd9); x<-data.frame(visitId = "a", icd9 = "441"); icd9ComorbidQuanDeyo(x, isShort = F, applyHierarchy = T)'


# same but use openmp tests and devtools
cd icd9
git pull
Rscript -e "library(devtools); clean_dll(); load_all(); library(testthat); print(getwd()); test_file('tests/testthat/test-openmp.R')"

