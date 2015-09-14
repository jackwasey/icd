#!/bin/sh

# run a thousand identical icd9 codes
Rscript -e 'devtools::clean_dll(); devtools::load_all(); mydf <- data.frame(visitId = rep("a", 1000), icd9 = rep("441", 1000)); cmb <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE, applyHierarchy = TRUE)'

# run a single icd9 code
Rscript -e 'devtools::clean_dll(); devtools::load_all(); mydf <- data.frame(visitId = "a", icd9 = "441"); cmb <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE, applyHierarchy = TRUE)'

# 
R CMD build --no-build-vignettes && R CMD INSTALL `ls -tr | grep gz$ | tail -1` && Rscript -e 'library(icd9); mydf <- data.frame(visitId = rep("a", 1000), icd9 = rep("441", 1000)); cmb <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE, applyHierarchy = TRUE)'

