#!/bin/bash

#pushd /tmp
#R CMD build ~/rprojects/icd
# set ICD_DEBUG=1 in config.h
#R CMD build ~/rprojects/icd
#R CMD INSTALL --install-tests icd_*.tar.gz
#popd

debugger=lldb

R -d "$debugger" -e '
library(icd);
x <- readRDS(system.file("tests", "testthat", "github138-b.rds", package = "icd")); icd10_comorbid(x, icd10_map_ahrq, visit_name = "CLAIMNO", icd_name = "icd10",  aggregate = FALSE)
'

R -d "$debugger" -e '
pts10 <- icd::long_data(
     visit_name = c("a", "b", "c", "d", "e"),
     icd_name = c("I058", NA, "T82817A", "", "I69369"),
     date = as.Date(c("2011-01-01", "2011-01-02", "2011-01-03", "2011-01-03", "2011-01-03")))

icd::icd10_comorbid(pts10, map = icd10_map_ahrq)'

R -d "$debugger" -e '
pts10 <- icd::long_data(
     visit_name = c("a"),
     icd_name = c(""),
     date = as.Date(c("2011-01-01")));
     icd::icd10_comorbid(pts10, map = icd::icd10_map_ahrq)
'
