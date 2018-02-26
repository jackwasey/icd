#!/bin/bash

pushd /tmp
R CMD build ~/rprojects/icd
ICD_DEBUG=1 R CMD INSTALL --install-tests icd_*.tar.gz
popd

R -d gdb -e 'library(icd); x <- readRDS(system.file("tests", "testthat", "github138-b.rds", package = "icd")); icd10_comorbid(x, icd10_map_ahrq, visit_name = "CLAIMNO", icd_name = "icd10",  aggregate = FALSE)'

