#!/usr/bin/r
library(testthat)
library(devtools)
load_all()

filter = NULL

options("icd.do_slow_tests" = TRUE)
test_all_res <- test_package(package = "icd", reporter = "list", filter = filter)
as.data.frame(test_all_res) -> resdf
print(tail(resdf[order(resdf$user), ]))

# Rscript tools/find_slow_tests.R
slow_by_file <-  aggregate(real ~ file, data = resdf, FUN = sum)
print(slow_by_file)
