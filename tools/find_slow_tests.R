#!/usr/bin/r
library(testthat)
library(devtools)
load_all()

test_all_res <- test_package(package = "icd", reporter = "list", filter = NULL)
as.data.frame(test_all_res) -> resdf
print(tail(resdf[order(resdf$user), ], n = 12L))

# Rscript tools/find_slow_tests.R
slow_by_file <-  aggregate(real ~ file, data = resdf, FUN = sum)
print(slow_by_file[order(slow_by_file$real),])
