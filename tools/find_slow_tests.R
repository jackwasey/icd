#!/usr/bin/r
library(testthat)
library(devtools)
load_all()

do_slow_tests = TRUE
do_online_tests = FALSE
test_all_res <- test_package(package = "icd9", reporter = "list")
as.data.frame(test_all_res) -> resdf
tail(resdf[order(resdf$user), ])

# Rscript tools/find_slow_tests.R
