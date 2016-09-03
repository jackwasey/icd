#!/usr/bin/r
library(testthat)
library(devtools)
load_all()

for (slow in c(FALSE, TRUE)) {
  icd:::do_slow_tests(slow)

  test_all_res <- test_package(package = "icd", reporter = "list", filter = NULL)
  as.data.frame(test_all_res) -> resdf
  print(tail(resdf[order(resdf$user), -c(1, 2)], n = 12))

  # Rscript tools/find_slow_tests.R
  slow_by_file <-  aggregate(real ~ file, data = resdf, FUN = sum)
  print(tail(slow_by_file[order(slow_by_file$real),]), 6)
}
