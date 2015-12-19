library(testthat)
library(devtools)
load_all()

#test_all_res <- test(reporter="list")
test_all_res <- test_package(package = "icd9", reporter = "list")
as.data.frame(test_all_res) -> resdf
tail(resdf[order(resdf$user), ])

