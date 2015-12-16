library(testthat)
library(magrittr)
library(devtools)
load_all()

#test_all_res <- test(reporter="list")
test_all_res <- test_package(package = "icd9", reporter = "list")
test_all_res %>% as.data.frame -> resdf
resdf[order(resdf$user),] %>% tail

