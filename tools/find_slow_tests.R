library(testthat)
library(magrittr)
#test_all_res <- test(reporter="list")
test_all_res <- test_package(pkg="icd9", reporter="list")
test_all_res %>% as.data.frame -> resdf
resdf[order(resdf$user),] %>% tail

