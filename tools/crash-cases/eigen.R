library(icd)
library(testthat)
library(devtools)
library(magrittr)
requireNamespace("R.cache")
ten_million_random_pts <- R.cache::loadCache(key = list("ten_million_random_pts"), suffix = "icd.Rcache")
devnull2 <- replicate(100L, devnull <- icd_comorbid_ahrq(ten_million_random_pts, preclean = FALSE))

