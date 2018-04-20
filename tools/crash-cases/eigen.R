library(icd)
library(testthat)
library(devtools)
library(magrittr)
library(R.cache)
ten_million_random_pts <- loadCache(key = list("ten_million_random_pts"), suffix = "icd.Rcache")
devnull2 <- replicate(100L, devnull <- icd_comorbid_ahrq(ten_million_random_pts, preclean = FALSE))

