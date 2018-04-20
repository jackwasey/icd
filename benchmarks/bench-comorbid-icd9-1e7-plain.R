# generate a bunch of random patients and get comorbidities, intended for valgrind usage
library(icd)
library(microbenchmark)
library(magrittr)
library(R.cache)
library(profvis)
ten_million_random_pts <- R.cache::loadCache(key = list("ten_million_random_pts"), suffix = "icd.Rcache")
if (is.null(ten_million_random_pts)) {
  ten_million_random_pts <- icd:::generate_random_pts(1e7)
  R.cache::saveCache(ten_million_random_pts, key = list("ten_million_random_pts"), suffix = "icd.Rcache")
}
comorbid_ahrq(ten_million_random_pts, preclean = FALSE)

