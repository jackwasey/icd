# generate a bunch of random patients and get comorbidities, intended for valgrind usage
library(icd)
library(microbenchmark)
library(magrittr)
library(R.cache)
library(profvis)

get_ten_million_icd9_pts <- function() {
  ten_million_random_pts <- R.cache::loadCache(key = list("ten_million_random_pts"), suffix = "icd.Rcache")
  if (is.null(ten_million_random_pts)) {
    ten_million_random_pts <- generate_random_pts(1e7)
    R.cache::saveCache(ten_million_random_pts, key = list("ten_million_random_pts"), suffix = "icd.Rcache")
  }
  ten_million_random_pts
}

ten_million_random_pts <- get_ten_million_icd9_pts()
# the following is a mix of repeated vermont patients and random patients,
# totally about 114,000,000 rows, which is about 3.5GB on disk in R.cache and
# 9GB in RAM.
huge_mixed_pts <- R.cache::loadCache(key = list("huge_mixed_pts"), suffix = "icd.Rcache")
if (is.null(huge_mixed_pts)) {
  vt <- wide_to_long(vermont_dx)[c("visit_id", "icd_code")]
  vts <- mefa:::rep.data.frame(vt, 10000)
  rnd <- ten_million_random_pts[c("visit_id", "code")]
  names(rnd) <- names(vts)
  huge_mixed_pts <- rbind(rnd, vts)
  huge_mixed_pts$visit_id <- icd:::as_char_no_warn(huge_mixed_pts$visit_id)
  rm(list = c("rnd", "vts", "vt"))
  R.cache::saveCache(huge_mixed_pts, key = list("huge_mixed_pts"), suffix = "icd.Rcache")
}

message("profiling!")
profvis(comorbid_ahrq(ten_million_random_pts, preclean = FALSE))

profvis::profvis(
  icd::comorbid_ahrq(huge_mixed_pts, preclean = FALSE)
)

message("benchmark starting!")
mb <- microbenchmark(
  comorbid(huge_mixed_pts, icd9_map_ahrq, comorbid_fun = icd:::comorbidMatMul),
  #  comorbid(huge_mixed_pts, icd9_map_ahrq, comorbid_fun = icd:::icd9Comorbid_alt_Taskloop),
  #  comorbid(huge_mixed_pts, icd9_map_ahrq, comorbid_fun = icd:::icd9Comorbid_alt_Taskloop2),
  comorbid(huge_mixed_pts, icd9_map_ahrq, comorbid_fun = icd:::icd9ComorbidShortCpp),
  times = 25L)

#  comorbid(pts, icd9_map_ahrq, comorbid_fun = icd:::icd9Comorbid_alt_SparseOmp),

print(mb)

big_icd10 <- do.call("rbind", replicate(250, uranium_pathology, simplify = FALSE))

# look at all icd-10 options at once
microbenchmark::microbenchmark(
  icd:::icd10_comorbid_reduce(
    uranium_pathology, icd10_map_ahrq,
    visit_name = "case", icd_name = "icd10",
    short_code = FALSE, short_map = TRUE, return_df = FALSE),
  icd:::icd10_comorbid_parent_search_use_cpp(
    uranium_pathology, icd10_map_ahrq,
    visit_name = "case", icd_name = "icd10",
    short_code = FALSE, short_map = TRUE, return_df = FALSE),
  check = icd:::all_identical, times = 30)
# The following are all much slower by 1-2 orders of magnitude

# icd:::icd10_comorbid_parent_search_str(
#   uranium_pathology, icd10_map_ahrq,
#   visit_name = "case", icd_name = "icd10",
#   short_code = FALSE, short_map = TRUE, return_df = FALSE),
# icd:::icd10_comorbid_parent_search_orig(
#   uranium_pathology, icd10_map_ahrq,
#   visit_name = "case", icd_name = "icd10",
#   short_code = FALSE, short_map = TRUE, return_df = FALSE),
# icd:::icd10_comorbid_parent_search_all(
#   uranium_pathology, icd10_map_ahrq,
#   visit_name = "case", icd_name = "icd10",
#   short_code = FALSE, short_map = TRUE, return_df = FALSE),
# icd:::icd10_comorbid_parent_search_no_loop(
#   uranium_pathology, icd10_map_ahrq,
#   visit_name = "case", icd_name = "icd10",
#   short_code = FALSE, short_map = TRUE, return_df = FALSE),

# Found tight confidence intervals on reduce method being 1-2x as fast as cpp parent
# search for small (uranium) data

microbenchmark::microbenchmark(
  icd:::icd10_comorbid_reduce(
    big_icd10, icd10_map_ahrq,
    visit_name = "case", icd_name = "icd10",
    short_code = FALSE, short_map = TRUE, return_df = FALSE),
  icd:::icd10_comorbid_parent_search_use_cpp(
    big_icd10, icd10_map_ahrq,
    visit_name = "case", icd_name = "icd10",
    short_code = FALSE, short_map = TRUE, return_df = FALSE),
  identical = icd:::my_check, times = 10)

# reduce method 1.5-2 orders of magnitude faster!


comorbid_ahrq(vermont_dx %>% wide_to_long, comorbid_fun = icd:::icd9ComorbidShortCpp)

# to test Eigen sparse calcs, remove _alt line in .Rbuildignore, then these will be available.
# Also, re-enable [[Rcpp::depends(RcppEigen)]]
microbenchmark::microbenchmark(
  comorbid_ahrq(vermont_dx %>% wide_to_long, comorbid_fun = icd:::comorbidMatMul),
  comorbid_ahrq(vermont_dx %>% wide_to_long, comorbid_fun = icd:::icd9ComorbidShortCpp),
  times = 25)




vermont_dx %>% wide_to_long() -> vt
microbenchmark::microbenchmark(
  res1 <- comorbid(vt, icd9_map_ahrq, comorbid_fun = icd:::icd9ComorbidShortCpp),
  res2 <- comorbid(vt, icd9_map_ahrq, comorbid_fun = icd:::icd9Comorbid_alt_Taskloop),
  times = 50)
identical(res1, res2)


#' \dontrun{
#' microbenchmark::microbenchmark(
#'    substr("12345", 1, 4),
#'    substring("12345", 1, 4),
#'    stringr::str_sub("12345", 1, 4), times = 1e4)
#' # substr is fastest by a good margin
#' }

