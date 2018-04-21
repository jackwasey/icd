library(icd)
requireNamespace("comorbidity")
requireNamespace("medicalrisk")
requireNamespace("R.cache")

get_ten_million_icd9_pts <- function() {
  ten_million_random_pts <- R.cache::loadCache(key = list("ten_million_random_pts"), suffix = "icd.Rcache")
  if (is.null(ten_million_random_pts)) {
    ten_million_random_pts <- generate_random_pts(1e7)
    R.cache::saveCache(ten_million_random_pts, key = list("ten_million_random_pts"), suffix = "icd.Rcache")
  }
  ten_million_random_pts
}

bench_versus_others <- function() {
  set.seed(43)
  ten_million_random_pts <- get_ten_million_icd9_pts()
  bench_runs <- list("one" = c(n = 1, times = 1000),
                     "thousand" = c(n = 1e3, times = 100),
                     "ten thousand" = c(n = 1e4, times = 25),
                     "one hundred thousand" = c(n = 1e5, times = 10)
  )
  bench_res <- list()
  for (b_name in names(bench_runs)) {
    print(b_name)
    b <- bench_runs[[b_name]]
    print(b)
    n <- b["n"]
    times = b["times"]
    pts <- ten_million_random_pts[seq_len(n), ]
    # fixes for medicalrisk
    pts_mr <- pts
    names(pts_mr) <- c("id", "icd9cm")
    pts_mr$icd9cm <- paste("D", pts_mr$icd9cm, sep = "")
    mb <- microbenchmark::microbenchmark(
      icd::comorbid_charlson(pts, return_df = TRUE),
      comorbidity::comorbidity(x = pts, id = "visit_id", code = "code", score = "charlson_icd9"),
      comorbidity::comorbidity(x = pts, id = "visit_id", code = "code", score = "charlson_icd9", parallel = TRUE),
      medicalrisk::generate_comorbidity_df(pts_mr, icd9mapfn = medicalrisk::icd9cm_charlson_quan),
      times = times
    )
    bench_res[[b_name]] <- mb
  }
  lapply(bench_res, print)
  invisible(bench_res)
}

compare_versus_others <- function() {
  icd_res <- icd::comorbid_quan_deyo(thousand_pts, return_df = TRUE)
  comorbidity_res <- comorbidity::comorbidity(x = thousand_pts, id = "visit_id", code = "code", score = "charlson_icd9")
  comorbidity_res$visit_id <- as.character(comorbidity_res$visit_id)
  comorbidity_res <- icd:::binary_to_logical(comorbidity_res)[1:18]
  medicalrisk_res <- medicalrisk::generate_comorbidity_df(thousand_pts_mr, icd9mapfn = medicalrisk::icd9cm_charlson_quan)
  icd_res <- unname(icd_res)
  comorbidity_res <- unname(comorbidity_res)
  medicalrisk_res <- unname(medicalrisk_res)
  identical(icd_res, comorbidity_res)
  identical(icd_res, medicalrisk_res)
  identical(comorbidity_res, medicalrisk_res)
  # oh dear - all three are different
  colSums(icd_res[-1])
  colSums(medicalrisk_res[-1])
  colSums(comorbidity_res[-1])

  # differences are in chronic pulmonary disease (6) and cancer (14)
  icd_res$visit_id[which(icd_res$Pulmonary != comorbidity_res$copd)] # two differences
  icd_res$visit_id[which(icd_res$Cancer != comorbidity_res$canc)] # seven different patients

  icd_res$visit_id[which(icd_res$Pulmonary != medicalrisk_res$chrnlung)] # two differences
  icd_res$visit_id[which(icd_res$Cancer != medicalrisk_res$tumor)] # just one difference

  icd_res$visit_id[which(comorbidity_res$copd != medicalrisk_res$chrnlung)] # agreement
  icd_res$visit_id[which(comorbidity_res$canc != medicalrisk_res$tumor)] # six differ

  # patients "5", "35", "39" appear most
  bad_pts <- thousand_pts[thousand_pts$visit_id %in% c("5", "35", "39"), c("visit_id", "code")]

  pt_39_codes <- bad_pts[bad_pts$visit_id == "39", "code"]
  # look for where chronic lung disease might be hiding
  explain(icd:::icd_get_major.icd9(pt_39_codes, short_code = TRUE), warn = FALSE)
}

j <- bench_versus_others()
