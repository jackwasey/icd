library(icd)
requireNamespace("comorbidity")
requireNamespace("medicalrisk")
requireNamespace("R.cache")

get_ten_million_icd9_pts <- function() {
  ten_million_random_pts <-
    R.cache::loadCache(key = list("ten_million_random_pts"),
                       suffix = "icd.Rcache")
  if (is.null(ten_million_random_pts)) {
    ten_million_random_pts <- generate_random_pts(1e7)
    R.cache::saveCache(ten_million_random_pts,
                       key = list("ten_million_random_pts"),
                       suffix = "icd.Rcache")
  }
  invisible(ten_million_random_pts)
}

bench_n_vs_times <- function(start = 1, end = 5,
                             it_base = 1e4, it_max = 25) {
  stopifnot(nrow(ten_million_random_pts) >= 10^end)
  r <- seq(from = start, to = end)
  bench_times <- as.integer(it_base^(1/(r + 1)))
  bench_times[bench_times > it_max] <- it_max
  bench_n <- 10^r
  unname(
    as.list(
      as.data.frame(
        t(
          matrix(byrow = FALSE, ncol = 2,
                 data = c(bench_n, bench_times))))))
}

medicalrisk_fix <- function(pts_mr) {
  names(pts_mr) <- c("id", "icd9cm")
  pts_mr$icd9cm <- paste("D", pts_mr$icd9cm, sep = "")
  pts_mr
}

bench_versus_others <-
  function(do_slow = FALSE,
           end = 5,
           bench_runs =  bench_n_vs_times(end = end)) {
    set.seed(43)
    ten_million_random_pts <- get_ten_million_icd9_pts()
    bench_res <- list()
    for (b_name in seq_along(bench_runs)) {
      print(b_name)
      b <- bench_runs[[b_name]]
      n <- b[1]
      times = b[2]
      message("working on n = ", n, ", times = ", times)
      pts <- ten_million_random_pts[seq_len(n), ]
      pts_mr <- medicalrisk_fix(pts)
      mb <- microbenchmark::microbenchmark(
        icd::comorbid_charlson(pts, return_df = TRUE),
        medicalrisk::generate_comorbidity_df(
          pts_mr,
          icd9mapfn = medicalrisk::icd9cm_charlson_quan),
        times = times
      )
      # never use 'comorbidity' without parallel for huge calculations
      mb2 <- if (n < 5e4 && do_slow)
        microbenchmark::microbenchmark(
          comorbidity::comorbidity(
            x = pts, id = "visit_id",
            code = "code", score = "charlson_icd9",
            parallel = TRUE),
          times = times)
      else # if (n < cut_off)
        microbenchmark::microbenchmark(
          comorbidity::comorbidity(
            x = pts, id = "visit_id",
            code = "code", score = "charlson_icd9",
            parallel = FALSE),
          times = times)
      mb <- rbind(mb, mb2)
      print(mb)
      bench_res[[b_name]] <- mb
    }
    invisible(bench_res)
  }

j <- bench_versus_others(end = 5)
