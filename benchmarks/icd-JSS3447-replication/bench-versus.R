if (!file.exists("install-dependencies.R")) {
  message("not running in the benchmark replication directory")
  if (file.exists(file.path("benchmarks",
                        "icd-JSS3447-replication",
                        "install-dependencies.R"))) {
    message("setting directory to benchmark replication directory")
      setwd(file.path("benchmarks", "icd-JSS3447-replication"))
  } else {
    stop("cannot find benchmark replication directory")
  }
}
source("install-dependencies.R")

# the following control default behavior
# n is total number of rows of data
# dz_per_pt is number of disease codes per patient
# total number of patients is n / dz_per_pt
#
# N.b., changing these numbers will interfere with Makefile knowing what to do.
n_order_default <- 3L
n_order_big = 6L # cut-off for only doing one iteration
dz_per_pt <- 20L

# r-lib/bench package does memory profiling and garbage collection analysis,
# which themselves use a lot of memory, and cause out-of-memory process aborts
# with 32GB RAM. Therefore, since running time is so long for comorbidity and
# medicalrisk, I will just time these expression for one execution with large
# iteration counts.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("Only one argument is accepted, which is the order",
                            " of magitude of the biggest benchmark")
if (length(args) == 1L)
  n_order_default <- as.integer(args[1])
message("Running benchmarks, with biggest synthetic data set having 10^",
        n_order_default, " rows.")
if (n_order_default > 5L)
  warning("Depending on hardware, running these benchmarks with 10^6 or more",
          " rows may take hours.")

get_pts <- function(n = 1e6, dz_per_pt = 20) {
  set.seed(1441)
  diags <- sample(icd.data::icd9cm_hierarchy$code, size = n, replace = TRUE)
  data.frame(
    visit_id = as.character(floor(seq_len(n) / dz_per_pt)),
    code = diags,
    stringsAsFactors = FALSE
  )
}

medicalrisk_fix <- function(pts_mr) {
  names(pts_mr) <- c("id", "icd9cm")
  pts_mr$icd9cm <- paste("D", pts_mr$icd9cm, sep = "")
  pts_mr
}

bench_small <- function(n_order = n_order_default) {
  n = 10L^(1L:min(n_order, n_order_big - 1))
  bench::press(n = n, {
    pts <- get_pts(n, dz_per_pt = dz_per_pt)
    pts_mr <- medicalrisk_fix(pts)
    # the output is saved to result, and is potentially gigantic, so use local
    bench::mark(
      local(icd::comorbid_charlson(pts)),
      local(comorbidity::comorbidity(
        x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
        parallel = n >= 1e5)),
      local(medicalrisk::generate_comorbidity_df(
        pts_mr, icd9mapfn = medicalrisk::icd9cm_charlson_quan)),
      filter_gc = TRUE,
      check = FALSE
    )
  })
}

time_big <- function(n_order = n_order_default) {
  n = 10L^(n_order_big:n_order)
  res <- data.frame(datarows = integer(),
                    icd = numeric(),
                    comorbidity = numeric(),
                    medicalrisk = numeric())
  if (n_order < n_order_big) return(res)
  message("Running one iteration with:")
  for (nit in seq_along(n)) {
    message(n[nit])
    pts <- get_pts(n[nit], dz_per_pt = dz_per_pt)
    pts_mr <- medicalrisk_fix(pts)
    res[nit,] <- c(
      n[nit],
      system.time(icd::comorbid_charlson(pts))["elapsed"],
      system.time(comorbidity::comorbidity(
        x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
        parallel = TRUE))["elapsed"],
      system.time(medicalrisk::generate_comorbidity_df(
        pts_mr, icd9mapfn = medicalrisk::icd9cm_charlson_quan))["elapsed"]
    )
  }
  res
}

get_bench_filename <- function(prefix, suffix, use_date = FALSE,
                               n_order = n_order_default) {
  paste0(
    paste(prefix, "n", n_order, "dz", dz_per_pt,
          Sys.info()["nodename"], ifelse(use_date, Sys.Date(), ""), sep = "-"),
    ".", suffix)
}

get_bench_short_filename <- function(prefix, suffix,
                                     n_order = n_order_default) {
  paste0(
    paste(prefix, "n", n_order, # "dz", dz_per_pt,
          sep = "-"),
    ".", suffix)
}

bench_versus <- function(n_order = n_order_default) {
  bres <- bench_small(n_order)
  # now take the medians and make suitable for the article:
  res <- tidyr::spread(bres[c("expression", "n", "median")], expression, median)
  # name order is not deterministic!
  names(res) <- c("datarows",
                  sub("local\\(([^:]*).*", "\\1", names(res)[-1]))
  res <- res[c("datarows", "icd", "comorbidity", "medicalrisk")]
  res$icd <- as.numeric(res$icd)
  res$comorbidity <- as.numeric(res$comorbidity)
  res$medicalrisk <- as.numeric(res$medicalrisk)
  res <- as.data.frame(res)
  # add the timings for the very long-running computations (which bench only does
  # once anyway, and which are dominated by the computations themselves)
  rbind(res, time_big(n_order))
}

res <- bench_versus()

# work around an older R version abbreviating dput output in some R versions
old_opt_dml <- options(deparse.max.lines = 0)
# keep file name the same so Makefile will keep track, i.e. not dated, but
# unique for machine and benchmark
dput(res, get_bench_short_filename("dput-latest", "R"))
# and a dated version
dput(res, get_bench_filename("dput-dated", "R", use_date = TRUE))
options(old_opt_dml)
