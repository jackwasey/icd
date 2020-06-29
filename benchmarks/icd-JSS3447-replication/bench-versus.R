if (!file.exists("install-dependencies.R")) {
  message("not running in the benchmark replication directory")
  if (file.exists(file.path(
    "benchmarks",
    "icd-JSS3447-replication",
    "install-dependencies.R"
  ))) {
    message("setting directory to benchmark replication directory")
    setwd(file.path("benchmarks", "icd-JSS3447-replication"))
  } else {
    stop("cannot find benchmark replication directory")
  }
}
source("install-dependencies.R")
suppressPackageStartupMessages({
  library(icd, warn.conflicts = TRUE, quietly = TRUE)
  library(medicalrisk, warn.conflicts = TRUE, quietly = TRUE)
  library(comorbidity, warn.conflicts = TRUE, quietly = TRUE)
})

# the following control default behavior
# n is total number of rows of data
# dz_per_pt is number of disease codes per patient
# total number of patients is n / dz_per_pt
#
# N.b., changing these numbers will interfere with Makefile knowing what to do.
n_order_default <- 3L
n_order_big_default <- 6L # cut-off for only doing one iteration
n_order_prl_cmb <- 5L # best cut-off found for using 'parallel = TRUE' for 'comorbidity' package call
n_order_max_non_icd <- 7L # only 'icd' completes 10^8 or more iterations in a reasonable time.
dz_per_pt <- 20L
icd_ver <- "9"

have_colours <- function() {
  if (requireNamespace("crayon", quietly = TRUE)) return(crayon::has_color())
  if (getOption("crayon.enabled", default = FALSE)) return(TRUE)
  if ("COLORTERM" %in% names(Sys.getenv())) return(TRUE)
  if (Sys.getenv("TERM") == "dumb") return(FALSE)
  grepl("^screen|^xterm|^vt100|color|ansi|cygwin|linux", Sys.getenv("TERM"),
        ignore.case = TRUE, perl = TRUE)

}

bold_green <- "\\033[1m\033[32mJACK\033[39m\033[22m"
if (requireNamespace("crayon", quietly = TRUE)) {
  green <- crayon::green
  bold <- crayon::bold
} else if (have_colours())
sprintf("\b\n", bold("Setting the following defaults"))
cat(paste(bold(names(n)), bold(green(n)), collapse="\n"))

#n_order <- n_order_default
#n_order_big <- n_order_big_default # cut-off for only doing one iteration

if (icd_ver == "9") {
  icd_code_src <- icd::icd9cm_hierarchy$code
}
if (icd_ver == "10") {
  icd_code_src <- icd::icd10cm2019$code
}
empty_res <- data.frame(
  datarows = integer(),
  icd = numeric(),
  comorbidity = numeric(),
  medicalrisk = numeric()
)

# r-lib/bench package does memory profiling and garbage collection analysis,
# which themselves use a lot of memory, and cause out-of-memory process aborts
# with 32GB RAM. Therefore, since running time is so long for comorbidity and
# medicalrisk, I will just time these expression for one execution with large
# iteration counts.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) {
  stop(
    "Only one argument is accepted, which is the order ",
    "of magitude of the biggest benchmark"
  )
}
if (length(args) == 1L) {
  message("As no order of magnitude argument was given, using default of ", n_order_default)
  n_order_default <- as.integer(args[1])
}
message(
  "Running benchmarks, with biggest synthetic data set having 10^",
  n_order_default, " rows."
)
if (n_order_default >= n_order_big) {
  warning(
    "Depending on hardware, running these benchmarks with 10^6 or more",
    " rows may take hours."
  )
}

get_pts <- function(n = 3L, dz_per_pt = 20) {
  set.seed(1441)
  diags <- sample(icd_code_src, size = n, replace = TRUE)
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

bench_press_small <- function(n_order = n_order_default) {
  ns <- 10L^(1L:min(n_order, n_order_big - 1L))
  bench::press(n = ns, {
    pts <- get_pts(n, dz_per_pt = dz_per_pt)
    pts_mr <- medicalrisk_fix(pts)
    # the output is saved to result, and is potentially gigantic, so use local
    bench::mark(
      icd = local(comorbid_charlson(pts)),
      comorbidity = local(comorbidity(
        x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
        parallel = n >= 10L^n_order_prl_cmb
      )),
      medicalrisk = local(generate_comorbidity_df(
        pts_mr,
        icd9mapfn = icd9cm_charlson_quan
      )),
      filter_gc = TRUE,
      check = FALSE
    )
  })
}

bench_small <- function(n_order = n_order_default) {
  bres_tbl <- bench_press_small(n_order = n_order)
  bref <- bres_tbl[c("expression", "n", "median")]
  bref[["median"]] <- as.numeric(bref[["median"]])
  bres <- as.data.frame(bref)
  names(bres)[which(names(bres) == "n")] <- "datarows"
  bres$pkg <- names(bres$expression)
  bres$expression <- NULL
  # now take the medians and make suitable for the article:
  res <- reshape(data = bres, idvar = "datarows", direction = "wide", timevar = "pkg")
  names(res) <- sub("median\\.", "", names(res))
  rownames(res) <- NULL
  # ensure name order is consistent
  res[c("datarows", "icd", "comorbidity", "medicalrisk")]
}

time_big <- function(n_order = n_order_default, n_order_big = n_order_big_default) {
  n <- 10L^(n_order_big:n_order)
  res <- empty_res
  if (n_order < n_order_big) {
    return(res)
  }
  message("Running with bigger data")
  for (nit in seq_along(n)) {
    message(n[nit])
    pts <- get_pts(n[nit], dz_per_pt = dz_per_pt)
    pts_mr <- medicalrisk_fix(pts)
    message("Warming up with single runs...")
    message("First warm-up will get the dat hot,
          no matter which package first.")
    warmup <- system.time(icd::comorbid_charlson(pts))["elapsed"]
    message("icd took ", warmup, " seconds.")
    warmup <- system.time(comorbidity::comorbidity(
      x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
      parallel = TRUE
    ))["elapsed"]
    message("comorbidity warmup took ", warmup, " seconds.")
    warmup <- system.time(medicalrisk::generate_comorbidity_df(
      pts_mr,
      icd9mapfn = medicalrisk::icd9cm_charlson_quan
    ))["elapsed"]
    message("medicalrisk warmup took ", warmup, " seconds.")
    # now actually benchmark
    res[nit, ] <- c(
      n[nit],
      system.time(comorbid_charlson(pts))["elapsed"],
      system.time(comorbidity(
        x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
        parallel = TRUE
      ))["elapsed"],
      system.time(generate_comorbidity_df(
        pts_mr,
        icd9mapfn = medicalrisk::icd9cm_charlson_quan
      ))["elapsed"]
    )
  }
  res
}

time_extra_big <- function(n_order = n_order_default) {
  n <- 10L^(n_order_big:n_order)
  if (n_order < n_order_big) {
    return(empty_res)
  }
  message("Running with bigger data, from 10^", n_order, " to ", n_order_big)
  message(appendLF = FALSE, "Warming up with single run... ")
  warmup <- system.time(icd::comorbid_charlson(pts))["elapsed"]
  message("that took ", warmup, " seconds.")
  res <- empty_res
  for (nit in seq_along(n)) {
    message(n[nit])
    pts <- get_pts(n[nit], dz_per_pt = dz_per_pt)
    pts_mr <- medicalrisk_fix(pts)
    res[nit, ] <- c(
      n[nit],
      system.time(icd::comorbid_charlson(pts))["elapsed"],
      NA,
      NA
    )
  }
  res
}

get_bench_filename <- function(prefix, suffix, use_date = FALSE,
                               n_order = n_order_default,
                               results_dir = "results") {
  fn <- paste0(
    paste(prefix, "n", n_order, "dz", dz_per_pt,
      Sys.info()["nodename"], ifelse(use_date, Sys.Date(), ""),
      sep = "-"
    ),
    ".", suffix
  )
  file.path(results_dir, fn)
}

get_bench_short_filename <- function(prefix, suffix,
                                     n_order = n_order_default,
                                     results_dir = "results") {
  fn <- paste0(
    paste(prefix, "n", n_order,
      sep = "-"
    ),
    ".", suffix
  )
  file.path(results_dir, fn)
}

# combine small and big benchmark results, and deal with tibble/bench S3 problems
bench_versus <- function(n_order = n_order_default) {

  # now add the timings for the very long-running computations (which bench only does
  # once anyway, and which are dominated by the computations themselves)
  rbind(
    bench_small(n_order),
    time_big(n_order),
    time_extra_big(n_order)
  )
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
