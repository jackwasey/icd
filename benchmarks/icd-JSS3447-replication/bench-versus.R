source("install-dependencies.R")
# these control default behavior
# n is total number of rows of data
# dz_per_pt is number of disease codes per patient
# total number of patients is n / dz_per_pt
#
# N.b., changing these numbers will interfere with Makefile knowing what to do.
n_order <- 3L
dz_per_pt <- 20L

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("Only one argument is accepted, which is the order",
                            " of magitude of the biggest benchmark")
if (length(args) == 1L)
  n_order <- as.integer(args[1])
message("Running benchmarks, with biggest synthetic data set having 10^",
        n_order, " rows.")
if (n_order > 5L)
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

#ten_million_random_pts <- get_ten_million_icd9_pts()
#ten_million_random_pts <- get_pts(1e7, dz_per_pt = 20, use_cache = TRUE)
n <- 10L^(1L:n_order)
bres <- bench::press(n = n, {
  #pts <- ten_million_random_pts[seq_len(n), ]
  pts <- get_pts(n, dz_per_pt = dz_per_pt)
  pts_mr <- medicalrisk_fix(pts)
  bench::mark(
    icd::comorbid_charlson(pts),
    comorbidity::comorbidity(
      x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
      parallel = n >= 1e5),
    medicalrisk::generate_comorbidity_df(
      pts_mr, icd9mapfn = medicalrisk::icd9cm_charlson_quan),
    check = FALSE
  )
})

get_bench_filename <- function(prefix, suffix, use_date = FALSE) {
  paste0(
    paste(prefix, "n", n_order, "dz", dz_per_pt,
          Sys.info()["nodename"], ifelse(use_date, Sys.Date(), ""), sep = "-"),
    ".", suffix)
}

get_bench_short_filename <- function(prefix, suffix) {
  paste0(
    paste(prefix, "n", n_order, # "dz", dz_per_pt,
          sep = "-"),
    ".", suffix)
}

# keep the dated results
saveRDS(bres, get_bench_filename("bench-versus-result", "rds", use_date = TRUE))

# now take the medians and make suitable for the article:
res <- tidyr::spread(bres[c(1, 2, 5)], expression, median)
# name order is not deterministic!
names(res) <- c("datarows",
                sub("([^:]*).*", "\\1", names(res)[-1]))
res <- res[c("datarows", "icd", "comorbidity", "medicalrisk")]
res$icd <- as.numeric(res$icd)
res$comorbidity <- as.numeric(res$comorbidity)
res$medicalrisk <- as.numeric(res$medicalrisk)
res <- as.data.frame(res)
# work around an older R version abbreviating dput output in some R versions
old_opt_dml <- options(deparse.max.lines = 0)
# keep file name the same so Makefile will keep track, i.e. not dated, but
# unique for machine and benchmark
dput(res, get_bench_short_filename("dput-latest", "R"))
# and a dated version
dput(res, get_bench_filename("dput-dated", "R", use_date = TRUE))
options(old_opt_dml)
