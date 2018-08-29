source("install-dependencies.R")
args <- commandArgs(trailingOnly = TRUE)
n_order <- 3L
if (length(args) > 1L) stop("Only one argument is accepted, which is the order",
                            " of magitude of the biggest benchmark")
if (length(args) == 1L)
  n_order <- as.integer(args[1])
message("Running benchmarks, with biggest synthetic data set having 10^",
        n_order, " rows.")
if (n_order > 5L)
  warning("Depending on hardware, running these benchmarks with 10^6 or more",
          " rows may take hours.")

# include generation functions for reproducibility
generate_random_short_icd9 <- function(n = 50000)
  as.character(floor(stats::runif(min = 1, max = 99999, n = n)))

generate_pts <- function(num_patients, dz_per_patient = 20,
                         n = num_patients, np = dz_per_patient) {
  set.seed(1441)
  pts <- round(n / np)
  data.frame(
    visit_id = as.character(sample(seq(1, pts), replace = TRUE, size = n)),
    code = fun(n),
    poa = as.factor(
      sample(x = c("Y", "N", "n", "n", "y", "X", "E", "", NA),
             replace = TRUE, size = n)),
    stringsAsFactors = FALSE
  )
}

get_ten_million_icd9_pts <- function() {
  ten_million_random_pts <-
    R.cache::loadCache(key = list("ten_million_random_pts"),
                       suffix = "icd.Rcache")
  if (is.null(ten_million_random_pts)) {
    ten_million_random_pts <- generate_pts(1e7)
    R.cache::saveCache(ten_million_random_pts,
                       key = list("ten_million_random_pts"),
                       suffix = "icd.Rcache")
  }
  invisible(ten_million_random_pts)
}

get_pts <- function(n = 1e6, dz_per_pt = 5, use_cache = FALSE) {
  if (use_cache) {
    pts <- R.cache::loadCache(key = list("pts_real_diags", n, dz_per_pt),
                              suffix = "icd.Rcache")
    if (!is.null(pts)) return(pts)
  }
  set.seed(1441)
  diags <- sample(icd.data::icd9cm_hierarchy$code, size = n, replace = TRUE)
  pts <- data.frame(
    visit_id = as.character(floor(seq_len(n) / dz_per_pt)),
    code = diags,
    stringsAsFactors = FALSE
  )
  R.cache::saveCache(pts,
                     key = list("pts_real_diags", n, dz_per_pt),
                     suffix = "icd.Rcache")
  pts
}

medicalrisk_fix <- function(pts_mr) {
  names(pts_mr) <- c("id", "icd9cm")
  pts_mr$icd9cm <- paste("D", pts_mr$icd9cm, sep = "")
  pts_mr
}

#ten_million_random_pts <- get_ten_million_icd9_pts()
ten_million_random_pts <- get_pts(1e7, dz_per_pt = 5, use_cache = TRUE)
n <- 10^(1L:n_order)
bres <- bench::press(n = n, {
  pts <- ten_million_random_pts[seq_len(n), ]
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

host <- Sys.info()["nodename"]

# keep the dated results
saveRDS(bres,
        paste0(
          paste("bench-versus-result", n_order, host, Sys.Date(), sep = "-"),
          ".rds")
)

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
dput(res)
# keep file name the same so Makefile will keep track, i.e. not dated, but
# unique for machine and benchmark
dput(res,
     paste0(
       paste("bench-versus-dput", n_order, host, sep = "-"),
       ".R")
)
# and a dated version
dput(res,
     paste0(
       paste("bench-versus-dput", n_order, host, Sys.Date(), sep = "-"),
       ".R")
)
options(old_opt_dml)
