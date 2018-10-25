#source("install-dependencies.R")
if (!require("icd")) {
  yn <- readline("'icd' not installed. Installing from CRAN? (y/n)")
  if (tolower(yn) == "y") install.packages("icd")
}
args <- commandArgs(trailingOnly = TRUE)
n_order <- 5L
if (length(args) > 1L) stop("Only one argument is accepted, which is the order",
                            " of magitude of the biggest benchmark")
if (length(args) == 1L)
  n_order <- as.integer(args[1])
message("Running benchmarks, with biggest synthetic data set having 10^",
        n_order, " rows.")
if (n_order > 5L)
  warning("Depending on hardware, running these benchmarks with 10^6 or more",
          " rows may take hours.")

requireNamespace("comorbidity")
requireNamespace("medicalrisk")
requireNamespace("touch")
requireNamespace("R.cache")
requireNamespace("bench")
requireNamespace("tidyr", quietly = TRUE)
library(icd)

# include generation functions for reproducibility
generate_random_short_icd9 <- function(n = 50000)
  as.character(floor(stats::runif(min = 1, max = 99999, n = n)))
generate_pts <- function(num_patients, dz_per_patient = 20,
                         n = num_patients, np = dz_per_patient,
                         fun = generate_random_short_icd9) {
  set.seed(1441)
  pts <- round(n / np)
  data.frame(
    visit_id = as_char_no_warn(sample(seq(1, pts), replace = TRUE, size = n)),
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

medicalrisk_fix <- function(pts_mr) {
  names(pts_mr) <- c("id", "icd9cm")
  pts_mr$icd9cm <- paste("D", pts_mr$icd9cm, sep = "")
  pts_mr
}

ten_million_random_pts <- get_ten_million_icd9_pts()
n <- 10^(1L:n_order)
bres <- bench::press(n = n, {
  pts <- ten_million_random_pts[seq_len(n), ]
  pts_mr <- medicalrisk_fix(pts)
  # slow for making matrix of many rows, don't count against 'touch'
  pts_mat <- icd::long_to_wide(pts)
  bench::mark(
    comorbid_charlson(pts),
    comorbidity::comorbidity(
      x = pts, id = "visit_id", code = "code", score = "charlson",
      parallel = n >= 1e5),
    medicalrisk::generate_comorbidity_df(
      pts_mr, icd9mapfn = medicalrisk::icd9cm_charlson_quan),
    touch::cmbd(pts), # AHRQ, Charlson not available.
    check = FALSE
  )
})

host <- Sys.info()["nodename"]

# keep the results (Makefile will look for updated dput results below)
saveRDS(bres,
        paste0(
          paste("bench-versus-result", n_order, host,
                make.names(Sys.Date()), sep = "-"),
          ".rds")
)
# now take the medians and make suitable for the article:
res <- tidyr::spread(bres[c(1, 2, 5)], expression, median)
names(res) <- c("datarows", "icd", "comorbidity", "medicalrisk", "touch")
res$icd <- as.numeric(res$icd)
res$comorbidity <- as.numeric(res$comorbidity)
res$medicalrisk <- as.numeric(res$medicalrisk)
res$touch <- as.numeric(res$touch)
res <- as.data.frame(res)
# work around R abbreviating dput output in some R versions
old_opt_dml <- options(deparse.max.lines = 0)
dput(res)
# keep file name the same so Makefile will keep track
dput(res,
     paste0(
       paste("bench-versus-dput", n_order, host, sep = "-"),
       ".R")
)
options(old_opt_dml)
