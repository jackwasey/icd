library(icd)
requireNamespace("comorbidity")
requireNamespace("medicalrisk")
requireNamespace("R.cache")
requireNamespace("bench")
requireNamespace("tidyr")

find_cmb_cutoff <- FALSE

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

my_check <- function(x, y) {
  x2 <- unname(x[2:18])
  y2 <- unname(y[2:18])
  if (is.data.frame(x2)) {
    x2 <- lapply(x2, as.logical)
    y2 <- lapply(y2, as.logical)
  }
  all.equal(x2, y2)
}

ten_million_random_pts <- get_ten_million_icd9_pts()
# first need to benchmark comorbidity against itself to know the cut-off for
# using the parallel flag
if (find_cmb_cutoff) {
  n <- 10^(0L:5L)
  cmb_res <- bench::press(n = n, {
    pts <- ten_million_random_pts[seq_len(n), ]
    bench::mark(
      comorbidity::comorbidity(
        x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
        parallel = TRUE),
      comorbidity::comorbidity(
        x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
        parallel = FALSE)
    )})
}
# benchmark
n <- 10^(1L:7L)
bres <- bench::press(n = n, {
  pts <- ten_million_random_pts[seq_len(n), ]
  pts_mr <- medicalrisk_fix(pts)
  bench::mark(
    comorbid_charlson(pts),
    comorbidity::comorbidity(
      x = pts, id = "visit_id", code = "code", score = "charlson_icd9",
      parallel = n >= 1e5),
    medicalrisk::generate_comorbidity_df(
      pts_mr, icd9mapfn = medicalrisk::icd9cm_charlson_quan),
    check = FALSE
  )
})
saveRDS(bres, paste0("bench-versus-result-", make.names(date()), ".rds"))
# now take the medians and make suitable for the article:
res <- tidyr::spread(bres[c(1, 2, 5)], expression, median)
names(res) <- c("datarows", "icd", "comorbidity", "medicalrisk")
res$icd <- as.numeric(res$icd)
res$comorbidity <- as.numeric(res$comorbidity)
res$medicalrisk <- as.numeric(res$medicalrisk)
res <- as.data.frame(res)
dput(res)


data.frame(
  datarows = c(10, 100, 1000, 10000, 1e+05, 1e+06, 1e+07),
  icd = c(
    0.00271933455951512,
    0.00298570952145383,
    0.00365349801722914,
    0.0127100950339809,
    0.116895835148171,
    0.9445570159005,
    12.3969755911967
  ),
  comorbidity = c(
    0.027545070159249,
    0.0408755420939997,
    0.152954794000834,
    1.2128290720284,
    7.22272577905096,
    38.3780716699548,
    946.861061039963
  ),
  medicalrisk = c(
    0.00384478160412982,
    0.0118726470973343,
    0.0797245030407794,
    0.802611763007008,
    8.43512935494073,
    85.9830659220461,
    6336.18419963482
  )
)
