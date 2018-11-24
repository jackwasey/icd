# valgrind icd10 searching
# ln -sf ~/.R/Makevars.valgrind Makevars
# ~/icd/tools/install_full.sh
# R --vanilla -d "valgrind --tool=callgrind --instr-atstart=no"
#
library(icd)
library(dplyr)
library(magrittr)
library(R.cache)
library(pccc)
do_icd9 <- TRUE
# simulating NEDS (icd-9) db for size (also need to simulate width, which is 15+4 columns!)
n <- 28584301L
divisor <- as.integer(readline(prompt = "Enter a divisor: "))
n <- n / divisor
codes <- if (do_icd9) {
  unclass(icd:::as_char_no_warn(icd.data::icd9cm_hierarchy$code))
} else {
  unclass(icd:::as_char_no_warn(icd.data::icd10cm2016$code))
}
set.seed(1441)
dat <- data.frame(id = as.character(n + seq(n)),
                  icd_code = sample(codes, n, replace = TRUE),
                  stringsAsFactors = TRUE)
#system.time(pccc_dx <- comorbid_pccc_dx(dat))
if (FALSE)
  profvis::profvis(pccc_dx <- comorbid_pccc_dx(dat))
# generate wide data with realistic number of codes per patient:
pts_per_code_pos <- as.integer(n / (1:20) ^ 4)
key_factors = list(paste0("bench-pccc-factors", do_icd9, n))
key_str = list(paste0("bench-pccc-wide", do_icd9, n))
use_cache <- FALSE
if (use_cache) {
  dat_wide_factors <- R.cache::loadCache(key_factors)
  dat_wide_str <- R.cache::loadCache(key_str)
}
if (is.null(dat_wide_factors) || nrow(dat_wide_factors) != n) {
  dat_wide_factors <- data.frame(id = dat$id,
                                 dx01 = dat$icd_code,
                                 stringsAsFactors = TRUE)
  for (dx in seq(2L, 20L)) {
    dx_str <- sprintf("%02i", dx)
    message("building column:", dx_str)
    len <- pts_per_code_pos[dx]
    l <-  unique(c(NA, sample(codes, len, replace = TRUE)))
    f <- as.integer(sample(c(seq_along(l), rep(1L, n - length(l)))))
    attr(f, "levels") <- l
    attr(f, "class") <- "factor"
    dat_wide_factors[[paste0("dx", dx_str)]] <- f
  }
  R.cache::saveCache(dat_wide_factors, key_factors)
  dat_wide_str <- dat_wide_factors[1]
  for (i in seq_along(dat_wide_factors)) {
    if (i == 1) next
    dat_wide_str[names(dat_wide_factors)[i]] <-
      as.character(dat_wide_factors[[i]])
  }
  R.cache::saveCache(dat_wide_str, key_str)
}

if (FALSE) { # rest is testing

  # NEDS simulated data result: 14 mins on Mac (4 core, just 2.5Ghz 16G RAM) after
  # some simple optimization, with GC each step, 5m40s (vs 18 mins in JAMA letter)
  # without garbage collection, 10 seconds per col, more with more occupancy:
  # Down to 4m40s, which is similar on xeon and mac
  if (FALSE)
    profvis::profvis(icd9_comorbid_pccc_dx(dat_wide_factors[c("id", "dx10")],
                                           icd_name = "dx10",
                                           restore_id_order = FALSE,
                                           unique_ids = TRUE))
  m_tens <- 10^c(seq_len(log10(n)))
  pccc_timings <- list()
  for (m in unique(c(m_tens, n))) {
    message("working on pccc:ccc_mat_rcpp with ", m, " rows")
    st <- system.time(
      devnull <- pccc:::ccc_mat_rcpp(as.matrix(dat_wide[seq_len(m), -1]),
                                     matrix("", nrow = m), 9)
    )
    pccc_timings[as.character(m)] <- as.list(st)$elapsed
    message(" - took ", as.list(st)$elapsed, " seconds")
  }
  # full simulated NEDS 21 mins on xeon 4 core, (likely similar on Mac) vs twice
  # the time for PCCC compared to icd on 72-core server (busy server)
  if (FALSE)
    pccc::ccc(head(dat_wide_str),
              "id",
              dx_cols = seq(2, ncol(dat_wide_str)),
              #dx_cols = tidyselect::vars_select(names(dat_wide), tidyselect::starts_with("dx")),
              # pc_cols,
              icdv = 9)

  # finding unique values is slowest step: consider
  # https://github.com/jl2922/omp_hash_map or
  # https://github.com/efficient/libcuckoo or
  # https://github.com/preshing/junction

  # repeat with character instead of factor
}

if (FALSE)
  system.time(dat_long <- icd::wide_to_long(dat_wide))
# ugh - wide to long is very slow - 10 or 20 seconds on Mac on 1% of data instead, can we just
# calculate comorbidities for each column, then OR? But icd has to work on the
# generated ID column each time, which is a big time sink

if (FALSE) {
  message("icd old way:")
  ptm <- proc.time()
  res <- matrix(0L, nrow = n, ncol = 12L)
  for (x in rev(names(dat_wide_factors)[-1])) {
    message("working on column ", x);
    if (do_icd9)
      res <- res | icd9_comorbid_pccc_dx( # copies big matrix each update
        dat_wide_factors[c("id", x)], icd_name = x,
        restore_id_order = FALSE)
    else
      res <- res | icd10_comorbid_pccc_dx(
        dat_wide_factors[c("id", x)], icd_name = x,
        restore_id_order = FALSE)
  }
  message("icd old way:")
  print(proc.time() - ptm)
}

message("icd:")
icdtm2 <- proc.time()
res_icd2 <- icd::comorbid_pccc_dx(dat_wide_str)
print(proc.time() - icdtm2)

message("PCCC:")
pccctm <- proc.time()
res_pccc <- pccc::ccc(dat_wide_str,
                      "id",
                      dx_cols = seq(2, ncol(dat_wide_str)),
                      icdv = 9)
print(proc.time() - pccctm)
identical(colSums(res), colSums(res_pccc[2:13]))
