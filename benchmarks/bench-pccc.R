# valgrind icd10 searching
# ln -sf ~/.R/Makevars.valgrind Makevars
# ~/icd/tools/install_full.sh
# R --vanilla -d "valgrind --tool=callgrind --instr-atstart=no"
#
library(icd)
library(magrittr)
library(R.cache)
library(pccc)
# try with and without ICD codes as a factor: need strings either way, but a
# factor we should not be converting to string and back, just work on the factor
# levels.

do_icd9 <- TRUE
divide <- 1L

# simulating NEDS (icd-9) db for size (also need to simulate width, which is 15+4 columns!)
n <- 28584301L
n <- n %/% divide
codes <- if (do_icd9) unclass(icd:::as_char_no_warn(icd.data::icd9cm_hierarchy$code)) else unclass(icd:::as_char_no_warn(icd.data::icd10cm2016$code))
set.seed(1441)
dat <- data.frame(id = as.character(n + seq(n)),
                  icd_code = sample(codes, n, replace = TRUE),
                  stringsAsFactors = TRUE)
#system.time(pccc_dx <- comorbid_pccc_dx(dat))
if (FALSE) {
  profvis::profvis(pccc_dx <- comorbid_pccc_dx(dat))
}

# generate wide data with realistic number of codes per patient:

pts_per_code_pos <- as.integer(n / (1:20) ^ 4)
key = list(paste0("bench-pccc-div", divide, ifelse(do_icd9, "-icd9", "-icd10")))
dat_wide <- R.cache::loadCache(key)
if (is.null(dat_wide) || nrow(dat_wide != n)) {
  dat_wide <- data.frame(id = dat$id,
                         dx01 = dat$icd_code,
                         stringsAsFactors = TRUE)
  for (dx in seq(2L, 20L)) {
    dx_str <- sprintf("%02i", dx)
    message("building column:", dx_str)
    len <- pts_per_code_pos[dx]
    f <- as.integer(sample(c(1L + seq_len(len), rep(1L, n - len))))
    attr(f, "levels") <- c(NA, sample(codes, len, replace = TRUE))
    attr(f, "class") <- "factor"
    dat_wide[[paste0("dx", dx_str)]] <- f
  }
  R.cache::saveCache(dat_wide, key)
}

if (FALSE)
  system.time(dat_long <- icd::wide_to_long(dat_wide))
# ugh - very slow - 10 or 20 seconds on Mac on 1% of data

# can we just calculate comorbidities for each column, then OR?
ptm <- proc.time()
res <- lapply(
  rev(names(dat_wide)[-1]),
  function(x) {
    message("working on column: ", x);
    tm <- system.time(
      if (do_icd9)
        icd9_comorbid_pccc_dx(dat_wide[c("id", x)], icd_name = x, restore_visit_order = FALSE)
      else
        icd10_comorbid_pccc_dx(dat_wide[c("id", x)], icd_name = x, restore_visit_order = FALSE)
    )
    print(tm)
    #gc(verbose = TRUE)
  }
)
print(proc.time() - ptm)

# NEDS simulated data result: 14 mins on Mac (4 core, just 2.5Ghz 16G RAM)
# after some simple optimization, with GC each step, 5m40s (vs 18 mins in JAMA letter)
# without garbage collection, 10 seconds per col, more with more occupancy: 4m40s

if (FALSE)
  profvis::profvis(icd9_comorbid_pccc_dx(dat_wide[c("id", "dx10")], icd_name = "dx10", restore_visit_order = FALSE))
# profiling shows that converting the ID column to string was a big time sink (repeated for each column!),
# as was re-ordering the visit IDs, which could be skipped if we just want summary data.

# try to get pccc working at all:
m_tens <- 10^c(3, 4, 5)
pccc_timings <- list()
for (m in c(m_tens, n)) {
  message("working on pccc:ccc_mat_rcpp with", m, "rows")
  st <- system.time(
    pccc:::ccc_mat_rcpp(as.matrix(dat_wide[seq_len(m), -1]),
                        matrix("", nrow = m), 9) # gave up after about 10 minutes on first run.
  )
  pccc_timings[as.character(m)] <- as.list(st)$elapsed
  message(" - took ", as.list(st)$elapsed, "seconds")
}
# simply extrapolating, it will take at least half an hour for PCCC to complete

pccc::ccc(head(dat_wide),
          "id",
          dx_cols = seq(2, ncol(dat_wide)),
          #dx_cols = tidyselect::vars_select(names(dat_wide), tidyselect::starts_with("dx")),
          pc_cols = NULL,
          icdv = 9)

# repeat with character instead of factor
