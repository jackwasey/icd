# valgrind icd10 searching
# ln -sf ~/.R/Makevars.valgrind Makevars
# ~/icd/tools/install_full.sh
# R --vanilla -d "valgrind --tool=callgrind --instr-atstart=no"
#
library(icd)
library(magrittr)
library(R.cache)
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
dat <- data.frame(id = n + seq(n),
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
system.time(
  res <- lapply(rev(names(dat_wide)[-1]),
                function(x) {
                  message(x);
                  system.time(
                    if (do_icd9)
                      icd9_comorbid_pccc_dx(dat_wide[c("id", x)], icd_name = x)
                    else
                      icd10_comorbid_pccc_dx(dat_wide[c("id", x)], icd_name = x)
                  )
                  gc(verbose = TRUE)
                }
  )
)

pccc::ccc(ned,
    subject_id,
    dx_cols = dplyr::vars(dplyr::num_range("dx", c(2,21))),
    pc_cols = NULL,
    icdv = 9)

# repeat with character instead of factor
