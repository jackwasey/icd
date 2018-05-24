# valgrind icd10 searching
# ln -sf ~/.R/Makevars.valgrind Makevars
# ~/icd/tools/install_full.sh
# R --vanilla -d "valgrind --tool=callgrind --instr-atstart=no"
#
library(icd)
library(magrittr)
# try with and without ICD codes as a factor: need strings either way, but a
# factor we should not be converting to string and back, just work on the factor
# levels.

# simulating NEDS db for size (also need to simulate width, which is 15+4 columns!)
n <- 2.8584301 * 1e7
codes <- unclass(icd:::as_char_no_warn(icd.data::icd10cm2016$code))
set.seed(1441)
dat <- data.frame(id = n + seq(n),
                  icd_code = sample(codes, n, replace = TRUE),
                  stringsAsFactors = TRUE)
#system.time(pccc_dx <- comorbid_pccc_dx(dat))
if (FALSE) {
  profvis::profvis(pccc_dx <- comorbid_pccc_dx(dat))
}

# generate wide data with realistic number of codes per patient:

pts_per_code_pos <- as.integer(n / (1:19) ^ 4)
library(R.cache)
dat_wide <- R.cache::loadCache(key = list("bench-icd10-pccc"))
if (is.null(dat_wide)) {
  dat_wide <- data.frame(id = dat$id,
                         dx01 = dat$icd_code,
                         stringsAsFactors = TRUE)
  dx_seq <- seq(2L, 19L)
  for (dx in dx_seq) {
    dx_str <- sprintf("%02i", dx_seq[dx])
    len <- pts_per_code_pos[dx]
    f <- as.integer(sample(c(1L + seq_len(len), rep(1L, n - len))))
    attr(f, "levels") <- c(NA, sample(codes, len, replace = TRUE))
    attr(f, "class") <- "factor"
    dat_wide[[paste0("dx", dx_str)]] <- f
  }
  R.cache::saveCache(dat_wide, key = list("bench-icd10-pccc"))
}

if (FALSE)
  system.time(dat_long <- icd::wide_to_long(dat_wide))
# ugh - very slow

# can we just calculate comorbidities for each column, then OR?
system.time(
  res <- lapply(names(dat_wide)[-1], function(x) { message(x); comorbid_pccc_dx(dat_wide[c("id", x)]) })
)
