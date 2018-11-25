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
# simulating NEDS (icd-9) db for size (also need to simulate width, which is 15+4 columns!)
n_neds <- 28584301L
if (!exists("divisor"))
  divisor <- as.integer(readline(prompt = "Enter a divisor: "))
n <- n_neds / divisor
key = list("bench-pccc-wide", n, icd10 = FALSE, ncol = 20L)
dat_wide_str <- R.cache::loadCache(key)
if (is.null(dat_wide_str)) {
  dat_wide_str <-
    generate_neds_pts(n = n, ncol = 20L, icd10 = FALSE, verbose = TRUE)
  R.cache::saveCache(dat_wide_str, key)
}
message("Benchmarking for ", n, " rows of simulated NEDS data.")
message("icd:")
icdtm2 <- proc.time()
res_icd2 <- icd::comorbid_pccc_dx(dat_wide_str,
                                  restore_id_order = FALSE, # don't re-sort
                                  validate = FALSE) # don't check input factors
print(proc.time() - icdtm2)

message("PCCC:")
pccctm <- proc.time()
res_pccc <- pccc::ccc(dat_wide_str,
                      "id",
                      dx_cols = seq(2, ncol(dat_wide_str)),
                      icdv = 9)
print(proc.time() - pccctm)
if (identical(colSums(res_icd2), colSums(res_pccc[2:13]))) {
  message("results identical for 'icd' and 'pccc' computations")
} else {
  stop("results differ between 'icd' and 'pccc'")
}
