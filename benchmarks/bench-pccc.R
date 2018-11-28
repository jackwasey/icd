#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)
if (length(args)) divisor = as.integer(args[1])
# valgrind icd10 searching
# ln -sf ~/.R/Makevars.valgrind Makevars
# ~/icd/tools/install_full.sh
# R --vanilla -d "valgrind --tool=callgrind --instr-atstart=no"

# Run with less garbage collection. e.g.
#
# R_GC_MEM_GROW=3 R -e 'devtools::load_all(); pts <- icd:::generate_neds_pts(1e7, icd10 = TRUE); gprofiler::profile(icd::comorbid_pccc_dx(pts), out_format="pdf", out_filename = "/tmp/out7-icd10.pdf")'
suppressPackageStartupMessages({
  library(icd)
  library(dplyr)
  library(magrittr, warn.conflicts = FALSE)
  library(R.cache)
  library(pccc)
})
# simulating NEDS (icd-9) db for size (also need to simulate width, which is 15+4 columns!)
n_neds <- 28584301L
if (!exists("divisor")) {
  divisor <- if (interactive())
    as.integer(readline(prompt = "Enter a divisor: "))
  else
    10000
}
n <- n_neds / divisor
key = list("bench-pccc-wide", n, icd10 = FALSE, ncol = 20L)
dat_wide_str <- R.cache::loadCache(key)
if (is.null(dat_wide_str)) {
  dat_wide_str <-
    icd:::generate_neds_pts(n = n, ncol = 20L, icd10 = FALSE, verbose = TRUE)
  R.cache::saveCache(dat_wide_str, key)
}
# See https://stat.ethz.ch/R-manual/R-devel/library/base/html/Memory.html for
# possible ways to reduce frequency or agressiveness of the garbage collection.

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
