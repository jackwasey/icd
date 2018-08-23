icd_bench_lib <- file.path(getwd(), "lib-bench")
dir.create(icd_bench_lib, showWarnings = FALSE)
source("checkpoint.R")
old_lib_paths <- icd_checkpoint()
message("updating packages to benchmark package library, not modifying system")
update.packages(lib.loc = c(icd_bench_lib, .libPaths()[1]),
                instlib = icd_bench_lib,
                ask = FALSE)
library(icd.data)
library(bench)
library(checkmate)
library(knitr)
library(profmem)
library(R.cache)
library(Rcpp)
library(RcppEigen)
library(rmarkdown)
library(tidyr)
library(tinytex)
library(comorbidity)
library(medicalrisk)
# may be installed in a different library, so don't use the temporary
# benchmarking library as first choice
if (!require("icd", lib.loc = c(old_libs_paths, .libPaths()))) {
  yn <- readline("'icd' not installed. Install from CRAN? (y/n)")
  if (!interactive() || tolower(yn) == "y")
    install.packages("icd",
                     lib = icd_bench_lib,
                     INSTALL_opts = "--no-test-load")
}
unCheckpoint(old_lib_paths)
