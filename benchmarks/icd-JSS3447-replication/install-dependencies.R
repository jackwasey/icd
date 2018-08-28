source("checkpoint.R")
print(sessionInfo())
message(".libPaths()")
print(.libPaths())
old_lib_paths <- icd_checkpoint()
message("old library paths are:")
print(old_lib_paths)
message("updating packages to benchmark package library, not modifying system")
#update.packages(lib.loc = c(icd_bench_lib, .libPaths()[1]),
#                instlib = icd_bench_lib,
#                ask = FALSE)

# although checkpoint should scan for and install these packages, let's be explicit:
#if (!require(bench)) install.packages(bench)

library(utf8)
library(checkmate)
library(knitr)
library(profmem)
library(R.cache)
library(Rcpp)
library(RcppEigen)
library(tidyr)
library(bench)
#library(rmarkdown) # not needed for repeating benchmarks
#library(tinytex)
library(comorbidity)
library(medicalrisk)
library(icd.data)
# may be installed in a different library, so don't use the temporary
# benchmarking library as first choice
if (!require("icd", lib.loc = c(old_lib_paths, .libPaths()))) {
  yn <- readline("'icd' not installed. Install from CRAN? (y/n)")
  if (!interactive() || tolower(yn) == "y")
    install.packages("icd",
#                     lib = icd_bench_lib,
                     INSTALL_opts = "--no-test-load")
}
message("restoring original library paths for user")
unCheckpoint(old_lib_paths)

