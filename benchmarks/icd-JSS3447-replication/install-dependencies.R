# Ensure the user has a CRAN repository to install benchmark dependencies
repos_new <- repos_old <- getOption("repos")
if (length(repos_old) == 0) {
  repos_new[["CRAN"]] = "https://cloud.r-project.org/"
  options(repos = repos_new)
}
on.exit(options(repos = repos_old), add = TRUE)
libPaths_old <- .libPaths()
on.exit(.libPaths(libPaths_old), add = TRUE)
icd_bench_lib <- "icd-bench-lib"
.libPaths(icd_bench_lib)

# icd should already have required this in package Depends, but let's make sure
if (!require("icd.data")) install.packages("icd.data")

# icd should also have the following installed because listed as Imports
if (!require("checkmate")) install.packages("checkmate")
if (!require("magrittr")) install.packages("magrittr")
if (!require("Rcpp")) install.packages("Rcpp")

# The following are specifically for benchmark and not pacakge dependencies
if (!require("comorbidity")) install.packages("comorbidity")
if (!require("medicalrisk")) install.packages("medicalrisk")
if (!require("R.cache")) install.packages("R.cache")
if (!require("bench")) install.packages("bench")
if (!require("tidyr")) install.packages("tidyr")
