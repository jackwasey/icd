# Ensure the user has a CRAN repository to install benchmark dependencies
repos_new <- repos_old <- getOption("repos")
if (length(repos_old) == 0) {
  repos_new[["CRAN"]] = "https://cloud.r-project.org/"
  options(repos = repos_new)
}
icd_bench_lib <- file.path(getwd(), "icd-bench-lib")
dir.create(icd_bench_lib, showWarnings = FALSE)

# icd should already have required this in package Depends, but let's make sure
if (!require("icd.data")) install.packages("icd.data", lib = icd_bench_lib)

# icd should also have the following installed because listed as Imports
if (!require("checkmate")) install.packages("checkmate", lib = icd_bench_lib)
if (!require("magrittr")) install.packages("magrittr", lib = icd_bench_lib)
if (!require("Rcpp")) install.packages("Rcpp", lib = icd_bench_lib)

# some suggested dependencies needed for building replication materials
if (!require("rmarkdown")) install.packages("rmarkdown", lib = icd_bench_lib)
if (!require("knitr")) install.packages("knitr", lib = icd_bench_lib)

# The following are specifically for benchmark and not pacakge dependencies
if (!require("comorbidity")) install.packages("comorbidity", lib = icd_bench_lib)
if (!require("medicalrisk")) install.packages("medicalrisk", lib = icd_bench_lib)
if (!require("R.cache")) install.packages("R.cache", lib = icd_bench_lib)
if (!require("bench")) install.packages("bench", lib = icd_bench_lib)
if (!require("tidyr")) install.packages("tidyr", lib = icd_bench_lib)

if (!require("icd")) {
  yn <- readline("'icd' not installed. Installing from CRAN? (y/n)")
  if (!interactive() || tolower(yn) == "y")
    install.packages("icd", lib = icd_bench_lib)
}

options(repos = repos_old)
