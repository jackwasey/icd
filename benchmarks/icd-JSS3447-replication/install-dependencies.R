# Ensure the user has a CRAN repository to install benchmark dependencies
repos_new <- repos_old <- getOption("repos")
if (length(repos_old) == 0) {
	  repos_new[["CRAN"]] = "https://cloud.r-project.org/"
  options(repos = repos_new)
}
libPaths_old <- .libPaths()
icd_bench_lib <- file.path(getwd(), "icd-bench-lib")
dir.create(icd_bench_lib, showWarnings = FALSE)
.libPaths(icd_bench_lib)
message("Using the following libPaths:")
print(.libPaths())

# icd should already have required this in package Depends, but let's make sure
if (!require("icd.data")) install.packages("icd.data")

# icd should also have the following installed because listed as Imports
if (!require("checkmate")) install.packages("checkmate")
if (!require("magrittr")) install.packages("magrittr")
if (!require("Rcpp")) install.packages("Rcpp")

# some suggested dependencies needed for building replication materials
if (!require("rmarkdown")) install.packages("rmarkdown")
if (!require("knitr")) install.packages("knitr")

# The following are specifically for benchmark and not pacakge dependencies
if (!require("comorbidity")) install.packages("comorbidity")
if (!require("medicalrisk")) install.packages("medicalrisk")
if (!require("R.cache")) install.packages("R.cache")
if (!require("bench")) install.packages("bench")
if (!require("tidyr")) install.packages("tidyr")

if (!require("icd")) {
	  yn <- readline("'icd' not installed. Installing from CRAN? (y/n)")
  if (tolower(yn) == "y") install.packages("icd")
}

.libPaths(libPaths_old)
options(repos = repos_old)

