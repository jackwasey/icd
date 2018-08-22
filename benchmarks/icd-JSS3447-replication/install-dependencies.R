# Ensure the user has a CRAN repository to install benchmark dependencies
repos_new <- repos_old <- getOption("repos")
if (length(repos_old) == 0) {
  repos_new[["CRAN"]] = "https://cloud.r-project.org/"
  options(repos = repos_new)
}
icd_bench_lib <- file.path(getwd(), "lib-bench")
dir.create(icd_bench_lib, showWarnings = FALSE)
message("library paths are:")
print(.libPaths())
message("updating packages to benchmark pacakge library, not whole system")
update.packages(lib.loc = c(icd_bench_lib, .libPaths()[1]),
                instlib = icd_bench_lib,
                ask = FALSE)

for (p in c("icd.data",
            "checkmate",
            "Rcpp",
            "RcppEigen",
            "rmarkdown",
            "knitr",
            "bench",
            "tidyr",
            "tinytex",
            "R.cache",
            "comorbidity",
            "medicalrisk")) {
  if (!require(package = p,
               #lib.loc = c(icd_bench_lib, .libPaths()),
               lib.loc = icd_bench_lib,
               quietly = TRUE,
               warn.conflicts = FALSE,
               character.only = TRUE))
    install.packages(p, lib = icd_bench_lib)
}

# may be installed in a different library, so don't use the temporary benchmarking library
if (!require("icd")) {
  yn <- readline("'icd' not installed. Installing from CRAN? (y/n)")
  if (!interactive() || tolower(yn) == "y")
    install.packages("icd", lib = icd_bench_lib)
}

options(repos = repos_old)
