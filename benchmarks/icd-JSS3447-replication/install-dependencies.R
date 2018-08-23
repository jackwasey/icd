icd_bench_lib <- file.path(getwd(), "lib-bench")
dir.create(icd_bench_lib, showWarnings = FALSE)
message("updating packages to benchmark package library, not modifying system")
update.packages(lib.loc = c(icd_bench_lib, .libPaths()[1]),
                instlib = icd_bench_lib,
                ask = FALSE)
for (p in c("icd.data",
            "bench",
            "checkmate",
            "knitr",
            "profmem",
            "R.cache",
            "Rcpp",
            "RcppEigen",
            "rmarkdown",
            "tidyr",
            "tinytex",
            "comorbidity",
            "medicalrisk")) {
  if (!require(package = p,
               lib.loc = c(icd_bench_lib, .libPaths()),
               quietly = TRUE,
               warn.conflicts = FALSE,
               character.only = TRUE))
    install.packages(p,
                     lib = icd_bench_lib,
                     INSTALL_opts = "--no-test-load")
}
# may be installed in a different library, so don't use the temporary
# benchmarking library as first choice
if (!require("icd", lib.loc = c(.libPaths(), icd_bench_lib))) {
  yn <- readline("'icd' not installed. Install from CRAN? (y/n)")
  if (!interactive() || tolower(yn) == "y")
    install.packages("icd",
                     lib = icd_bench_lib,
                     INSTALL_opts = "--no-test-load")
}
