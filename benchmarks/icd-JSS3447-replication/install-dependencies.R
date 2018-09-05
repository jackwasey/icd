install_jss3447_deps <- function() {
  repos = options("repos")[1]
  # don't even assume the option for CRAN repo is correct...
  cran_ok <- TRUE
  tryCatch(readLines(url(repos)),
           error = function(e) cran_ok <<- FALSE,
           warning = function(e) {})
  if (is.null(repos$repos) || !cran_ok)
    repos = c(CRAN = "https://cloud.r-project.org/")
  for (p in c(
    "utf8",
    "bench",
    "backports",
    "checkmate",
    "magrittr",
    "testthat",
    "knitr",
    "profmem",
    "Rcpp",
    "RcppEigen",
    "tidyr",
    "comorbidity",
    "medicalrisk",
    "icd.data"
  )) {
    if (!require(p, character.only = TRUE,
                 quietly = TRUE, warn.conflicts = FALSE)) {
      install.packages(p, character.only = TRUE, repos = repos)
    }
    library(p, character.only = TRUE,
            quietly = TRUE, warn.conflicts = FALSE)
  }

  if (!require("icd", quietly = TRUE)) {
    message("icd not yet installed, so installing from CRAN")
    install.packages("icd", repos = repos)
    library("icd", quietly = TRUE)
  }

  if (!file.exists(".deps")) file.create(".deps", showWarnings = FALSE)
  return(invisible())
}

install_jss3447_deps()
