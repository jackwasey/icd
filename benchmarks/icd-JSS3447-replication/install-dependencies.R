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
    "parallel", # used by 'comorbidity'
    "plyr", "reshape", "hash", # used by 'medicalrisk'
    "testthat",
    "knitr",
    "profmem",
    "Rcpp",
    "RcppEigen",
    "tidyr",
#    "comorbidity", # CRAN version has incompatible updates
#    "medicalrisk", # CRAN version from 2016 unchanged at time of submission
    "icd.data"
  )) {
    if (!require(p, character.only = TRUE,
                 quietly = TRUE, warn.conflicts = FALSE)) {
      install.packages(p, character.only = TRUE, repos = repos)
    }
    library(p, character.only = TRUE,
            quietly = TRUE, warn.conflicts = FALSE)
  }
  install.packages("medicalrisk_1.2.tar.gz", repos = NULL)
  install.packages("comorbidity_0.1.1.tar.gz", repos = NULL)
  if (!require("icd", quietly = TRUE)) {
    message("icd not yet installed, so installing from CRAN")
    install.packages("icd", repos = repos)
    library("icd", quietly = TRUE)
  }

  if (!file.exists(".deps")) file.create(".deps", showWarnings = FALSE)
  return(invisible())
}

install_jss3447_deps()
