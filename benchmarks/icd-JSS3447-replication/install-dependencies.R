install_jss3447_deps <- function() {
  repos <- options("repos")[1]
  # don't even assume the option for CRAN repo is correct...
  cran_ok <- TRUE
  tryCatch(readLines(url(repos)),
    error = function(e) cran_ok <<- FALSE,
    warning = function(e) {}
  )
  if (is.null(repos$repos) || !cran_ok) {
    repos <- c(CRAN = "https://cloud.r-project.org/")
  }
  for (p in c(
#    "utf8", # what for?
    "bench",
    #"backports",
    "checkmate", # imported by comorbidity
    #"magrittr", # no need
    #"parallel", # base package imported by 'comorbidity'
    "plyr", "reshape2", "hash", # imported by 'medicalrisk'
    # "testthat", # not testing any packages here
    # "knitr", # not building any vignettes
    # "profmem", # do someday
    "Rcpp",
    "RcppEigen",
    # "tidyr", # really?
    #    "comorbidity", # CRAN version has incompatible updates
    #    "medicalrisk", # CRAN version from 2016 unchanged at time of submission
    NULL
  )) {
    if (!require(p,
      character.only = TRUE,
      quietly = TRUE,
      warn.conflicts = FALSE
    )) {
      install.packages(p, character.only = TRUE, repos = repos)
    }
    library(p,
      character.only =TRUE,
      quietly = TRUE, warn.conflicts = FALSE
    )
  }
  message("Installing medicalrisk from tar.gz")
  install.packages("medicalrisk_1.2.tar.gz", repos = NULL)
  message("Installing comorbidity from tar.gz")
  install.packages("comorbidity_0.1.1.tar.gz", repos = NULL)
  # This relies on icd not already being loaded, or it will pass with any version
  if (!requireNamespace("icd",
    quietly = TRUE,
    versionCheck = list(
      version = "3.2.2",
      op = ">="
    )
  )) {
    message("icd not yet installed, so installing from CRAN")
    if ("icd" %in% available.packages()["Package"]) {
        install.packages("icd", repos = repos)
    } else {
        message("icd does not seem to be available in current repos. Installing from source")
        system2("../../tools/install-quick.sh") 
    }

  }
  library("icd", quietly = TRUE)
  # create the .deps file so Makefile knows we are done
  if (!file.exists(".deps")) {
    file.create(".deps", showWarnings = FALSE)
  }
  invisible()
}

install_jss3447_deps()
