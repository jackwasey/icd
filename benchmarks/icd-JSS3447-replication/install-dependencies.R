install_jss3447_deps <- function() {
  old_repos <- options("repos")
  on.exit(options(old_repos), add = TRUE)
  # don't assume anything, and I do not wish to prompt user, e.g. if repos = "@CRAN@"
  repos <- old_repos["repos"]
  repo_ok <- TRUE
  if (any(repos == "@CRAN@")) {
    repo_ok <- FALSE
  }

  tryCatch(readLines(url(repos)),
    error = function(e) repo_ok <<- FALSE,
    warning = function(e) {}
  )
  if (is.null(repos$repos) ||
    length(repos$repos) == 1 ||
    !repo_ok
  ) {
    repos <- c(
      CRAN = "https://cloud.r-project.org/",
      CRAN_http = "http://cloud.r-project.org/"
    )
    options("repos" = repos)
  }
  for (p in c(
    #    "utf8", # what for?
    "bench",
    # "backports",
    "checkmate", # imported by comorbidity
    # "magrittr", # no need?
    "parallel", # used by 'comorbidity' for maximum speed
    "plyr", "reshape2", "hash", # imported by 'medicalrisk'
    # "profmem", # do someday
    "Rcpp",
    "RcppEigen",
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
      character.only = TRUE,
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
    message("icd of sufficient recency not yet installed, so installing from local source package")
    if ("icd" %in% available.packages()["Package"]) {
      stop("WIP - build a slim - no documentation source package")
      install.packages("icd", quiet = TRUE, repos = repos)
    } else {
      message("icd does not seem to be available in current repos. Installing from source")
      icd_home_path <- normalizePath("../..")
      Sys.setenv("ICD_HOME" = icd_home_path)
      system2("bash", "../../tools/install-quick.sh") # , env = c(ICD_HOME = icd_home_path))
    }
  }
  library("icd", quietly = TRUE)
  # re-create the .deps file so Makefile knows that we are done
  if (file.exists(".deps")) {
    unlink(".deps", force = TRUE, recursive = FALSE, expand = FALSE)
  }
  file.create(".deps", showWarnings = FALSE)
  invisible(NULL)
}

install_jss3447_deps()
