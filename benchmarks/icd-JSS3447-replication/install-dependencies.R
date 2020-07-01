#' Carefully and non-intrusively install dependencies for benchmarking
#'
#' Installs dependencies needed for benchmarking in a temporary library, not
#' assuming anything, including whether or not a CRAN mirror has been selected.
#' This should allow benchmarking in a docker container, an old Windows machine,
#' vanilla R session, or user account without administrative privileges on a
#' server.
#'
#' source this file, since not in package \code{R/} then call the function.
#'
#' This needs testing on multiple platforms, with busy or stripped down R
#' environments. Vanilla, no pre-loaded packages, repos option set or unset or wrong:
#' I want the benchmarks to run really robustly, and without modifying the user's
#' installed R libraries or environment, or session options.
#' @keywords internal
#' @noRd
install_jss3447_deps <- function(lib.loc, depsdone_file = ".depsdone.txt") {
  repos <- getOption("repos")
  repo_ok <- TRUE
  # I do not wish to prompt user, e.g. if repos = "@CRAN@"
  if (!any(repos == "@CRAN@")) {
    message("@CRAN@ found in repos: will use cloud.r-project.org temporarily")
    repo_ok <- FALSE
  }

  for (r in repos) {
    repo_con <- url(r)
    tryCatch(
      {
        readLines(repo_con)
        if (isOpen(repo_con)) close(repo_con)
      },
      error = function(e) {
        warning(
          "repo: ",
          r,
          " is not accessible. Will try later with cloud.r-project.org"
        )
        repo_ok <<- FALSE
      },
      warning = function(e) {}
    )
    close(repo_con)
  }
  if (!repo_ok ||
    is.null(repos) ||
    length(repos) == 0L
  ) {
    repos <- c(
      CRAN = "https://cloud.r-project.org/",
      CRAN_http = "http://cloud.r-project.org/"
    )
  }

  for (p in c(
    "bench",
    # "backports",
    "checkmate",
    # imported by comorbidity
    # "magrittr", # no need?
    "parallel",
    # used by 'comorbidity' for maximum speed
    "plyr",
    "reshape2",
    "hash",
    # imported by 'medicalrisk'
    # "profmem", # bench package does this when capabilities("profmem") is TRUE
    "Rcpp",
    "RcppEigen",
    #    "comorbidity", # CRAN version has incompatible updates
    #    "medicalrisk", # CRAN version from 2016 unchanged at time of submission
    NULL
  )) {
    suppressWarnings({
      have_p <- find.package(p, lib.loc = lib.loc, quiet = TRUE)
    })
    if (length(have_p) == 0L) {
      install.packages(p,
        character.only = TRUE,
        repos = repos,
        lib = lib.loc
      )
    }
    suppressPackageStartupMessages(library(
      p,
      character.only = TRUE,
      quietly = TRUE,
      warn.conflicts = FALSE,
      lib.loc = lib.loc
    ))
  }
  message("Installing medicalrisk from tar.gz to ", lib.loc)
  have_medicalrisk <- identical(find.package("medicalrisk", lib.loc = lib.loc, quiet = TRUE), character())
  if (!have_medicalrisk) install.packages("medicalrisk_1.2.tar.gz", repos = NULL, lib = lib.loc)
  message("Installing comorbidity from tar.gz to ", lib.loc)
  have_comorbidity <- identical(find.package("comorbidity", lib.loc = lib.loc, quiet = TRUE), character())
  if (!have_comorbidity) {
    install.packages("comorbidity_0.1.1.tar.gz",
      repos = NULL,
      lib = lib.loc
    )
  }
  # This relies on 'icd' not already being loaded, or it will pass with any version
  if (!requireNamespace(
    "icd",
    quietly = TRUE,
    lib.loc = lib.loc,
    versionCheck = list(
      version = "4.0.8",
      op = ">="
    )
  )) {
    message("icd of sufficient recency not yet installed, so installing from local source package")
    if ("icd" %in% available.packages()["Package"]) {
      stop("WIP - build a slim - no documentation source package?")
      install.packages("icd", quiet = TRUE, repos = repos)
    } else {
      message("icd does not seem to be available in current repos. Installing from this source")
      Sys.setenv("ICD_HOME" = normalizePath("../.."))
      # TODO: use or rely on Makefile to do this better
      system2("bash", "../../tools/install-quick.sh")
    }
  }
  library("icd", quietly = TRUE)
  # re-create the .deps file so Makefile knows that we are done
  if (file.exists(depsdone_file)) {
    if (file.size(depsdone_file) != 0L) {
      warning("dependency empty file flag is not empty! Not deleting.")
    } else {
      unlink(depsdone_file,
        force = TRUE,
        recursive = FALSE,
        expand = FALSE
      )
    }
  }
  file.create(depsdone_file, showWarnings = TRUE)
  invisible(NULL)
}

if (trimws(tolower(Sys.getenv("ICD_INSTALL_BENCH_DEPS_ON_SOURCE"))) %in% c("", "no", "false", "f", "no", "n", "0")) {
  if (!(trimws(tolower(Sys.getenv("ICD_VERBOSE"))) %in% c("", "no", "false", "f", "no", "n", "0"))) {
    message("ICD_INSTALL_BENCH_DEPS_ON_SOURCEenvironment variable is not set, so not installing benchmarks dependencies. Use install_jss3447_deps() to do this.")
  }
} else {
  # TODO: if we are triggered by environment variable, then use environment
  # variables, if set, to parameterize the benchmarking.
  install_jss3447_deps()
}
