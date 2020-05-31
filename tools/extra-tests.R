#!/usr/bin/env Rscript

release_sanity_checks <- function() {
  message("superficially checking data download before attaching 'icd' with library")
  icd::download_all_icd_data()

  suppressPackageStartupMessages(library("icd", quietly = TRUE, warn.conflicts = FALSE))
  message("superficially checking data download again after attaching 'icd'")
  icd::download_all_icd_data()
  message("codetools")
  codetools::checkUsagePackage('icd', all = TRUE, suppressLocal = TRUE)
  message("Missing NAMESPACE S3")
  s <- devtools::missing_s3(); if (length(s)) s
  message("style check")
  # exclude R_user_dir.R so easier to diff with upstream
  styler::style_pkg(exclude_files = c("R/RcppExports.R", "R/R_user_dir.R"), dry = "fail")
message("spelling")
aspell_package_Rd_files('.')
}

check_rhub_no_prebuilt_vign <- function(
                                      check_fun = rhub::check_for_cran,
                                      check_fun_args = list()) {
  td <- tempfile("icd-no-static-vignettes")
  dir.create(td)
  message("Using temp dir: ", td)
  ih <- Sys.getenv("ICD_HOME")
  if (ih == "") ih <- file.path("~","icd")
  file.copy(ih, td, recursive = TRUE, overwrite = TRUE)
  icd_tmp_path <- file.path(td, "icd")
  setwd(icd_tmp_path)
  cat("^vignettes\\/.*\\.Rnw$",
    fill = TRUE,
    append = TRUE,
    file = file.path(icd_tmp_path, ".Rbuildignore")
  )
  do.call(check_fun, check_fun_args)
  message("Completed check using tree at: ", td)
}

check_local_gctorture <- function(filter = ".*", args = character()) {
  with_split_tests(
    rcmdcheck::rcmdcheck(args = c("--use-gct", args = args))
  )
}

# THIS DOESN'T WORK! Eigen spews out warnings when RcppEigen itself is
# installed, not when icd is installed.
check_rhub_valgrind <- function(fun = rhub::check_with_valgrind,
                                filter = ".*") {
  shutup <-
    paste0(
      "CXX11FLAGS=",
      paste0(
        "-Wno-",
        c(
          "unused-parameter",
          "unused-variable",
          "ignored-attributes",
          "cast-function-type",
          "unknown-pragmas",
          "unknown-warning-option",
          "unknown-warning"
        ),
        collapse = " "
      )
    )
  with_split_tests(
    fun(
      check_args = paste(
        "--no-vignettes",
        "--no-build-vignettes",
        "--ignore-vignettes",
        paste0("--install-args='--configure-vars=\"", shutup, "\"'")
      )
    )
  )
}
