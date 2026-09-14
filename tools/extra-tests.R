#!/usr/bin/env Rscript

wd_icd <- function() {
  icd_home <- Sys.getenv("ICD_HOME", unset = NA_character_)
  if (is.na(icd_home)) {
    warning(
      "Shell environment variable ICD_HOME is unset. ",
      "Set to top of working set to avoid ambiguity. E.g.:"
    )
    message(
      "Sys.setenv(ICD_HOME = Sys.getenv(\"ICD_HOME\")\""
    )
    message(
      "Sys.setenv(ICD_HOME = Sys.getenv(\"ICD_HOME\")\""
    )
    warning(
      "Setting working directory to top of this package source tree, ",
      "if possible"
    )
    devtools::wd()
  } else {
    message("PWD=", getwd())
    message("ICD_HOME=", icd_home)
    setwd(icd_home)
  }
  invisible(icd_home)
}

release_sanity_checks <- function() {
  message("superficially checking data download before and after attaching")
  icd::download_all_icd_data()
  suppressPackageStartupMessages(
    library(
      "icd",
      quietly = TRUE,
      warn.conflicts = FALSE
    )
  )
  download_all_icd_data()

  wd_icd()

  message("style check")
  # exclude R_user_dir.R so easier to diff with upstream
  styler::style_pkg(
    exclude_dirs = c("icd.Rcheck"),
    exclude_files = c(
      "R/RcppExports.R",
      "R/R_user_dir.R"
    ),
    dry = "fail"
  )

  message("codetools on everything")
  codetools::checkUsagePackage("icd",
                               all = TRUE,
                               suppressLocal = TRUE)

  message("Missing NAMESPACE S3")
  s <- devtools::missing_s3()
  if (length(s)) s


  message("spelling")
  aspell_package_Rd_files(".")
}

check_local_gctorture <- function(filter = ".*", args = character()) {
  wd_icd()
  with_split_tests(
    rcmdcheck::rcmdcheck(args = c("--use-gct", args = args))
  )
}
