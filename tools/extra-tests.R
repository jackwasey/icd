#!/usr/bin/env Rscript

lint_extra <- function(path = getwd()) {
  requireNamespace("lintr")
  not_pkg_root <- function(p) !file.exists(file.path(p, "DESCRIPTION"))
  ex <- c(list("R/z-deprecated.R", "R/RcppExports.R", "R/benchmark.R"),
          list.files(path = "inst/doc", recursive = TRUE, full.names = TRUE))
  more_linters <- lintr::with_defaults(
    object_name_linter = NULL, object_usage_linter =
      NULL, cyclocomp_linter = NULL, line_length_linter(90),
    object_length_linter(length = 36L), undesirable_function_linter("sapply"),
    trailing_whitespace_linter = NULL, todo_comment_linter,
    extraction_operator_linter, T_and_F_symbol_linter, trailing_semicolons_linter,
    unneeded_concatenation_linter, undesirable_operator_linter,
    absolute_paths_linter, absolute_path_linter, implicit_integer_linter,
    semicolon_terminator_linter, undesirable_function_linter,
    nonportable_path_linter
  )
  old_wd <- setwd(path)
  on.exit(setwd(old_wd))
  if (not_pkg_root(path)) path <- dirname(path)
  if (not_pkg_root(path)) path <- dirname(path)
  if (not_pkg_root(path)) path <- dirname(path)
  if (not_pkg_root(path)) stop("Cannot find package root start from ", old_wd)
  lintr::lint_package(path = path, relative_path = FALSE, exclusions = ex, linters = more_linters)
}

release_sanity_checks <- function() {
  message("superficially checking data download before attaching 'icd' with library")
  icd::download_all_icd_data()

  suppressPackageStartupMessages(library("icd", quietly = TRUE, warn.conflicts = FALSE))
  message("superficially checking data download again after attaching 'icd'")
  icd::download_all_icd_data()
  message("codetools")
  codetools::checkUsagePackage("icd", all = TRUE, suppressLocal = TRUE)
  message("Missing NAMESPACE S3")
  s <- devtools::missing_s3()
  if (length(s)) print(s)
  message("style check")
  # exclude R_user_dir.R so easier to diff with upstream
  styler::style_pkg(exclude_files = c("R/RcppExports.R", "R/R_user_dir.R"), dry = "fail")
  message("spelling")
  aspell_package_Rd_files(".")
}

check_rhub_no_prebuilt_vign <- function(
                                        check_fun = rhub::check_for_cran,
                                        check_fun_args = list()) {
  td <- tempfile("icd-no-static-vignettes")
  dir.create(td)
  message("Using temp dir: ", td)
  ih <- Sys.getenv("ICD_HOME")
  if (ih == "") ih <- file.path("~", "icd")
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
