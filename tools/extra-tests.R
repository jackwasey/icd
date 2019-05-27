
check_no_static_vignettes <- function(
  check_fun = rhub::check_for_cran,
  check_fun_args = list()) {
  td <- tempfile("icd-no-static-vignettes")
  dir.create(td)
  message("Using temp dir: ", td)
  file.copy("~/icd", td, recursive = TRUE, overwrite = TRUE)
  setwd(icd_tmp_path <- file.path(td, "icd"))
  cat('^vignettes\\/.*-vignette\\.Rnw$',
      fill = TRUE,
      append = TRUE,
      file = file.path(icd_tmp_path, ".Rbuildignore")
  )
  do.call(check_fun, check_fun_args)
  message("Completed check using tree at: ", td)
}

check_local_gctorture <- function(filter = ".*") {
  with_split_tests(
    rcmdcheck::rcmdcheck(args = c("--use-gct", check_args))
  )
}

# THIS DOESN'T WORK! Eigen spews out warnings when RcppEigen itself is
# installed, not when icd is installed.
check_rhub_valgrind_quiet <- function(
  fun = rhub::check_with_valgrind,
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

