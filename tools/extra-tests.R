
check_local_gctorture <- function(filter = ".*") {
  with_split_tests(
    rcmdcheck::rcmdcheck(args = c("--use-gct", check_args))
  )
}

# THIS DOESN'T WORK! Eigen spews out warnings when RcppEigen itself is installed, not when icd is installed.
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

