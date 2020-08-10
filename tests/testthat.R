# this is common code to all the tests, each of which runs test_check with a
# different filter:
library("icd")
library("testthat", warn.conflicts = FALSE, quietly = TRUE)
message("initial options")
icd:::.show_options()
# Definitely don't download data on CRAN
if (!icd:::.env_var_is_true("NOT_CRAN")) {
  old_cran <- options(
    "icd.offline" = TRUE,
    "icd.test_slow" = FALSE,
    "icd.interact" = FALSE
  )
  on.exit(options(old_cran), add = TRUE)
} else {
  old_not_cran <- options(
    "icd.offline" = !icd:::env_var_is_false("ICD_OFFLINE"),
    "icd.test_slow" = icd:::env_var_is_true("ICD_TEST_SLOW"),
    "icd.interact" = !icd::env_var_is_false("interact")
  )
  on.exit(options(old_not_cran), add = TRUE)
}
message("pre-test options")
icd:::.show_options()
testthat::test_check("icd")
message("post-test options")
icd:::.show_options()
