# this is common code to all the tests, each of which runs test_check with a
# different filter:
library("icd")
library("testthat", warn.conflicts = FALSE, quietly = TRUE)
icd:::.show_options()
# Definitely don't download data on CRAN
if (!icd:::.env_var_is_true("NOT_CRAN")) {
  old_offline <- options("icd.offline" = TRUE)
  on.exit(options(old_offline), add = TRUE)
} else {
  old_offline <- options("icd.offline" = FALSE)
  on.exit(options(old_offline), add = TRUE)
  if (icd:::.env_var_is_true("ICD_TEST_SLOW")) {
    old_test_slow <- options("icd.test_slow" = TRUE)
    on.exit(options(old_test_slow), add = TRUE)
  }
}
old_interact <- options("icd.interact" = FALSE)
on.exit(options(old_interact), add = TRUE)
icd:::.show_options()
testthat::test_check("icd")
icd:::.show_options()
