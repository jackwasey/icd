# this is common code to all the tests, each of which runs test_check with a
# different filter:
library("icd")
library("testthat", warn.conflicts = FALSE, quietly = TRUE)
# Definitely don't download data on CRAN
if (!icd:::.env_var_is_true("NOT_CRAN")) {
  old_offline <- options("icd.data.offline" = TRUE)
  on.exit(options(old_offline), add = TRUE)
}
if (icd:::.env_var_is_true("ICD_DATA_TEST_SLOW")) {
  old_test_slow <- options("icd.data.test_slow" = TRUE)
  on.exit(options(old_test_slow), add = TRUE)
}
old_interact <- options("icd.data.interact" = FALSE)
on.exit(options(old_interact), add = TRUE)
icd:::.show_options()
stopifnot(
  with_icd10cm_version(
    ver = "2014",
    code = icd:::get_icd10cm_active_ver() == "2014"
  )
)
testthat::test_check("icd")
