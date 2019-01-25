library("icd")
library("icd.data")
library("testthat", warn.conflicts = FALSE, quietly = TRUE)
library("magrittr", warn.conflicts = FALSE, quietly = TRUE)
if (tolower(Sys.getenv("ICD_TEST_BUILD_DATA")) %in% c("yes", "true", "1"))
  testthat::test_dir("tests-build-data",
                     env = icd:::test_env(),
                     reporter = testthat::CheckReporter)
