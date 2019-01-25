library("icd")
library("icd.data")
library("testthat", warn.conflicts = FALSE, quietly = TRUE)
library("magrittr", warn.conflicts = FALSE, quietly = TRUE)
if (tolower(Sys.getenv("ICD_TEST_SLOW")) %in% c("yes", "true", "1"))
  testthat::test_dir("tests-slow",
                     env = icd:::test_env(),
                     reporter = testthat::CheckReporter)
