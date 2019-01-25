# this is common code to all the tests, each of which runs test_check with a different filter:
library("icd")
library("icd.data")
library("testthat", warn.conflicts = FALSE, quietly = TRUE)
if (!tolower(Sys.getenv("ICD_TEST_ALL")) %in% c("no", "false"))
  testthat::test_check("icd")
