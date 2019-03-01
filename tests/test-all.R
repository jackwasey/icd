# this is common code to all the tests, each of which runs test_check with a different filter:
library("icd")
library("icd.data")
library("testthat", warn.conflicts = FALSE, quietly = TRUE)
# Set ICD_TEST_SLOW to yes/true to run slow tests
testthat::test_check("icd")
