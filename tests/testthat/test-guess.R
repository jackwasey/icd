context("guesss ICD type and version")

test_that("tricky ICD-10 codes", {
  expect_equal(icd_guess_version("C7B00"), "icd10")
  expect_equal(icd_guess_version("C7A024"), "icd10")
})

