context("ICD-10-BE")

test_that("basic Belgian", {
  skip_missing_dat("icd10be2014")
  skip_missing_dat("icd10be2017")
  expect_true(inherits(get_icd10be2014()$code, "icd10be"))
  expect_true(inherits(get_icd10be2017()$code, "icd10be"))
})
