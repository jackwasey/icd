context("WHO ICD-10 codes were fetched")

test_that("some tricky looking codes", {
  if (!exists("icd10_who", where = paste0("package:", methods::getPackageName())))
    testthat::skip("don't have WHO codes to test. Run function to download them.")

  expect_true("A00" %in% icd10_who["major"])
})
