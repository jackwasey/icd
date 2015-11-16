context("test we got WHO ICD-10 codes okay")

if (!exists("icd10_who", where = paste0("package:", getPackageName())))
  testthat::skip()

  test_that("some tricky looking codes", {
    expect_true("A00" %in% icd10_who["major"])
  })


