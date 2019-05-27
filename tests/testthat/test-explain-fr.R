context("explain France codes")

test_that("can explain some codes", {
  skip_missing_icd10fr()
  expect_false(is.null(explain_code(as.icd10fr("A00"))))
  expect_false(is.null(explain_code(as.icd10fr("Z998"))))
  expect_identical(explain_code(as.icd10fr("B04")), "Monkeypox")
})
