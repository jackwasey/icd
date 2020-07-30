context("filtering on ICD-10 validity")

pts <- icd10_all_ahrq
pts[1, "icd10_code"] <- "invalid"

test_that("filter an invalid row", {
  res <- expect_warning(icd10_filter_invalid(pts), regexp = NA)
  expect_is(res, "data.frame")
})
