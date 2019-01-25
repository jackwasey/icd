context("billable code lists")

test_that("specific known parsing errors", {
  # problems with whitespace stripping or sorting around: 12167 13276 14567
  b32 <- icd.data::icd9cm_billable[["32"]]
  nines <- b32[b32$code == "9999", ]
  expect_equal(nrow(nines), 1)
  expect_equal(nines$long_desc, paste("Other and unspecified complications of",
                                      "medical care, not elsewhere classified"))
})

test_that("billable codes for expected versions exist", {
  expect_true(all(as.character(23:32) %in% names(icd.data::icd9cm_billable)))
  expect_true(all(vapply(icd.data::icd9cm_billable, is.data.frame, logical(1))))
})

test_that("billable codes are all in order", {
  testthat::skip_on_cran()
  for (v in names(icd.data::icd9cm_billable)) {
    i <- icd.data::icd9cm_billable[[v]][["code"]]
    expect_identical(i, sort_icd.icd9(i, short_code = TRUE),
                     info = paste("version = ", v))
  }
})

test_that("parsing 27 gives zero-padded digit icd9 codes", {
  expect_equal(icd.data::icd9cm_billable[["27"]][1, "code"], "0010")
})
