context("billable code lists")

test_that("specific known parsing errors", {
  # problems with whitespace stripping or sorting around: 12167 13276 14567
  b32 <- icd.data::icd9cm_leaf_v32
  nines <- b32[b32$code == "9999", ]
  expect_equal(nrow(nines), 1)
  expect_equal(
    nines$long_desc,
    paste(
      "Other and unspecified complications of",
      "medical care, not elsewhere classified"
    )
  )
})

test_that("billable codes are all in order", {
  i <- icd.data::icd9cm_leaf_v32[["code"]]
  expect_identical(i, sort.icd9cm(i, short_code = TRUE))
})
