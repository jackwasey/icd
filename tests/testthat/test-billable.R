context("billable code lists")

# cover the reverse dependency on icd 3.3
test_that("billable codes for expected versions exist", {
  skip("this is a breaking change in icd.data.")
  expect_true(all(as.character(23:32) %in% names(icd9cm_billable)))
  expect_true(all(vapply(icd9cm_billable, is.data.frame, logical(1))))
})

# also to ensure icd 3.3 tests without error
test_that("parsing 27 gives zero-padded digit icd9 codes", {
  skip("this is a breaking change in icd.data.")
  expect_equal(icd9cm_billable[["27"]][1, "code"], "0010")
})

test_that("specific known parsing errors", {
  skip("this is a breaking change in icd.data.")
  # problems with whitespace stripping or sorting around: 12167 13276 14567
  b32 <- icd_data_icd9cm_leaf_v32()
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
