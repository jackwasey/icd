context("attributes")

test_that("attribute is set by different mechanisms", {
  j <- "100"
  expect_false(is.short_diag(j))
  expect_false(is.decimal_diag(j))
  attr(j, "icd_short_diag") <- TRUE
  expect_true(is.short_diag(j))
  expect_false(is.decimal_diag(j))
  attr(j, "icd_short_diag") <- FALSE
  expect_false(is.short_diag(j))
  expect_true(is.decimal_diag(j))
  icd:::attr_short_diag(j)
  expect_true(is.short_diag(j))
  expect_false(is.decimal_diag(j))
  icd:::attr_decimal_diag(j)
  expect_false(is.short_diag(j))
  expect_true(is.decimal_diag(j))

  k <- as.decimal_diag("222")
  expect_false(attr(k, "icd_short_diag"))
  k <- as.short_diag("753")
  expect_true(attr(k, "icd_short_diag"))

})


test_that("attribute is set by different mechanisms with icd_ functions", {
  j <- "100"
  expect_false(is.icd_short_diag(j))
  expect_false(is.icd_decimal_diag(j))
  attr(j, "icd_short_diag") <- TRUE
  expect_true(is.icd_short_diag(j))
  expect_false(is.icd_decimal_diag(j))
  attr(j, "icd_short_diag") <- FALSE
  expect_false(is.icd_short_diag(j))
  expect_true(is.icd_decimal_diag(j))
  icd:::attr_short_diag(j)
  expect_true(is.icd_short_diag(j))
  expect_false(is.icd_decimal_diag(j))
  icd:::attr_decimal_diag(j)
  expect_false(is.icd_short_diag(j))
  expect_true(is.icd_decimal_diag(j))

  k <- as.icd_decimal_diag("222")
  expect_false(attr(k, "icd_short_diag"))
  k <- as.icd_short_diag("753")
  expect_true(attr(k, "icd_short_diag"))

})
