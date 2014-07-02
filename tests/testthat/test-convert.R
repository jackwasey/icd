context("icd9 type conversions")

test_that("icd9 decimal to short form", {

  expect_equal(icd9DecimalToShort("1"), "001")
  expect_equal(icd9DecimalToShort("1.1"), "0011")
  expect_equal(icd9DecimalToShort("1.23"), "00123")
  expect_equal(icd9DecimalToShort("81"), "081")
  expect_equal(icd9DecimalToShort("81.1"), "0811")
  expect_equal(icd9DecimalToShort("81.23"), "08123")
  expect_equal(icd9DecimalToShort("991"), "991")
  expect_equal(icd9DecimalToShort("991.1"), "9911")
  expect_equal(icd9DecimalToShort("991.23"), "99123")

  expect_equal(icd9DecimalToShort(c("1", "991.23")), c("001", "99123"))
  expect_equal(icd9DecimalToShort(c("1.", "991.23")), c("001", "99123"))
  expect_equal(icd9DecimalToShort(c("1", NA, "991.23")), c("001", NA, "99123"))
  expect_equal(icd9DecimalToShort(c("1", "g", "", "991.23")), c("001", NA, NA, "99123"))

  expect_error(icd9DecimalToShort(1))
  expect_error(icd9DecimalToShort(22))
  expect_error(icd9DecimalToShort(333))
  expect_error(icd9DecimalToShort(1.9))
  expect_error(icd9DecimalToShort(22.8))
  expect_error(icd9DecimalToShort(333.7))
  expect_error(icd9DecimalToShort(1.98))
  expect_error(icd9DecimalToShort(22.76))
  expect_error(icd9DecimalToShort(333.54))

  expect_error(icd9DecimalToShort(c("07022","07023"), validate = TRUE))

})
