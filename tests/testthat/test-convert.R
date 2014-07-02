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

test_that("short to decimal, leading zeroes flag", {
  expect_equal(icd9DecimalToShort("1.0", leadingZeroes = TRUE), "0010") # if there is anything after decimal, zeroes must be there!
  expect_equal(icd9DecimalToShort("1.0", leadingZeroes = FALSE), "0010")
  expect_equal(icd9DecimalToShort("1", leadingZeroes = TRUE), "001")
  expect_equal(icd9DecimalToShort("1", leadingZeroes = FALSE), "1")
  expect_equal(icd9DecimalToShort("22", leadingZeroes = TRUE), "022")
  expect_equal(icd9DecimalToShort("22", leadingZeroes = FALSE), "22")
  expect_equal(icd9DecimalToShort("345", leadingZeroes = TRUE), "345")
  expect_equal(icd9DecimalToShort("345", leadingZeroes = FALSE), "345")
})

test_that("short to decimal with flags", {
  #TODO more permutations to expand here:
  expect_equal(icd9ShortToDecimal("013"), "13")
  expect_equal(icd9ShortToDecimal("013", leadingZeroes = TRUE), "013")
  expect_equal(icd9ShortToDecimal("V013", leadingZeroes = TRUE), "V01.3")
  expect_equal(icd9ShortToDecimal("013", leadingZeroes = TRUE, keepLoneDecimal = TRUE), "013.")
  expect_equal(icd9ShortToDecimal("V01", leadingZeroes = TRUE, keepLoneDecimal = TRUE), "V01.")
  expect_equal(icd9ShortToDecimal("V01", keepLoneDecimal = TRUE), "V1.")
  expect_equal(icd9ShortToDecimal("013", keepLoneDecimal = TRUE), "13.")

})

test_that("running short to decimal conversion before and after expansion of a ICD-9 base codes gives the same result", {

  #bad codes:
  expect_equal(icd9ShortToDecimal("valsalva", invalidAction = "silent"), NA_character_)
  expect_equal(icd9ShortToDecimal("123456", invalidAction = "silent"), NA_character_)
  expect_equal(icd9ShortToDecimal("", invalidAction = "silent"), NA_character_)
  expect_equal(icd9ShortToDecimal("-1", invalidAction = "silent"), NA_character_)
  expect_error(icd9ShortToDecimal("-1", invalidAction = "stop"))
  expect_error(icd9ShortToDecimal(NA, invalidAction = "silent")) # NA is not character type, so expect error.
  expect_error(icd9ShortToDecimal(NA, invalidAction = "ignore")) # NA is not character type, so expect error.
  expect_equal(icd9ShortToDecimal(c("000000", "0ab1bc2d"), invalidAction = "silent"), c(NA_character_, NA_character_))
  expect_error(icd9ShortToDecimal("valsalva", invalidAction = "stop"))
  expect_error(icd9ShortToDecimal("123456", invalidAction = "stop"))
  expect_error(icd9ShortToDecimal("", invalidAction = "stop"))
  expect_error(icd9ShortToDecimal("-1", invalidAction = "stop"))
  expect_error(icd9ShortToDecimal(NA, invalidAction = "stop"))
  expect_error(icd9ShortToDecimal(c("000000", "0ab1bc2d"), invalidAction = "stop"))
  expect_error(icd9ShortToDecimal(c("123", "0ab1bc2d"), invalidAction = "stop")) # first is valid

  expect_equal(icd9ShortToDecimal("V013"), "V1.3")

  icd9List <- ahrqComorbid #todo SUBSET OR EXTRA MAPPINGS?
  for (i in names(icd9List)) {
    expect_equal(
      icd9DecimalToShort(icd9ShortToDecimal(icd9List[[i]], leadingZeroes = TRUE), leadingZeroes = TRUE),
      icd9AddLeadingZeroesShort(icd9List[[i]]),
      info = paste("in loop:", i)
    )
  }

  n = 250
  randomDecimalIcd9 <- paste(
    round(runif(min = 1, max = 999, n = n)),
    sample(icd9ExpandMinor(), replace = TRUE, size = n),
    sep = "."
  )
  # keep the decimal point just because that is how we created the test data.
  expect_equal(icd9ShortToDecimal(icd9DecimalToShort(randomDecimalIcd9), keepLoneDecimal = TRUE), randomDecimalIcd9)
  # test without decimal, too...
  rd2 <- as.character(round(runif(min = 1, max = 999, n = n)))
  expect_equal(icd9ShortToDecimal(icd9DecimalToShort(rd2), keepLoneDecimal = FALSE), rd2)

})
