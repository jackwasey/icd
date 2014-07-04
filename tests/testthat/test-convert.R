context("icd9 type conversions")

test_that("extract decimal parts - invalid or empty input", {
  expect_equal(icd9DecimalToParts(character()), data.frame(major = character(), minor = character()))

  expect_equal(
    icd9DecimalToParts(""),
    data.frame(major = "", minor = "", stringsAsFactors = FALSE)
  )

  expect_equal(
    icd9DecimalToParts("", minorEmpty = NA_character_),
    data.frame(major = "", minor = NA_character_, stringsAsFactors = FALSE)
  )

  # empty input gives empty output, not error.
  expect_that(icd9DecimalToParts(character(), invalidAction = "stop"), not(throws_error()))
  expect_warning(icd9DecimalToParts("", invalidAction = "warn"))
})

test_that("extract decimal parts - valid inputs", {
  # zero is technically "valid", means no code. TODO: apply elsewhere?
  expect_equal(icd9DecimalToParts("0"), data.frame(major = "0", minor = "", stringsAsFactors = FALSE))
  expect_equal(icd9DecimalToParts("000"), data.frame(major = "000", minor = "", stringsAsFactors = FALSE))
  expect_equal(icd9DecimalToParts("V1.2"), data.frame(major = "V1", minor = "2", stringsAsFactors = FALSE))
  expect_equal(icd9DecimalToParts("1.1"), data.frame(major = "1", minor = "1", stringsAsFactors = FALSE))
  expect_equal(icd9DecimalToParts("001.1"), data.frame(major = "001", minor = "1", stringsAsFactors = FALSE))
  expect_equal(icd9DecimalToParts("22.22"), data.frame(major = "22", minor = "22", stringsAsFactors = FALSE))
  expect_equal(icd9DecimalToParts("022.22"), data.frame(major = "022", minor = "22", stringsAsFactors = FALSE))
  expect_equal(icd9DecimalToParts("333.3"), data.frame(major = "333", minor = "3", stringsAsFactors = FALSE))
  expect_equal(icd9DecimalToParts("444"), data.frame(major = "444", minor = "", stringsAsFactors = FALSE))
  expect_equal(icd9DecimalToParts("444", minorEmpty=NA_character_), data.frame(major = "444", minor=NA_character_, stringsAsFactors = FALSE))
  expect_equal(icd9DecimalToParts("444", minorEmpty = ""), data.frame(major = "444", minor = "", stringsAsFactors = FALSE))
  expect_equal(icd9DecimalToParts(c("9.9", "88.88", "777.6")),
               data.frame(
                 major = c("9", "88", "777"),
                 minor = c("9", "88", "6"),
                 stringsAsFactors = FALSE
               )
  )

  expect_equal(icd9DecimalToParts(c("009.9", "088.88", "777.6")),
               data.frame(
                 major = c("009", "088", "777"),
                 minor = c("9", "88", "6"),
                 stringsAsFactors = FALSE
               )
  )
  expect_equal(icd9DecimalToParts(c("9.9", "88", "777.6"), minorEmpty = NA),
               data.frame(
                 major = c("9", "88", "777"),
                 minor = c("9", NA, "6"),
                 stringsAsFactors = FALSE
               )
  )

  expect_equal(icd9DecimalToParts(c("9.9", "88", "777.6"), minorEmpty = ""),
               data.frame(
                 major = c("9", "88", "777"),
                 minor = c("9", "", "6"),
                 stringsAsFactors = FALSE
               )
  )

  expect_equal(icd9DecimalToParts(c("01", "g", "", "991.23"), invalidAction = "silent", minorEmpty = NA),
               data.frame(
                 major = c("01", NA, NA, "991"),
                 minor = c(NA, NA, NA, "23"),
                 stringsAsFactors = FALSE
               )
  )
  # make minorEmpty work even if not validating codes
  expect_equal(icd9DecimalToParts(c("001", "g", "", "991.23"), invalidAction = "ignore", minorEmpty = NA),
               data.frame(
                 major = c("001", "g", "", "991"),
                 minor = c(NA, NA, NA, "23"),
                 stringsAsFactors = FALSE
               )
  )
})

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
  expect_equal(icd9DecimalToShort(c("1", "g", "", "991.23")), c("001", "g", "", "99123")) # default to 'ignore'
  expect_equal(icd9DecimalToShort(c("1", "g", "", "991.23"), invalidAction = "ignore"), c("001", "g", "", "99123"))
  expect_equal(icd9DecimalToShort(c("1", "g", "", "991.23"), invalidAction = "silent"), c("001", NA, NA, "99123"))
  expect_warning(icd9DecimalToShort(c("1", "g", "", "991.23"), invalidAction = "warn")) # we should warn for any invalid input, not just one item.
  expect_error(icd9DecimalToShort(c("1", "g", "", "991.23"), invalidAction = "stop")) # we should stop for any invalid input, not just one item.

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

test_that("short to decimal, numbers", {
  expect_equal(icd9DecimalToShort("1.0"), "0010") # if there is anything after decimal, zeroes must be there!
  expect_equal(icd9DecimalToShort("1"), "001")
  expect_equal(icd9DecimalToShort("22"), "022")
  expect_equal(icd9DecimalToShort("345"), "345")
})

test_that("short to decimal with flags", {
  #TODO more permutations to expand here:
  expect_equal(icd9ShortToDecimal("013"), "013")
  expect_equal(icd9ShortToDecimal("V013"), "V01.3")
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

  icd9List <- ahrqComorbid #todo SUBSET OR EXTRA MAPPINGS?
  for (i in names(icd9List)) {
    expect_equal(
      icd9DecimalToShort(icd9ShortToDecimal(icd9List[[i]])),
      icd9AddLeadingZeroesShort(icd9List[[i]]),
      info = paste("in loop:", i)
    )
  }

  set.seed(1441)
  n = 250
  randomDecimalIcd9 <- paste(
    round(runif(min = 1, max = 999, n = n)),
    sample(icd9ExpandMinor(), replace = TRUE, size = n),
    sep = "."
  )
  randomDecimalIcd9 <- sub(pattern = "\\.$", replacement = "", randomDecimalIcd9)
  # keep the decimal point just because that is how we created the test data.
  expect_equal(
    icd9ShortToDecimal(icd9DecimalToShort(randomDecimalIcd9)),
    icd9AddLeadingZeroesDecimal(randomDecimalIcd9, )
  )
  # test without decimal, too... starting with non-zero-spaced shorts
  rd2 <- as.character(round(runif(min = 1, max = 999, n = n)))
  expect_equal(icd9ShortToDecimal(icd9DecimalToShort(rd2)), icd9AddLeadingZeroesDecimal(rd2))
  expect_equal(icd9DecimalToShort(icd9ShortToDecimal(rd2)), icd9AddLeadingZeroesDecimal(rd2))

  rd3 <- sprintf(fmt = "%03d", round(runif(min = 1, max = 999, n = n)))
  expect_equal(icd9ShortToDecimal(icd9DecimalToShort(rd3)), rd3)

})

test_that("recompose parts realises when data frame is sent to major, or vector to parts", {
  expect_error(icd9PartsRecompose(major = data.frame(major = "100", minor = "98")))
  expect_error(icd9PartsRecompose(data.frame(major = "100", minor = "98")))
  expect_error(icd9PartsRecompose(parts = c("100", "200")))
})

test_that("parts to decimal", {
  #expect_that(icd9PartsToDecimal("100", NA), equals("100"))
  #expect_that(icd9PartsToDecimal("100", ""), equals("100"))
  #expect_that(icd9PartsToDecimal("100", "1"), equals("100.1"))
})

test_that("parts to short invalid inputs", {
  dfempty <- data.frame(major = character(), minor = character())
  dfe2 <- data.frame(major = "", minor = "")

  expect_equal(icd9PartsToShort(parts = dfempty), character())
  expect_equal(icd9PartsToShort(parts = dfe2), "")
  expect_error(icd9PartsToShort(parts = data.frame(major = "turbot", minor = "23"), invalidAction = "stop"))
  expect_error(icd9PartsToShort(parts = data.frame(major = "", minor = "23"), invalidAction = "stop"))
  expect_error(icd9PartsToShort(parts = data.frame(major = "turbot", minor = ""), invalidAction = "stop"))
  expect_error(icd9PartsToShort(parts = data.frame(major = "", minor = ""), invalidAction = "stop"))
  expect_error(icd9PartsToShort(parts = data.frame(major = "turbot", minor = NA), invalidAction = "stop"))
  expect_error(icd9PartsToShort(parts = data.frame(major = "", minor = NA), invalidAction = "stop"))

  expect_equal(icd9PartsToShort(parts = data.frame(major = NA, minor = "trout")), NA_character_)
  expect_equal(icd9PartsToShort(parts = data.frame(major = NA, minor = "23")), NA_character_)
  expect_equal(icd9PartsToShort(parts = data.frame(major = NA, minor = "")), NA_character_)
  expect_equal(icd9PartsToShort(parts = data.frame(major = NA, minor = NA)), NA_character_)

  expect_error(icd9PartsToShort(major = data.frame(major = "100", minor = "23"))) # parts data frame sent to major

})

test_that("parts to valid short with empty or NA minor", {

  for (mn in c("", NA)) {
    # leading zeroes should default to true for codes <100 without minor part:
    # more consistent, because any code <100 with a minor must be zero padded to avoid ambiguity.
    expect_equal(icd9PartsToShort(parts = data.frame(major = "100", minor = mn)), "100")
    expect_equal(icd9PartsToShort(parts = data.frame(major = "10", minor = mn)), "10")
    expect_equal(icd9PartsToShort(parts = data.frame(major = "010", minor = mn)), "010")
    expect_equal(icd9PartsToShort(parts = data.frame(major = "001", minor = mn)), "001")
    expect_equal(icd9PartsToShort(parts = data.frame(major = "01", minor = mn)), "01")
    expect_equal(icd9PartsToShort(parts = data.frame(major = "1", minor = mn)), "1")
  }
})

test_that("parts to valid simple numeric inputs", {
  expect_equal(icd9PartsToShort(parts = data.frame(major = "1", minor = "23")), "00123")
  expect_equal(icd9PartsToShort(parts = data.frame(major = "01", minor = "23")), "00123")
  expect_equal(icd9PartsToShort(parts = data.frame(major = "001", minor = "23")), "00123")
  expect_equal(icd9PartsToShort(parts = data.frame(major = "10", minor = "23")), "01023")
  expect_equal(icd9PartsToShort(parts = data.frame(major = "010", minor = "23")), "01023")
  expect_equal(icd9PartsToShort(parts = data.frame(major = "100", minor = "23")), "10023")
})

test_that("parts to short V code inputs", {
  expect_equal(icd9PartsToShort("V1", c("0", "1")), c("V010", "V011")) # default to zero spacing the V codes
  expect_equal(icd9PartsToShort("V01", c("0", "1")), c("V010", "V011")) # and force zero spacing if required for syntax
  expect_equal(icd9PartsToShort("V1", c("", NA)), c("V1", "V1"))
  expect_equal(icd9PartsToShort("V01", c("", NA)), c("V01", "V01"))
})

test_that("icd9 parts to short: don't allow cycling.", {
  expect_error(icd9PartsToShort(c("123", "34", "56"), c("1", "20")))
  expect_error(icd9PartsToShort(c("123", "34"), c("1", "20", "45")))
})

test_that("icd9 parts to short form V and E input, mismatched lengths", {
  expect_equal(icd9PartsToShort(10L, "20"), "01020")
  expect_equal(icd9PartsToShort("V10", c("0", "1")), c("V100", "V101"))
  expect_equal(icd9PartsToShort("V01", c("0", "1")), c("V010", "V011"))
})
