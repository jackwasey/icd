context("test icd9 package")

test_that("running short to decimal conversion before and after expansion of a ICD-9 base codes gives the same result", {

  expect_equal(icd9ShortToDecimal("013"), "13")
  expect_equal(icd9ShortToDecimal("013", leadingZeroes = TRUE), "013")
  expect_equal(icd9ShortToDecimal("013", keepLoneDecimal = TRUE), "13.")
  expect_equal(icd9ShortToDecimal("013", leadingZeroes = TRUE, keepLoneDecimal = TRUE), "013.")
  expect_equal(icd9ShortToDecimal("V013"), "V1.3")
  expect_equal(icd9ShortToDecimal("V013", leadingZeroes = TRUE), "V01.3")
  expect_equal(icd9ShortToDecimal("V01", keepLoneDecimal = TRUE), "V1.")
  expect_equal(icd9ShortToDecimal("V01", leadingZeroes = TRUE, keepLoneDecimal = TRUE), "V01.")

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


test_that("appendZeroToNine", {
  expect_error(appendZeroToNine(list(a = c(1, 2)))) # error on silly input
  expect_identical(appendZeroToNine("1"), as.character(10:19))
  expect_identical(appendZeroToNine(1), as.character(10:19))
  expect_identical(appendZeroToNine(""), as.character(0:9))
  expect_identical(sort(appendZeroToNine(c("1", "2"))), as.character(c(10:19, 20:29)))
  expect_identical(sort(appendZeroToNine(c("", "9"))), as.character(c(0:9, 90:99)))
})

test_that("extract decimal parts", {
  expect_equal(icd9ExtractPartsDecimal("0"), data.frame(major = "000", minor = "", stringsAsFactors = FALSE)) # zero is valid, means no code.
  expect_equal(icd9ExtractPartsDecimal("V1.2"), data.frame(major = "V1", minor = "2", stringsAsFactors = FALSE))
  expect_equal(icd9ExtractPartsDecimal("1.1"), data.frame(major = "001", minor = "1", stringsAsFactors = FALSE))
  expect_equal(icd9ExtractPartsDecimal("22.22"), data.frame(major = "022", minor = "22", stringsAsFactors = FALSE))
  expect_equal(icd9ExtractPartsDecimal("333.3"), data.frame(major = "333", minor = "3", stringsAsFactors = FALSE))
  expect_equal(icd9ExtractPartsDecimal("444"), data.frame(major = "444", minor = "", stringsAsFactors = FALSE))
  expect_equal(icd9ExtractPartsDecimal("444", minorEmpty=NA_character_), data.frame(major = "444", minor=NA_character_, stringsAsFactors = FALSE))
  expect_equal(icd9ExtractPartsDecimal("444", minorEmpty = ""), data.frame(major = "444", minor = "", stringsAsFactors = FALSE))
  expect_equal(icd9ExtractPartsDecimal("12.3", leadingZeroes = TRUE), data.frame(major = "012", minor = "3", stringsAsFactors = FALSE))
  expect_equal(icd9ExtractPartsDecimal(c("9.9", "88.88", "777.6")),
               data.frame(
                 major = c("009", "088", "777"),
                 minor = c("9", "88", "6"),
                 stringsAsFactors = FALSE
               )
  )
  expect_equal(icd9ExtractPartsDecimal(c("9.9", "88", "777.6"), minorEmpty=NA),
               data.frame(
                 major = c("009", "088", "777"),
                 minor = c("9", NA, "6"),
                 stringsAsFactors = FALSE
               )
  )
  expect_equal(icd9ExtractPartsDecimal(c("1", "g", "", "991.23"), invalidAction = "silent", minorEmpty = NA),
               data.frame(
                 major = c("001", NA, NA, "991"),
                 minor = c(NA, NA, NA, "23"),
                 stringsAsFactors = FALSE
               )
  )
  expect_equal(icd9ExtractPartsDecimal(c("1", "g", "", "991.23"), invalidAction = "ignore", minorEmpty = NA),
               data.frame(
                 major = c("001", "g", NA, "991"),
                 minor = c(NA, NA, NA, "23"),
                 stringsAsFactors = FALSE
               )
  )
})


test_that("zero pad decimal", {

  expect_equal(icd9AddLeadingZeroesDecimal("1"), "001")
  expect_equal(icd9AddLeadingZeroesDecimal("01"), "001")
  expect_equal(icd9AddLeadingZeroesDecimal(" 01 "), "001")
  expect_equal(icd9AddLeadingZeroesDecimal("1.1"), "001.1")
  expect_equal(icd9AddLeadingZeroesDecimal("1.99"), "001.99")
  expect_equal(icd9AddLeadingZeroesDecimal("22"), "022")
  expect_equal(icd9AddLeadingZeroesDecimal(" 22.34 "), "022.34")
  expect_equal(icd9AddLeadingZeroesDecimal("V1"), "V1")
  expect_equal(icd9AddLeadingZeroesDecimal(" V1 "), "V1")
  expect_equal(icd9AddLeadingZeroesDecimal("V1.1"), "V1.1")
  expect_equal(icd9AddLeadingZeroesDecimal("V1.99"), "V1.99")
  expect_equal(icd9AddLeadingZeroesDecimal("V22"), "V22")
  expect_equal(icd9AddLeadingZeroesDecimal(" V22.34 "), "V22.34")
  expect_equal(icd9AddLeadingZeroesDecimal("333"), "333")
  expect_equal(icd9AddLeadingZeroesDecimal("333.99"), "333.99")
  expect_equal(icd9AddLeadingZeroesDecimal("333.1 "), "333.1")
  expect_equal(icd9AddLeadingZeroesDecimal(
    c("01", "1.99 ", "22.34", "333", "999.00")),
    c("001", "001.99", "022.34", "333", "999.00"))
  expect_equal(icd9AddLeadingZeroesDecimal(NA_character_), NA_character_)

})

test_that("zero pad short", {

  expect_error(icd9AddLeadingZeroesShort("1.1"))

  expect_equal(icd9AddLeadingZeroesShort("1"), "001")
  expect_equal(icd9AddLeadingZeroesShort("01"), "001")
  expect_equal(icd9AddLeadingZeroesShort("22"), "022")
  expect_equal(icd9AddLeadingZeroesShort(" 01 "), "001")
  expect_equal(icd9AddLeadingZeroesShort("199"), "199")
  expect_equal(icd9AddLeadingZeroesShort(" 02234 "), "02234")
  expect_equal(icd9AddLeadingZeroesShort("V1"), "V01")
  expect_equal(icd9AddLeadingZeroesShort(" V1 "), "V01")
  expect_equal(icd9AddLeadingZeroesShort("V11"), "V11")
  expect_equal(icd9AddLeadingZeroesShort("V012"), "V012")
  expect_equal(icd9AddLeadingZeroesShort("V199"), "V199")
  expect_equal(icd9AddLeadingZeroesShort(" V2234 "), "V2234")
  expect_equal(icd9AddLeadingZeroesShort("3331 "), "3331")
  expect_equal(icd9AddLeadingZeroesShort(
    c("9", "01", "0199 ", "02234", "333", "99900")),
    c("009", "001", "0199", "02234", "333", "99900"))
  expect_equal(icd9AddLeadingZeroesShort(NA_character_), NA_character_)
  expect_equal(icd9AddLeadingZeroesShort("V12.34"), NA_character_)

})

test_that("icd9 parts to short form numeric input", {
  expect_equal(icd9AddLeadingZeroesMajor(1L), "001")
  expect_equal(icd9AddLeadingZeroesMajor(10L), "010")
  expect_equal(icd9AddLeadingZeroesMajor(999L), "999")
  expect_error(icd9AddLeadingZeroesMajor(10.1))
})

test_that("icd9 parts to short form V and E input", {

  expect_equal(icd9AddLeadingZeroesMajor("V1"), "V01") # TODO
  expect_equal(icd9AddLeadingZeroesMajor(" V10"), "V10")
  expect_equal(icd9AddLeadingZeroesMajor("V2"), "V02")
  expect_equal(icd9AddLeadingZeroesMajor("V03"), "V03")
  expect_equal(icd9AddLeadingZeroesMajor(c("10", "V05")), c("010", "V05"))
  expect_equal(icd9AddLeadingZeroesMajor("E915"), "E915") # should never be any extra zeroes.
  expect_equal(icd9AddLeadingZeroesMajor("E", invalidAction = "silent"), NA_character_) # should be minimally valid code
  expect_equal(icd9AddLeadingZeroesMajor("V", invalidAction = "silent"), NA_character_)
  expect_equal(icd9AddLeadingZeroesMajor("jasmine", invalidAction = "silent"), NA_character_)
  expect_error(icd9AddLeadingZeroesMajor("E", invalidAction = "stop")) # error if validating
  expect_error(icd9AddLeadingZeroesMajor("V", invalidAction = "stop"))
  expect_error(icd9AddLeadingZeroesMajor("jasmine", invalidAction = "stop"))
  #expect_equal(icd9AddLeadingZeroesMajor("E9"), "E9") # minimal validation of major, should just give back E codes.

  expect_error(icd9PartsToShort(c("123", "34", "56"), c("1", "20"))) # don't allow cycling.
  expect_error(icd9PartsToShort(c("123", "34"), c("1", "20", "45"))) # don't allow cycling.
  expect_equal(icd9PartsToShort(10L, "20"), "01020")
  expect_equal(icd9PartsToShort("V10", c("0", "1")), c("V100", "V101"))
})

test_that("all generated icd9 lookup tables are valid!", {})

test_that("icd9ExtractAlphaNumeric", {

  expect_equal(icd9ExtractAlphaNumeric("V12"), matrix(data = c("V", "12"), ncol = 2))
  expect_equal(icd9ExtractAlphaNumeric(c("V12", 34)), t(matrix(data = c("V", "12", "", "34"), ncol = 2)))

})

test_that("strip leading zero from decimal errors", {

  expect_error(icd9DropZeroFromDecimal("sandwiches", invalidAction = "stop"))
  expect_error(icd9DropZeroFromDecimal("VE123456.789", invalidAction = "stop"))
})

test_that("strip leading zero from decimal numeric only", {

  expect_equal(icd9DropZeroFromDecimal(NA_character_), NA_character_)
  expect_equal(icd9DropZeroFromDecimal("1"), "1")
  expect_equal(icd9DropZeroFromDecimal("01"), "1")
  expect_equal(icd9DropZeroFromDecimal("001"), "1")
  expect_equal(icd9DropZeroFromDecimal("1."), "1.")
  expect_equal(icd9DropZeroFromDecimal("01."), "1.")
  expect_equal(icd9DropZeroFromDecimal("001."), "1.")
  expect_equal(icd9DropZeroFromDecimal("12"), "12")
  expect_equal(icd9DropZeroFromDecimal("012"), "12")
  expect_equal(icd9DropZeroFromDecimal("12."), "12.")
  expect_equal(icd9DropZeroFromDecimal("012."), "12.")
  expect_equal(icd9DropZeroFromDecimal("123"), "123")
  expect_equal(icd9DropZeroFromDecimal("123."), "123.")
  expect_equal(icd9DropZeroFromDecimal("1.2"), "1.2")
  expect_equal(icd9DropZeroFromDecimal("01.2"), "1.2")
  expect_equal(icd9DropZeroFromDecimal("001.2"), "1.2")
  expect_equal(icd9DropZeroFromDecimal("12.4"), "12.4")
  expect_equal(icd9DropZeroFromDecimal("012.4"), "12.4")
  expect_equal(icd9DropZeroFromDecimal("12.78"), "12.78")
  expect_equal(icd9DropZeroFromDecimal("012.78"), "12.78")
  expect_equal(icd9DropZeroFromDecimal("123.9"), "123.9")
  expect_equal(icd9DropZeroFromDecimal("123.87"), "123.87")
})

test_that("strip leading zero from decimal V and E", {

  expect_equal(icd9DropZeroFromDecimal("V1"), "V1")
  expect_equal(icd9DropZeroFromDecimal("V01"), "V1")
  expect_equal(icd9DropZeroFromDecimal("V1."), "V1.")
  expect_equal(icd9DropZeroFromDecimal("V01."), "V1.")
  expect_equal(icd9DropZeroFromDecimal("V12"), "V12")
  expect_equal(icd9DropZeroFromDecimal("V12.3"), "V12.3")
  expect_equal(icd9DropZeroFromDecimal("V1.2"), "V1.2")
  expect_equal(icd9DropZeroFromDecimal("V01.2"), "V1.2")
  expect_equal(icd9DropZeroFromDecimal("V12.78"), "V12.78")
  expect_equal(icd9DropZeroFromDecimal("E912"), "E912")
  expect_equal(icd9DropZeroFromDecimal("E912."), "E912.")
  expect_equal(icd9DropZeroFromDecimal("E912.7"), "E912.7")

  expect_equal(icd9DropZeroFromDecimal(c("V12.78", " E898.", "02", "034.5")), c("V12.78", "E898.", "2", "34.5"))
})
