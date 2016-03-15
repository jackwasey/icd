# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

context("deprecated icd9 type conversions")

test_that("deprecated - extract decimal parts - invalid or empty input", {
  expect_equal(icd9DecimalToParts(character()), list(major = character(),
                                                     minor = character()))

  expect_equal( icd9DecimalToParts(""), list(major = NA_character_, minor = NA_character_) )

  expect_equal(
    icd9DecimalToParts("", minorEmpty = NA_character_),
    list(major = NA_character_, minor = NA_character_)
  )
})

test_that("deprecated - extract decimal parts - valid inputs", {
  expect_equal(icd9DecimalToParts("0"), list(major = "000", minor = ""))
  expect_equal(icd9DecimalToParts("000"), list(major = "000", minor = ""))
  expect_equal(icd9DecimalToParts("V1.2"), list(major = "V01", minor = "2"))
  expect_equal(icd9DecimalToParts("1.1"), list(major = "001", minor = "1"))
  expect_equal(icd9DecimalToParts("001.1"), list(major = "001", minor = "1"))
  expect_equal(icd9DecimalToParts("22.22"), list(major = "022", minor = "22"))
  expect_equal(icd9DecimalToParts("022.22"), list(major = "022", minor = "22"))
  expect_equal(icd9DecimalToParts("333.3"), list(major = "333", minor = "3"))
  expect_equal(icd9DecimalToParts("444"), list(major = "444", minor = ""))
  expect_equal(icd9DecimalToParts("444", minorEmpty = NA_character_),
               list(major = "444", minor=NA_character_))
  expect_equal(icd9DecimalToParts("444", minorEmpty = ""),
               list(major = "444", minor = ""))
  expect_equal(icd9DecimalToParts(c("9.9", "88.88", "777.6")),
               list(
                 major = c("009", "088", "777"),
                 minor = c("9", "88", "6")
               )
  )

  expect_equal(icd9DecimalToParts(c("009.9", "088.88", "777.6")),
               list(
                 major = c("009", "088", "777"),
                 minor = c("9", "88", "6")
               )
  )
  expect_equal(icd9DecimalToParts(c("9.9", "88", "777.6"), minorEmpty = NA_character_),
               list(
                 major = c("009", "088", "777"),
                 minor = c("9", NA_character_, "6")
               )
  )

  expect_equal(icd9DecimalToParts(c("9.9", "88", "777.6"), minorEmpty = ""),
               list(
                 major = c("009", "088", "777"),
                 minor = c("9", "", "6")
               )
  )

  expect_equal(icd9DecimalToParts(c("001", "", "991.23"), minorEmpty = NA_character_),
               list(
                 major = c("001", NA_character_, "991"),
                 minor = c(NA_character_, NA_character_, "23")
               )
  )
})

test_that("deprecated - icd9 decimal to short form, bad codes", {
  expect_equal_no_icd(icd9DecimalToShort(character()), character())
})

test_that("deprecated - icd9 decimal to short form", {
  expect_equal_no_icd(icd9DecimalToShort("1"), "001")
  expect_equal_no_icd(icd9DecimalToShort("1.1"), "0011")
  expect_equal_no_icd(icd9DecimalToShort("1.23"), "00123")
  expect_equal_no_icd(icd9DecimalToShort("81"), "081")
  expect_equal_no_icd(icd9DecimalToShort("81.1"), "0811")
  expect_equal_no_icd(icd9DecimalToShort("81.23"), "08123")
  expect_equal_no_icd(icd9DecimalToShort("991"), "991")
  expect_equal_no_icd(icd9DecimalToShort("991.1"), "9911")
  expect_equal_no_icd(icd9DecimalToShort("991.23"), "99123")

  expect_equal_no_icd(icd9DecimalToShort(c("1", "991.23")), c("001", "99123"))
  expect_equal_no_icd(icd9DecimalToShort(c("1.", "991.23")), c("001", "99123"))
  expect_equal_no_icd(icd9DecimalToShort(c("1", NA, "991.23")), c("001", NA, "99123"))

})

test_that("deprecated - short to decimal, numbers", {
  # if there is anything after decimal, zeroes must be there!
  expect_equal_no_icd(icd9DecimalToShort("1.0"), "0010")
  expect_equal_no_icd(icd9DecimalToShort("1"), "001")
  expect_equal_no_icd(icd9DecimalToShort("22"), "022")
  expect_equal_no_icd(icd9DecimalToShort("345"), "345")
  expect_equal_no_icd(icd9ShortToDecimal("013"), "013")
  expect_equal_no_icd(icd9ShortToDecimal("V013"), "V01.3")
})

test_that("deprecated - short to decimal bad input", {
  expect_equal_no_icd(icd9ShortToDecimal(character()), character())
  expect_equal_no_icd(icd9ShortToDecimal("valsalva"), NA_character_)
  expect_equal_no_icd(icd9ShortToDecimal("123456"), NA_character_)
  expect_equal_no_icd(icd9ShortToDecimal(""), NA_character_)
  # NA is not character type, so expect error.
  expect_equal_no_icd(icd9ShortToDecimal(NA), NA_character_)
  # NA is not character type, so expect error.
  expect_equal_no_icd(icd9ShortToDecimal(c("000000", "0ab1bc2d")),
               c(NA_character_, NA_character_))
})

test_that("deprecated - icd9 short to major part, E codes", {
  expect_equal(icd9GetMajor(isShort = TRUE, "E000"), "E000")
  expect_equal(icd9GetMajor(isShort = TRUE, "E00"), "E000")
  expect_equal(icd9GetMajor(isShort = TRUE, "E0"), "E000")
  expect_equal(icd9GetMajor(isShort = TRUE, "E1"), "E001")
  expect_equal(icd9GetMajor(isShort = TRUE, "E001"), "E001")
  expect_equal(icd9GetMajor(isShort = TRUE, "E0123"), "E012")
  expect_equal(icd9GetMajor(isShort = TRUE, "E100"), "E100")
  expect_equal(icd9GetMajor(isShort = TRUE, "E1234"), "E123")

})

test_that("deprecated - running short to decimal conversion before and after expansion
          of a ICD-9 base codes gives the same result", {

            icd9List <- ahrqComorbid #todo SUBSET OR EXTRA MAPPINGS?
            for (i in names(icd9List)) {
              expect_equal_no_icd(
                icd9DecimalToShort(icd9ShortToDecimal(icd9List[[i]])),
                icd9AddLeadingZeroesShort(icd9List[[i]]),
                info = paste("in loop:", i)
              )
            }

            n <- 50
            set.seed(1441)
            randomDecimalIcd9pad <- paste(
              sprintf("%03d", round(stats::runif(min = 1, max = 199, n = n))),
              sample(icd9ExpandMinor("", isE = FALSE), replace = TRUE, size = n)[-1],
              sep = "."
            )
            set.seed(1441)
            randomDecimalIcd9 <- paste(
              sprintf("%d", round(stats::runif(min = 1, max = 199, n = n))),
              sample(icd9ExpandMinor("", isE = FALSE), replace = TRUE, size = n)[-1],
              sep = "."
            )
            randomDecimalIcd9pad <- sub(pattern = "\\.$", replacement = "",
                                        randomDecimalIcd9pad)
            expect_equal_no_icd(
              icd9ShortToDecimal(icd9DecimalToShort(randomDecimalIcd9)),
              randomDecimalIcd9pad
            )
            # test without decimal, too... starting with non-zero-spaced shorts
            set.seed(1441)
            rd2 <- as.character(round(stats::runif(min = 1, max = 999, n = n)))
            set.seed(1441)
            rd2pad <- sprintf("%03d", round(stats::runif(min = 1, max = 999, n = n)))
            expect_equal_no_icd(icd9ShortToDecimal(icd9DecimalToShort(rd2)), rd2pad)
            expect_equal_no_icd(icd9DecimalToShort(icd9ShortToDecimal(rd2)), rd2pad)

            expect_equal_no_icd(icd9ShortToDecimal(icd9DecimalToShort(rd2pad)), rd2pad)

            expect_equal_no_icd(icd9DecimalToShort("123."), "123")
          })

test_that("deprecated - parts to decimal", {
  expect_that(icd9PartsToDecimal(data.frame(major = "100", minor = NA)), testthat::equals("100"))
  expect_that(icd9PartsToDecimal(data.frame(major = "100", minor = "")), testthat::equals("100"))
  expect_that(icd9PartsToDecimal(data.frame(major = "100", minor = "1")), testthat::equals("100.1"))
  expect_that(icd9MajMinToDecimal("100", NA), testthat::equals("100"))
  expect_that(icd9MajMinToDecimal("100", ""), testthat::equals("100"))
  expect_that(icd9MajMinToDecimal("100", "1"), testthat::equals("100.1"))
})

test_that("deprecated - parts to short invalid inputs", {
  dfempty <- list(major = character(), minor = character())
  dfe2 <- list(major = "", minor = "")

  expect_equal(icd9PartsToShort(dfempty), character())
  expect_equal(icd9PartsToShort(dfe2), NA_character_)
  expect_equal(icd9PartsToShort(list(major = NA, minor = "trout")), NA_character_)
  expect_equal(icd9PartsToShort(list(major = NA, minor = "23")), NA_character_)
  expect_equal(icd9PartsToShort(list(major = NA, minor = "")), NA_character_)
  expect_equal(icd9PartsToShort(list(major = NA, minor = NA)), NA_character_)

})

test_that("deprecated - parts to valid short with empty or NA minor", {

  for (mn in c("", NA)) {
    # leading zeroes should default to true for codes <100 without minor part:
    # more consistent, because any code <100 with a minor must be zero padded to
    # avoid ambiguity.
    expect_equal(icd9PartsToShort(list(major = "100", minor = mn)), "100")
    expect_equal(icd9PartsToShort(list(major = "10", minor = mn)), "010")
    expect_equal(icd9PartsToShort(list(major = "010", minor = mn)), "010")
    expect_equal(icd9PartsToShort(list(major = "001", minor = mn)), "001")
    expect_equal(icd9PartsToShort(list(major = "01", minor = mn)), "001")
    expect_equal(icd9PartsToShort(list(major = "1", minor = mn)), "001")
  }
})

test_that("deprecated - parts to valid simple numeric inputs", {
  expect_equal(icd9PartsToShort(list(major = "1", minor = "23")),
               "00123")
  expect_equal(icd9PartsToShort(list(major = "01", minor = "23", stringsAsFactors = TRUE)),
               "00123")
  expect_equal(icd9PartsToShort(list(major = "001", minor = "23")),
               "00123")
  expect_equal(icd9PartsToShort(list(major = "10", minor = "23", stringsAsFactors = TRUE)),
               "01023")
  expect_equal(icd9PartsToShort(list(major = "010", minor = "23")),
               "01023")
  expect_equal(icd9PartsToShort(list(major = "100", minor = "23", stringsAsFactors = TRUE)),
               "10023")
})

test_that("deprecated - parts to short V code inputs", {
  # default to zero spacing the V codes
  expect_equal(icd9MajMinToShort("V1", c("0", "1")), c("V010", "V011"))
  # and force zero spacing if required for syntax
  expect_equal(icd9MajMinToShort("V01", c("0", "1")), c("V010", "V011"))
  # simpler to fix non-zero prefixed V codes in conversion
  expect_equal(icd9MajMinToShort("V1", c("", NA)), c("V01", "V01"))
  expect_equal(icd9MajMinToShort("V01", c("", NA)), c("V01", "V01"))
})

test_that("deprecated - icd9 parts to short: don't allow cycling.", {
  expect_error(icd9MajMinToShort(c("123", "34", "56"), c("1", "20")))
  # causes hang only when compiled with MinGW GCC 4.9 in Rtools 3.2 on 64 bit
  expect_error(icd9MajMinToShort(c("123", "34"), c("1", "20", "45")))
})

test_that("deprecated - Windows Rtools 3.2 hang test - also triggers bug #75", {
  expect_error(icd9MajMinToShort(c("123", "34"), c("1", "20", "45")))
  # see Rcpp issue #276.
})

test_that("deprecated - icd9 parts to short form V and E input, mismatched lengths", {
  expect_equal(icd9MajMinToShort(10L, "20"), "01020")
  expect_equal(icd9MajMinToShort("V10", c("0", "1")), c("V100", "V101"))
  expect_equal(icd9MajMinToShort("V01", c("0", "1")), c("V010", "V011"))
})

icd9ChaptersToMap <- icd9_chapters_to_map

test_that("deprecated - convert list of icd-9 ranges (e.g. chapter definitions to comorbidity map)", {
  skip_slow_tests()
  ooe <- data.frame(visitId = sprintf("pt%02d", seq_along(one_of_each)), icd9 = one_of_each)

  test_map <- icd9ChaptersToMap(icd::icd9Chapters)
  cmb <- icd9Comorbid(icd9df = ooe, isShort = FALSE, icd9Mapping = test_map,
                      isShortMapping = TRUE, return.df = TRUE)
  cmbcmp <- unname(as.matrix(logical_to_binary(cmb)[-1]))
  expmat <- diag(nrow = length(ooe$icd9))
  expect_equivalent(cmbcmp, expmat)
})

# some functions are only called via C++ (at present), so the path through
# RcppExports is not tested. Also, compare slower functions for identical
# results as a regression test.
test_that("deprecated - code routes through RcppExports.R and slower versions", {
  expect_equal(icd9ShortToParts("1001"),
               data.frame(major = "100", minor = "1", stringsAsFactors = FALSE))
  expect_equal(icd9ShortToParts(c("99999", "0011")),
               data.frame(major = c("999", "001"), minor = c("99", "1"), stringsAsFactors = FALSE))
})
