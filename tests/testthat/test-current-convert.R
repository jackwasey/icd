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

context("icd9 type conversions")

test_that("extract decimal parts - invalid or empty input", {
  expect_equal(icd_decimal_to_parts.icd9(character()), list(major = character(),
                                                            minor = character()))

  expect_equal( icd_decimal_to_parts.icd9(""), list(major = NA_character_, minor = NA_character_) )

  expect_equal(
    icd_decimal_to_parts.icd9("", minor_empty = NA_character_),
    list(major = NA_character_, minor = NA_character_)
  )
})

test_that("extract decimal parts - valid inputs", {
  expect_equal(icd_decimal_to_parts.icd9("0"), list(major = "000", minor = ""))
  expect_equal(icd_decimal_to_parts.icd9("000"), list(major = "000", minor = ""))
  expect_equal(icd_decimal_to_parts.icd9("V1.2"), list(major = "V01", minor = "2"))
  expect_equal(icd_decimal_to_parts.icd9("1.1"), list(major = "001", minor = "1"))
  expect_equal(icd_decimal_to_parts.icd9("001.1"), list(major = "001", minor = "1"))
  expect_equal(icd_decimal_to_parts.icd9("22.22"), list(major = "022", minor = "22"))
  expect_equal(icd_decimal_to_parts.icd9("022.22"), list(major = "022", minor = "22"))
  expect_equal(icd_decimal_to_parts.icd9("333.3"), list(major = "333", minor = "3"))
  expect_equal(icd_decimal_to_parts.icd9("444"), list(major = "444", minor = ""))
  expect_equal(icd_decimal_to_parts.icd9("444", minor_empty = NA_character_),
               list(major = "444", minor = NA_character_))
  expect_equal(icd_decimal_to_parts.icd9("444", minor_empty = ""),
               list(major = "444", minor = ""))
  expect_equal(icd_decimal_to_parts.icd9(c("9.9", "88.88", "777.6")),
               list(
                 major = c("009", "088", "777"),
                 minor = c("9", "88", "6")
               )
  )

  expect_equal(icd_decimal_to_parts.icd9(c("009.9", "088.88", "777.6")),
               list(
                 major = c("009", "088", "777"),
                 minor = c("9", "88", "6")
               )
  )
  expect_equal(icd_decimal_to_parts.icd9(c("9.9", "88", "777.6"), minor_empty = NA_character_),
               list(
                 major = c("009", "088", "777"),
                 minor = c("9", NA_character_, "6")
               )
  )

  expect_equal(icd_decimal_to_parts.icd9(c("9.9", "88", "777.6"), minor_empty = ""),
               list(
                 major = c("009", "088", "777"),
                 minor = c("9", "", "6")
               )
  )

  expect_equal(icd_decimal_to_parts.icd9(c("001", "", "991.23"), minor_empty = NA_character_),
               list(
                 major = c("001", NA_character_, "991"),
                 minor = c(NA_character_, NA_character_, "23")
               )
  )
})

test_that("icd9 decimal to short form, bad codes", {
  expect_equal_no_icd(icd_decimal_to_short.icd9(character()), character())
})

test_that("icd9 decimal to short form", {
  expect_equal_no_icd(icd_decimal_to_short.icd9("1"), "001")
  expect_equal_no_icd(icd_decimal_to_short.icd9("1.1"), "0011")
  expect_equal_no_icd(icd_decimal_to_short.icd9("1.23"), "00123")
  expect_equal_no_icd(icd_decimal_to_short.icd9("81"), "081")
  expect_equal_no_icd(icd_decimal_to_short.icd9("81.1"), "0811")
  expect_equal_no_icd(icd_decimal_to_short.icd9("81.23"), "08123")
  expect_equal_no_icd(icd_decimal_to_short.icd9("991"), "991")
  expect_equal_no_icd(icd_decimal_to_short.icd9("991.1"), "9911")
  expect_equal_no_icd(icd_decimal_to_short.icd9("991.23"), "99123")

  expect_equal_no_icd(icd_decimal_to_short.icd9(c("1", "991.23")), c("001", "99123"))
  expect_equal_no_icd(icd_decimal_to_short.icd9(c("1.", "991.23")), c("001", "99123"))
  expect_equal_no_icd(icd_decimal_to_short.icd9(c("1", NA, "991.23")), c("001", NA, "99123"))
})

test_that("icd9 decimal to short with factor input", {
  expect_is(icd_decimal_to_short(factor(c("1.", "991.23"))), "factor")
})

test_that("icd10 decimal to short with factor input", {
  expect_is(icd_decimal_to_short(factor(c("A10.1", "Z11.9AX"))), "factor")
})

test_that("short to decimal, numbers", {
  # if there is anything after decimal, zeroes must be there!
  expect_equal_no_icd(icd_decimal_to_short.icd9("1.0"), "0010")
  expect_equal_no_icd(icd_decimal_to_short.icd9("1"), "001")
  expect_equal_no_icd(icd_decimal_to_short.icd9("22"), "022")
  expect_equal_no_icd(icd_decimal_to_short.icd9("345"), "345")
  expect_equal_no_icd(icd_short_to_decimal.icd9("013"), "013")
  expect_equal_no_icd(icd_short_to_decimal.icd9("V013"), "V01.3")
})

test_that("short to decimal bad input", {
  expect_equal_no_icd(icd_short_to_decimal.icd9(character()), character())
  expect_equal_no_icd(icd_short_to_decimal.icd9("valsalva"), NA_character_)
  expect_equal_no_icd(icd_short_to_decimal.icd9("123456"), NA_character_)
  expect_equal_no_icd(icd_short_to_decimal.icd9(""), NA_character_)
  # NA is not character type, so expect error.
  expect_equal_no_icd(icd_short_to_decimal.icd9(NA), NA_character_)
  # NA is not character type, so expect error.
  expect_equal_no_icd(icd_short_to_decimal.icd9(c("000000", "0ab1bc2d")),
                      c(NA_character_, NA_character_))
})

test_that("icd10 short to decimal", {
  expect_true(is.icd_decimal_diag(icd_short_to_decimal("A00")))
  expect_true(is.icd_decimal_diag(icd_short_to_decimal.icd10("A00")))
  expect_true(is.icd_decimal_diag(icd_short_to_decimal.icd10cm("A00")))
  expect_true(is.icd_decimal_diag(icd_short_to_decimal("A009")))
  expect_true(is.icd_decimal_diag(icd_short_to_decimal.icd10("A009")))
  expect_true(is.icd_decimal_diag(icd_short_to_decimal.icd10cm("A009")))

  expect_equal(icd_short_to_decimal("A00"), as.icd10("A00") %>% as.icd_decimal_diag)
  expect_equal(icd_short_to_decimal.icd10("A00"), as.icd10("A00") %>% as.icd_decimal_diag)
  expect_equal(icd_short_to_decimal("A000"), as.icd10("A00.0") %>% as.icd_decimal_diag)
  expect_equal(icd_short_to_decimal.icd10("A000"), as.icd10("A00.0") %>% as.icd_decimal_diag)

})

test_that("icd10 short to decimal for multiple codes", {
  # pick up specific bug where a warning was given for muliple codes
  expect_warning(res <- icd_short_to_decimal(c("O9A119", "O9A53", "S0000XA", "T3299", "P150", "P159",
                                               "Z9989", "Z950", "C7A098", "C7A8")), regexp = NA)

  expect_equal(as.vector(res), c("O9A.119", "O9A.53", "S00.00XA", "T32.99", "P15.0", "P15.9",
                                 "Z99.89", "Z95.0", "C7A.098", "C7A.8"))
  expect_true(is.icd_decimal_diag(res))

  # is the icd10cm class preserved?
  expect_warning(res <- icd_short_to_decimal(icd10cm(c("O9A119", "O9A53", "S0000XA", "T3299", "P150", "P159",
                                                       "Z9989", "Z950", "C7A098", "C7A8"))), regexp = NA)

  expect_true(is.icd_decimal_diag(res))
  expect_true(is.icd10cm(res))

})

test_that("icd10 short to decimal and back", {
  expect_identical(icd_short_to_decimal(icd_decimal_to_short("A00.0")), as.icd10("A00.0") %>% as.icd_decimal_diag)
  expect_identical(icd_decimal_to_short(icd_short_to_decimal("A000")), as.icd10("A000") %>% as.icd_short_diag)
})

test_that("icd9 short to major part, E codes", {
  expect_equal_no_icd(icd_get_major.icd9(short_code = TRUE, "E000"), "E000")
  expect_equal_no_icd(icd_get_major.icd9(short_code = TRUE, "E00"), "E000")
  expect_equal_no_icd(icd_get_major.icd9(short_code = TRUE, "E0"), "E000")
  expect_equal_no_icd(icd_get_major.icd9(short_code = TRUE, "E1"), "E001")
  expect_equal_no_icd(icd_get_major.icd9(short_code = TRUE, "E001"), "E001")
  expect_equal_no_icd(icd_get_major.icd9(short_code = TRUE, "E0123"), "E012")
  expect_equal_no_icd(icd_get_major.icd9(short_code = TRUE, "E100"), "E100")
  expect_equal_no_icd(icd_get_major.icd9(short_code = TRUE, "E1234"), "E123")

})

test_that("running short to decimal conversion before and after expansion
          of a ICD-9 base codes gives the same result", {

            icd9List <- icd::icd9_map_ahrq #todo SUBSET OR EXTRA MAPPINGS?
            for (i in names(icd9List)) {
              expect_equal_no_icd(
                icd_decimal_to_short.icd9(icd_short_to_decimal.icd9(icd9List[[i]])),
                icd9_add_leading_zeroes(icd9List[[i]], short_code = TRUE),
                info = paste("in loop:", i)
              )
            }

            n <- 50
            set.seed(1441)
            rd9pad <- paste(
              sprintf("%03d", round(stats::runif(min = 1, max = 199, n = n))),
              sample(icd_expand_minor.icd9("", is_e = FALSE), replace = TRUE, size = n)[-1],
              sep = "."
            )
            set.seed(1441)
            rd9 <- paste(
              sprintf("%d", round(stats::runif(min = 1, max = 199, n = n))),
              sample(icd_expand_minor.icd9("", is_e = FALSE), replace = TRUE, size = n)[-1],
              sep = "."
            )
            rd9pad <- sub(pattern = "\\.$", replacement = "",
                          rd9pad)
            expect_equal_no_icd(
              icd_short_to_decimal.icd9(icd_decimal_to_short.icd9(rd9)),
              rd9pad
            )
            # test without decimal, too... starting with non-zero-spaced shorts
            set.seed(1441)
            rd2 <- as.character(round(stats::runif(min = 1, max = 999, n = n)))
            set.seed(1441)
            rd2pad <- sprintf("%03d", round(stats::runif(min = 1, max = 999, n = n)))
            expect_equal_no_icd(icd_short_to_decimal.icd9(icd_decimal_to_short.icd9(rd2)), rd2pad)
            expect_equal_no_icd(icd_decimal_to_short.icd9(icd_short_to_decimal.icd9(rd2)), rd2pad)

            expect_equal_no_icd(icd_short_to_decimal.icd9(icd_decimal_to_short.icd9(rd2pad)), rd2pad)

            expect_equal_no_icd(icd_decimal_to_short.icd9("123."), "123")
          })

test_that("short to decimal conversions also convert class", {

  expect_true(
    is.icd_short_diag(
      icd_decimal_to_short(
        as.icd_decimal_diag("12.34")
      )
    )
  )

  expect_true(
    is.icd_decimal_diag(
      icd_short_to_decimal(
        as.icd_short_diag("1234")
      )
    )
  )

  expect_true(
    is.icd_short_diag(
      icd_decimal_to_short.icd9(
        as.icd_decimal_diag("12.34")
      )
    )
  )

  expect_true(
    is.icd_decimal_diag(
      icd_short_to_decimal.icd9(
        as.icd_short_diag("1234")
      )
    )
  )

})

test_that("parts to decimal", {
  expect_that(icd9PartsToDecimal(data.frame(major = "100", minor = NA)), testthat::equals("100"))
  expect_that(icd9PartsToDecimal(data.frame(major = "100", minor = "")), testthat::equals("100"))
  expect_that(icd9PartsToDecimal(data.frame(major = "100", minor = "1")), testthat::equals("100.1"))
  expect_that(icd9MajMinToDecimal("100", NA), testthat::equals("100"))
  expect_that(icd9MajMinToDecimal("100", ""), testthat::equals("100"))
  expect_that(icd9MajMinToDecimal("100", "1"), testthat::equals("100.1"))
})

test_that("parts to short invalid inputs", {
  dfempty <- list(major = character(), minor = character())
  dfe2 <- list(major = "", minor = "")

  expect_equal(icd9PartsToShort(dfempty), character())
  expect_equal(icd9PartsToShort(dfe2), NA_character_)
  expect_equal(icd9PartsToShort(list(major = NA, minor = "trout")), NA_character_)
  expect_equal(icd9PartsToShort(list(major = NA, minor = "23")), NA_character_)
  expect_equal(icd9PartsToShort(list(major = NA, minor = "")), NA_character_)
  expect_equal(icd9PartsToShort(list(major = NA, minor = NA)), NA_character_)

})

test_that("parts to valid short with empty or NA minor", {

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

test_that("parts to valid simple numeric inputs", {
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

test_that("parts to short V code inputs", {
  # default to zero spacing the V codes
  expect_equal(icd9MajMinToShort("V1", c("0", "1")), c("V010", "V011"))
  # and force zero spacing if required for syntax
  expect_equal(icd9MajMinToShort("V01", c("0", "1")), c("V010", "V011"))
  # simpler to fix non-zero prefixed V codes in conversion
  expect_equal(icd9MajMinToShort("V1", c("", NA)), c("V01", "V01"))
  expect_equal(icd9MajMinToShort("V01", c("", NA)), c("V01", "V01"))
})

test_that("maj min to short for multiple majors", {
  expect_identical(icd9MajMinToShort(c("100", "200"), c("10", "20")),
                   c("10010", "20020"))
})

test_that("icd9 parts to short: don't allow cycling.", {
  expect_error(icd9MajMinToShort(c("123", "34", "56"), c("1", "20")))
  # causes hang only when compiled with MinGW GCC 4.9 in Rtools 3.2 on 64 bit
  expect_error(icd9MajMinToShort(c("123", "34"), c("1", "20", "45")))
})

test_that("Windows Rtools 3.2 hang test - also triggers bug #75", {
  expect_error(icd9MajMinToShort(c("123", "34"), c("1", "20", "45")))
  # see Rcpp issue #276.
})

test_that("icd9 parts to short form V and E input, mismatched lengths", {
  expect_equal(icd9MajMinToShort(10L, "20"), "01020")
  expect_equal(icd9MajMinToShort("V10", c("0", "1")), c("V100", "V101"))
  expect_equal(icd9MajMinToShort("V01", c("0", "1")), c("V010", "V011"))
})

test_that("convert list of icd-9 ranges (e.g. chapter definitions to comorbidity map)", {
  skip_slow_tests()
  ooe <- icd_long_data(visit_id = sprintf("pt%02d", seq_along(one_of_each)),
                       code = one_of_each,
                       stringsAsFactors = TRUE)

  class(ooe[["code"]]) <- c("icd9", "icd_decimal_diag", "factor")

  expect_warning(test_map <- icd9_chapters_to_map(icd::icd9Chapters), regexp = NA)
  expect_warning(
    cmb <- icd9_comorbid(x = ooe, short_code = FALSE, map = test_map,
                         short_map = TRUE, return_df = TRUE), regexp = NA)
  cmbcmp <- unname(as.matrix(logical_to_binary(cmb)[-1]))
  expmat <- diag(nrow = length(ooe$code))
  expect_equivalent(cmbcmp, expmat)
})

# some functions are only called via C++ (at present), so the path through
# RcppExports is not tested. Also, compare slower functions for identical
# results as a regression test.
test_that("code routes through RcppExports.R and slower versions", {
  expect_equal(icd_short_to_parts.icd9("1001"),
               data.frame(major = "100", minor = "1", stringsAsFactors = FALSE))
  expect_equal(icd_short_to_parts.icd9(c("99999", "0011")),
               data.frame(major = c("999", "001"), minor = c("99", "1"), stringsAsFactors = FALSE))
})


context("icd10 conversions")
# TODO: this needs fleshing out

test_that("decimal ICD-10 to parts", {
  expect_identical(
    icd_decimal_to_parts("A00.0"),
    list(major = "A00", minor = "0")
  )
  expect_identical(
    icd_decimal_to_parts("C7A.020"),
    list(major = "C7A", minor = "020")
  )
  expect_identical(
    icd_decimal_to_parts(icd10cm("E89.89")),
    list(major = "E89", minor = "89")
  )
})

test_that("icd10 short to parts", {
  expect_equal(icd_short_to_parts("A0101"),
              data.frame(major = "A01", minor = "01", stringsAsFactors = FALSE))
  # for V and E codes, we can't just assume ICD-10 will work with the ICD-9 function:
  expect_equal(icd_short_to_parts(as.icd10cm("E8989")),
               data.frame(major = "E89", minor = "89", stringsAsFactors = FALSE))
})
