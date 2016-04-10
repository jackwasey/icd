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

context("icd9 validation")

test_that("is ICD-9 code valid, factor or character", {
  x <- c("1001", "E999", "V01")
  expect_identical(icd_is_valid(x), icd_is_valid(factor(x)))
  expect_identical(icd_is_valid(icd9(x)), icd_is_valid(factor(x)))

  pts <- generate_random_short_icd9()
  expect_identical(icd_is_valid(pts), icd_is_valid(factor(pts)))
  expect_identical(icd_is_valid(icd9(pts)), icd_is_valid(factor(pts)))
})

test_that("mixed decimal and short codes can't all be valid", {
  pts <- generate_random_short_icd9()
  expect_true(!all(icd_is_valid(c(pts, "123.45"))))
})

test_that("mixed ICD-9 and ICD-10 codes can't all be valid", {
  pts <- generate_random_short_icd9()
  expect_true(!all(icd_is_valid(c(pts, "A01"))))
})


test_that("icd9IsValidDecimal - rubbish input", {
  expect_error(icd_is_valid.icd9(short_code = FALSE, list(1230, c(12323, 2323), c("nonesnseses"))))
  expect_error(icd_is_valid.icd9(short_code = FALSE, c(10.1, 200)))
  expect_equal(icd_is_valid.icd9(short_code = FALSE, character()), logical())
  expect_false(icd_is_valid.icd9(short_code = FALSE, "."))
  expect_equal(icd_is_valid.icd9(short_code = FALSE, c("100", "chestnut")), c(TRUE, FALSE))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "100, 200")) # note this is a string with two numbers in it.
  expect_false(icd_is_valid.icd9(short_code = FALSE, NA_character_))
  expect_equal(icd_is_valid.icd9(short_code = FALSE, c("two", "things")), c(FALSE, FALSE))
})

test_that("icd9IsValidDecimal numeric-only", {
  expect_false(icd_is_valid.icd9(short_code = FALSE, ""))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "0"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "00"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "000"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "0.00")) # maybe warn for this one?
  expect_true(icd_is_valid.icd9(short_code = FALSE, "000.00"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "0000"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "100"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "010")) # a bit weird, but should validate
  # not enough zero padding? but not ambiguous.
  expect_true(icd_is_valid.icd9(short_code = FALSE, "01"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "1.1")) # a subtype of cholera
  expect_true(icd_is_valid.icd9(short_code = FALSE, "01.1")) # a subtype of cholera
  expect_true(icd_is_valid.icd9(short_code = FALSE, "001.1")) # a subtype of cholera
  expect_true(icd_is_valid.icd9(short_code = FALSE, "01.10"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "999.99"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, " 22.2 "))
  expect_true(icd_is_valid.icd9(short_code = FALSE, " 33 "))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "01.10"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "01.10"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "100."))
  expect_equal(
    icd_is_valid.icd9(short_code = FALSE, c("100", "200.55", "V01.11")),
    c(TRUE, TRUE, TRUE))
  expect_equal(
    icd_is_valid.icd9(short_code = FALSE, as.factor(c("0", "100", "222.22", "100", "1", "0"))),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(
    icd_is_valid.icd9(short_code = FALSE, c("10.1", "100", "999.99", "0.01")),
    c(TRUE, TRUE, TRUE, TRUE))
})

test_that("icd9IsValidDecimal V codes", {
  expect_true(icd_is_valid.icd9(short_code = FALSE, "V55.55"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "V99. "))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "V1.")) # looks horrible, but not ambiguous
  expect_false(icd_is_valid.icd9(short_code = FALSE, "V0"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "V."))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "V.1"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "V123"))
})

test_that("validate invalid decimal E codes", {

  expect_error(icd9_is_valid_decimal_e(8760))

  expect_false(icd9_is_valid_decimal_e(NA_character_))
  expect_false(icd9_is_valid_decimal_e("NA"))
  expect_false(icd9_is_valid_decimal_e(""))
  expect_false(icd9_is_valid_decimal_e(" "))
  expect_false(icd9_is_valid_decimal_e("   "))
  expect_false(icd9_is_valid_decimal_e("."))
  expect_false(icd9_is_valid_decimal_e("V10.1"))

  expect_false(icd_is_valid.icd9(short_code = FALSE, "0E1"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E."))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E0000"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E00000"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E0000."))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E00000."))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E0000.0"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E00000.0"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E1000."))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E10000."))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E1000.0"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E10000.0"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E0000.1"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E00000.1"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E1000.1"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E10000.1"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E.1"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E00000"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E1234"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E000.00"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E999.12"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E099.12"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E99.12"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E009.12"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E099.12"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E009.12"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E99.9.12"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, ".E999.1"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, ".E9991"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E0999"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E999 9"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E99 9"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E9 9 9"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E00 0"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E9 9 9"))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E999 9 "))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E99 9 "))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E9 9 9 "))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E00 0 "))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E9 9 9 "))
  expect_false(icd_is_valid.icd9(short_code = FALSE, "E00 0 "))
})

test_that("icd9IsValidDecimal valid E codes", {
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E0")) #E000 is okay
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E00"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E000"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E0."))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E00."))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E000."))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E0.0"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E00.0"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E000.0"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E0.1"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E00.1"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E000.1"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E999"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, " E999"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E999 "))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E999. "))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E100")) # E001-E999 are okay, not just E800-E999
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E100.1"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E100."))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E010"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E010.1"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E010."))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E10"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E10.1"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E10."))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E001"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E001.0"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E001."))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E1"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E1.0"))
  expect_true(icd_is_valid.icd9(short_code = FALSE, "E1."))
})

test_that("detect valid short codes", {
  expect_equal(icd_is_valid.icd9(short_code = TRUE, character()), logical())
  expect_error(icd_is_valid.icd9(short_code = TRUE, list(1230, c(12323, 2323), c("nonesnseses"))))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "0"))
  expect_equal(
    icd_is_valid.icd9(short_code = TRUE, c("0", "00", "000", "0000", "00000")),
    c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "12345"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "12 "))
  expect_error(icd_is_valid.icd9(short_code = TRUE, 1))
  expect_equal(icd_is_valid.icd9(short_code = TRUE, c("99999", "1")), c(TRUE, TRUE))
  expect_equal(icd_is_valid.icd9(short_code = TRUE, c("1", "100", "222.22")), c(TRUE, TRUE, FALSE))
  expect_equal(
    icd_is_valid.icd9(short_code = TRUE, as.factor(c("1", "100", "222.22", "100", "1.1"))),
    c(TRUE, TRUE, FALSE, TRUE, FALSE))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "V1")) # not zero-padded, but not ambiguous.

  expect_false(icd_is_valid.icd9(short_code = TRUE, "jericho"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, ""))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "123456"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "10.1"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "11.22")) # am not expecting decimal points
})

test_that("validate short form V codes - invalid codes", {
  expect_false(icd_is_valid.icd9(short_code = TRUE, "V"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "VV"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "Vbog"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, " V0"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "V00000"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "V123456"))
})

test_that("validate short form V codes - valid codes", {
  expect_true(icd_is_valid.icd9(short_code = TRUE, "V234"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, " V45"))
})

test_that("valid short form E codes - invalid input", {
  # call the E validation directly for these completely non-E codes
  expect_false(icd9_is_valid_short_e(NA_character_))
  expect_false(icd9_is_valid_short_e("NA"))
  expect_false(icd9_is_valid_short_e(""))
  expect_false(icd9_is_valid_short_e(" "))
  expect_false(icd9_is_valid_short_e("   "))
  expect_false(icd9_is_valid_short_e("."))
  # now test by routing through the higher level function (which is really the
  # only one that should be used)
  expect_false(icd_is_valid.icd9(short_code = TRUE, "0E1"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E."))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E00000"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E0."))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E00."))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E000."))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E0000."))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E00000."))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E0.0"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E00.0"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E000.0"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E0000.0"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E00000.0"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E1000."))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E10000."))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E1000.0"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E10000.0"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E0.1"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E00.1"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E000.1"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E0000.1"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E00000.1"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E1000.1"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E10000.1"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E.1"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E00000"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E1234"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E000.00"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E999.12"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E099.12"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E99.12"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E009.12"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E099.12"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E009.12"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E99.9.12"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, ".E999.1"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, ".E9991"))
  expect_false(icd_is_valid.icd9(short_code = TRUE, "E98765"))
})

test_that("valid short form E codes - valid input", {
  # E000 is valid!
  # http://www.icd9data.com/2012/Volume1/E000-E999/E000-E000/E000/default.htm
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E0"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E00"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E000"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E0000"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E999"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E0999"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "e800"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, " E999"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E999 "))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "  E999 "))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E100")) # E001-E999 are okay, not just E800-E999
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E1001"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E010"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E0101"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E10"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E001"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E0010"))
  expect_true(icd_is_valid.icd9(short_code = TRUE, "E1"))
})

test_that("test valid major numeric, valid", {
  expect_equal(
    icd9_is_valid_major_n(c("1", "22", "333", "4444", "55555",
                            "1.1", "22.22", "333.333")),
    c(T, T, T, F, F, F, F, F)
  )
  expect_equal(
    icd9_is_valid_major_n(c("01", "001", "22", "022", "0333", "04444",
                            "0055555", "01.1", "022.22", "00333.333")),
    c(T, T, T, T, F, F, F, F, F, F)
  )
})

test_that("test major validation", {
  expect_equal(
    icd_is_valid_major.icd9(c("", "1", "22", "333", "4444", "V", "V2", "V34",
                              "V567", "E", "E1", "E000", "E70", "E300", "E876")),
    c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE,
      FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
  expect_equal(icd9_is_valid_major_e(c("E", "E1", "E000", "E70", "E300", "E876")),
               c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE))

  expect_equal(icd9_is_valid_major_v(c("", "1", "22", "333", "4444", "V", "V2", "V34",
                                       "V567", "E", "E1", "E000", "E70", "E300", "E876",
                                       "V1.1", "V2.89", "V12.4", "V23.45")),
               c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
                 FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                 FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("icd-9 code is really in the list, not just syntactically valid", {
  expect_true(icd_is_defined.icd9(short_code = TRUE, "8027"))
  expect_true(icd_is_defined.icd9(short_code = TRUE, "E9329"))
  expect_false(icd_is_defined.icd9(short_code = TRUE, "J8027"))
  expect_false(icd_is_defined.icd9(short_code = TRUE, "802.7"))
  expect_true(icd_is_defined.icd9(short_code = FALSE, "802.7"))
  expect_true(icd_is_defined.icd9(short_code = FALSE, "E932.9"))
  expect_false(icd_is_defined.icd9(short_code = FALSE, "E9329"))
  expect_false(icd_is_defined.icd9(short_code = FALSE, "J80.27"))
  expect_false(icd_is_defined.icd9(short_code = FALSE, "V802.7"))
  expect_false(icd_is_defined.icd9("V802.7", short_code = FALSE))
  expect_true(icd_is_defined(icd9("802.7"), short_code = FALSE))

  expect_equal(icd_is_defined.icd9(short_code = FALSE, "V802.7"), FALSE)
  expect_equal(
    icd_is_defined(c("8027", "E9329", "E000", "armitage"), short_code = TRUE),
    c(TRUE, TRUE, TRUE, FALSE)
  )
})

test_that("filter valid - bad input", {
  expect_error(icd_filter_valid())
  expect_error(icd_filter_valid(list(j = "k")))
  expect_error(icd9_filter_valid())
  expect_error(icd9_filter_valid())
  expect_error(icd_filter_valid.icd10(list(j = "k")))
  expect_error(icd_filter_valid.icd10(list(j = "k")))
})

test_that("get valid - vector input", {

  expect_equal(icd_get_valid.icd9("100"), "100")
  expect_equal(icd_get_valid(icd9("100")), icd9("100"))
  expect_equal(icd_get_valid.icd9(icd9("100")), icd9("100"))

  expect_equal(icd_get_valid.icd9("nolk"), character())
  expect_equal(icd_get_valid.icd9(c("100", "nolk")), "100")
  expect_equal(icd_get_valid.icd9(short_code = FALSE, c("10.0", "100.x")), "10.0")
  expect_equal(icd_get_valid.icd9(short_code = TRUE, "nolk"), character())
  expect_equal(icd_get_valid.icd9(short_code = TRUE, c("V100", "nolk")), "V100")
  expect_equal(icd_get_invalid.icd9(c("100", "nolk")), "nolk")
})

test_that("filter valid - data frame input", {

  expect_equal(icd9_filter_valid(pts_invalid_mix), pts_invalid_mix[c(1, 3), ])
  expect_equal(icd9_filter_invalid(pts_invalid_mix), pts_invalid_mix[2, ])
  expect_equal(icd9_filter_valid(pts_invalid_mix, invert = TRUE), pts_invalid_mix[2, ])
  # same with S3
  expect_equal(icd_filter_valid(pts_invalid_mix), pts_invalid_mix[c(1, 3), ])
  expect_equal(icd_filter_invalid(pts_invalid_mix), pts_invalid_mix[2, ])
  expect_equal(icd_filter_valid(pts_invalid_mix, invert = TRUE), pts_invalid_mix[2, ])

  # no non-short so all are invalid:
  expect_equal(icd9_filter_valid(pts_invalid_mix, invert = TRUE, short_code = FALSE), pts_invalid_mix)
  # same with S3 dispatch
  expect_equal(tfinvalid <- icd_filter_valid(pts_invalid_mix, invert = TRUE, short_code = FALSE), pts_invalid_mix)
  expect_true(is.icd_long_data(tfinvalid))
  # arg order irrelevant, but can be mixed up in S3 dispatch.
  expect_equal(icd9_filter_valid(pts_invalid_mix, short_code = FALSE, invert = TRUE), pts_invalid_mix)

  # use invert and isShort args:
  expect_equal(icd9_filter_valid(pts_invalid_mix, short_code = TRUE, invert = TRUE), pts_invalid_mix[2, ])
  expect_equal(icd9_filter_valid(pts_invalid_mix, short_code = TRUE, invert = FALSE), pts_invalid_mix[c(1, 3 ), ])
})

test_that("validate mappings", {
  expect_true(icd_is_valid.icd_comorbidity_map(short_code = FALSE, list(a = "100.1", b = "202.3")))
  expect_true(icd_is_valid.icd_comorbidity_map(short_code = TRUE, list(a = "1001", b = "2023")))
  expect_false(icd_is_valid.icd_comorbidity_map(short_code = FALSE, list(a = "1001", b = "2023")))
  expect_false(icd_is_valid.icd_comorbidity_map(short_code = TRUE, list(a = "100.1", b = "202.3")))

  expect_false(icd_is_valid.icd_comorbidity_map(list(a = "car", b = "et"), short_code = FALSE))
  expect_true(icd_is_valid.icd_comorbidity_map(list(a = "1001", b = "2023"), short_code = TRUE))
})

test_that("get invalid decimals", {
  expect_equal(icd_get_invalid.icd9(c("10.1", "rhubarb", "3000"), short_code = FALSE), c("rhubarb", "3000"))
})

test_that("get real codes from a longer list", {
  expect_equal(icd_get_defined(short_code = TRUE, c("003", "0031", "0032"), billable = TRUE), "0031")
  expect_equal(icd_get_defined(short_code = FALSE, c("003", "003.1", "3.2"), billable = TRUE), "003.1")
})

test_that("get real codes which are less than two digit major", {
  expect_equal(icd_get_defined(short_code = TRUE, c("3", "11", "V2"), billable = FALSE), c("3", "11", "V2"))
  expect_equal(icd_get_defined(short_code = FALSE, c("3", "11", "V2"), billable = FALSE), c("3", "11", "V2"))
})

test_that("billable codes are identified", {
  # care as this is dependent on year of ICD-9-CM
  expect_true(icd_is_billable("1000"))
  expect_false(icd_is_billable("1008"))
  expect_true(icd_is_billable("1009"))

  expect_true(icd_is_billable(icd9cm("1000")))
  expect_false(icd_is_billable(icd9cm("1008")))
  expect_true(icd_is_billable(icd9cm("1009")))

  expect_true(icd_is_billable.icd9cm("410.00"))
  expect_false(icd_is_billable.icd9cm("410.6"))
  expect_false(icd_is_billable.icd9cm("410"))

})

test_that("get subset of billable codes", {
  x <- c("410", "410.0", "410.00")
  expect_equal_no_icd(icd_get_billable(x), c("410.00"))
  expect_true(is.icd_decimal_diag(icd_get_billable(x)))
  # TODO: reasonable to assume that if we're talking billable, we make it ICD-9-CM
  expect_true(is.icd9(icd_get_billable(x)))
  expect_true(is.character(icd_get_billable(x)))

  expect_equal_no_icd(icd_get_billable.icd9cm(x), "410.00")
  # assume ICD-9 means ICD-9-CM: this may change
  expect_equal_no_icd(icd_get_billable.icd9(x), "410.00")
})

test_that("get inverted subset of billable codes", {
  x_inv <- c("410", "410.0", "410.00")
  expect_true(is.icd_decimal_diag(icd_get_billable(x_inv, invert = TRUE)))
  # TODO: reasonable to assume that if we're talking billable, we make it ICD-9-CM
  expect_true(is.icd9(icd_get_billable(x_inv, invert = TRUE)))
  expect_true(is.character(icd_get_billable(x_inv, invert = TRUE)))

  expect_equal_no_icd(icd_get_billable.icd9cm(x_inv, invert = TRUE), c("410", "410.0"))
  # assume ICD-9 means ICD-9-CM: this may change
  expect_equal_no_icd(icd_get_billable.icd9(x_inv, invert = TRUE), c("410", "410.0"))
})

test_that("an invalid code is not billable", {
  expect_false(icd_is_billable("Tukey"))
  expect_false(icd_is_billable.icd9("Tukey"))
  expect_false(icd_is_billable.icd9cm("Tukey"))
})

test_that("an invalid short code is not billable decimal", {
  expect_false(icd_is_billable("41000", short_code = FALSE))
  expect_true(icd_is_billable("41000", short_code = TRUE))
  expect_false(icd_is_billable("410.00", short_code = TRUE))
  expect_true(icd_is_billable("410.00", short_code = FALSE))
})


test_that("valid short n", {
  x <- icd9RandomShortN(100)
  expect_true(all(icd9_is_valid_short_n(x)))
})

test_that("valid short v", {
  x <- icd9RandomShortV(100)
  expect_true(all(icd9_is_valid_short_v(x)))
})

test_that("valid short e", {
  x <- icd9RandomShortE(100)
  expect_true(all(icd9_is_valid_short_e(x)))
})

test_that("icd9 and icd10 get valid gives same type as input", {
  expect_equal(icd_get_valid(c("invalid", "100")), "100")
  expect_equal(icd_get_valid(as.icd9(c("invalid", "100"))), as.icd9("100"))
  expect_equal(icd_get_valid(c("invalid", "A01")), "A01")
  expect_equal(icd_get_valid(as.icd10(c("invalid", "A01"))), as.icd10("A01"))
})

test_that("regexes are made and set in package", {
  re_env <- set_re_globals()
  for (re in ls(envir = re_env, pattern = "re_.+")) {
    expect_identical(
      get(re, envir = re_env),
      get(re)
    )
  }
})
