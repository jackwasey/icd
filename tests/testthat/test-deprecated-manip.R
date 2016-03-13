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

context("deprecated basic icd9 manipulation")

test_that("deprecated - zero pad decimal - bad input", {
  expect_equal(icd9AddLeadingZeroesDecimal(character()), character())
  expect_equal(icd9AddLeadingZeroesDecimal(NA_character_), NA_character_)
})

test_that("deprecated - zero pad decimal, numeric only", {

  expect_equal(icd9AddLeadingZeroesDecimal("1"), "001")
  expect_equal(icd9AddLeadingZeroesDecimal("01"), "001")
  expect_equal(icd9AddLeadingZeroesDecimal(" 01 "), "001")
  expect_equal(icd9AddLeadingZeroesDecimal("1.1"), "001.1")
  expect_equal(icd9AddLeadingZeroesDecimal("1.99"), "001.99")
  expect_equal(icd9AddLeadingZeroesDecimal("22"), "022")
  expect_equal(icd9AddLeadingZeroesDecimal(" 22.34 "), "022.34")

  expect_equal(icd9AddLeadingZeroesDecimal("333"), "333")
  expect_equal(icd9AddLeadingZeroesDecimal("333.99"), "333.99")
  expect_equal(icd9AddLeadingZeroesDecimal("333.1 "), "333.1")
  expect_equal(icd9AddLeadingZeroesDecimal(
    c("01", "1.99 ", "22.34", "333", "999.00")),
    c("001", "001.99", "022.34", "333", "999.00"))

})

test_that("deprecated - zero pad decimel V and E codes", {
  expect_equal(icd9AddLeadingZeroesDecimal("V1"), "V01")
  expect_equal(icd9AddLeadingZeroesDecimal(" V1 "), "V01")
  expect_equal(icd9AddLeadingZeroesDecimal("V1.1"), "V01.1")
  expect_equal(icd9AddLeadingZeroesDecimal("V1.99"), "V01.99")
  expect_equal(icd9AddLeadingZeroesDecimal("V22"), "V22")
  expect_equal(icd9AddLeadingZeroesDecimal(" V22.34 "), "V22.34")
  expect_equal(icd9AddLeadingZeroesDecimal("V1"), "V01")
  expect_equal(icd9AddLeadingZeroesDecimal(" V1 "), "V01")
  expect_equal(icd9AddLeadingZeroesDecimal("V1.1"), "V01.1")
  expect_equal(icd9AddLeadingZeroesDecimal("V1.99"), "V01.99")
  expect_equal(icd9AddLeadingZeroesDecimal("V22"), "V22")
  expect_equal(
    icd9AddLeadingZeroesDecimal(" V22.34 "),
    "V22.34")
})

test_that("deprecated - zero pad short invalid codes", {

  expect_equal(icd9AddLeadingZeroesShort(character()), character())
  expect_equal(icd9AddLeadingZeroesShort("anything"),
               NA_character_)
  expect_equal(icd9AddLeadingZeroesShort("anything"), NA_character_)
  expect_equal(icd9AddLeadingZeroesShort(NA_character_), NA_character_)
  # this is just re-checking the validation code...
  expect_equal(icd9AddLeadingZeroesShort("V012"), "V012")
  expect_equal(icd9AddLeadingZeroesShort("V199"), "V199")
})

test_that("deprecated - zero pad short", {
  expect_equal(icd9AddLeadingZeroesShort("1"), "001")
  expect_equal(icd9AddLeadingZeroesShort("01"), "001")
  expect_equal(icd9AddLeadingZeroesShort("22"), "022")
  expect_equal(icd9AddLeadingZeroesShort(" 01 "), "001")
  expect_equal(icd9AddLeadingZeroesShort("199"), "199")
  expect_equal(icd9AddLeadingZeroesShort(" 02234 "), "02234")
  expect_equal(icd9AddLeadingZeroesShort("V1"), "V01")
  expect_equal(icd9AddLeadingZeroesShort(" V1 "), "V01")
  expect_equal(icd9AddLeadingZeroesShort("V1"), "V01")
  expect_equal(icd9AddLeadingZeroesShort(" V1 "), "V01")
  expect_equal(icd9AddLeadingZeroesShort("V11"), "V11")
  expect_equal(icd9AddLeadingZeroesShort(" V11 "), "V11")
  expect_equal(icd9AddLeadingZeroesShort("V11"), "V11")
  expect_equal(icd9AddLeadingZeroesShort(" V11 "), "V11")
  expect_equal(icd9AddLeadingZeroesShort(" V2234 "), "V2234")
  expect_equal(icd9AddLeadingZeroesShort("3331 "), "3331")
  expect_equal(icd9AddLeadingZeroesShort(
    c("9", "01", "0199 ", "02234", "333", "99900")),
    c("009", "001", "0199", "02234", "333", "99900"))
  expect_equal(icd9AddLeadingZeroesShort(NA_character_), NA_character_)
  expect_equal(icd9AddLeadingZeroesShort("V12.34"), NA_character_)

})

test_that("deprecated - icd9 parts to short form numeric input", {
  expect_equal(icd9AddLeadingZeroesMajor(1L), "001")
  expect_equal(icd9AddLeadingZeroesMajor(10L), "010")
  expect_equal(icd9AddLeadingZeroesMajor(999L), "999")
  expect_equal(icd9AddLeadingZeroesMajor(10.1), NA_character_)
})

test_that("deprecated - add leading zeroes to V (and E) majors", {

  expect_equal(icd9AddLeadingZeroesMajor("V1"), "V01")
  expect_equal(icd9AddLeadingZeroesMajor("V2"), "V02")
  expect_equal(icd9AddLeadingZeroesMajor("V03"), "V03")
  expect_equal(icd9AddLeadingZeroesMajor(c("10", "V05")),
               c("010", "V05"))
  expect_equal(icd9AddLeadingZeroesMajor("E915"), "E915")
})

test_that("deprecated - add leading zeroes to majors, invalid input", {
  expect_equal(icd9AddLeadingZeroesMajor("E9"), "E009")
  # should be minimally valid code
  expect_equal(icd9AddLeadingZeroesMajor("E"), NA_character_)
  expect_equal(icd9AddLeadingZeroesMajor("V"), NA_character_)
  expect_equal(icd9AddLeadingZeroesMajor("jasmine"), NA_character_)
  # error if validating
  expect_equal(icd9AddLeadingZeroesMajor("E"), NA_character_)
  expect_equal(icd9AddLeadingZeroesMajor("V"), NA_character_)
  expect_equal(icd9AddLeadingZeroesMajor("jasmine"), NA_character_)
  expect_equal(icd9AddLeadingZeroesMajor("E9"), "E009")
})

test_that("deprecated - all generated icd9 lookup tables are valid!", {

})

test_that("deprecated - icd9ExtractAlphaNumeric", {

  expect_equal(icd9ExtractAlphaNumeric("V12"),
               matrix(data = c("V", "12"), ncol = 2))
  expect_equal(icd9ExtractAlphaNumeric(c("V12", 34)),
               t(matrix(data = c("V", "12", "", "34"), ncol = 2)))

})

test_that("deprecated - strip leading zeroes: errors", {

  expect_equal(icd9DropLeadingZeroesDecimal(NA_character_), NA_character_)
  # no guaranteed behaviour when code is invalid: it may or may not match the
  # regex. If the user wants to get the valid codes first, they can do that.
})

test_that("deprecated - strip leading zero from decimal numeric only", {

  expect_equal(icd9DropLeadingZeroesDecimal(NA_character_), NA_character_)
  expect_equal(icd9DropLeadingZeroesDecimal("1"), "1")
  expect_equal(icd9DropLeadingZeroesDecimal("01"), "1")
  expect_equal(icd9DropLeadingZeroesDecimal("001"), "1")
  expect_equal(icd9DropLeadingZeroesDecimal("1."), "1.")
  expect_equal(icd9DropLeadingZeroesDecimal("01."), "1.")
  expect_equal(icd9DropLeadingZeroesDecimal("001."), "1.")
  expect_equal(icd9DropLeadingZeroesDecimal("12"), "12")
  expect_equal(icd9DropLeadingZeroesDecimal("012"), "12")
  expect_equal(icd9DropLeadingZeroesDecimal("12."), "12.")
  expect_equal(icd9DropLeadingZeroesDecimal("012."), "12.")
  expect_equal(icd9DropLeadingZeroesDecimal("123"), "123")
  expect_equal(icd9DropLeadingZeroesDecimal("123."), "123.")
  expect_equal(icd9DropLeadingZeroesDecimal("1.2"), "1.2")
  expect_equal(icd9DropLeadingZeroesDecimal("01.2"), "1.2")
  expect_equal(icd9DropLeadingZeroesDecimal("001.2"), "1.2")
  expect_equal(icd9DropLeadingZeroesDecimal("12.4"), "12.4")
  expect_equal(icd9DropLeadingZeroesDecimal("012.4"), "12.4")
  expect_equal(icd9DropLeadingZeroesDecimal("12.78"), "12.78")
  expect_equal(icd9DropLeadingZeroesDecimal("012.78"), "12.78")
  expect_equal(icd9DropLeadingZeroesDecimal("123.9"), "123.9")
  expect_equal(icd9DropLeadingZeroesDecimal("123.87"), "123.87")

  # just double check we can do this the other way:
  expect_equal(icd9DropLeadingZeroes("012.78", isShort = FALSE), "12.78")
})

test_that("deprecated - strip leading zero from decimal V and E", {

  expect_equal(icd9DropLeadingZeroesDecimal("V1"), "V1")
  expect_equal(icd9DropLeadingZeroesDecimal("V01"), "V1")
  expect_equal(icd9DropLeadingZeroesDecimal("V1."), "V1.")
  expect_equal(icd9DropLeadingZeroesDecimal("V01."), "V1.")
  expect_equal(icd9DropLeadingZeroesDecimal("V12"), "V12")
  expect_equal(icd9DropLeadingZeroesDecimal("V12.3"), "V12.3")
  expect_equal(icd9DropLeadingZeroesDecimal("V1.2"), "V1.2")
  expect_equal(icd9DropLeadingZeroesDecimal("V01.2"), "V1.2")
  expect_equal(icd9DropLeadingZeroesDecimal("V12.78"), "V12.78")
  expect_equal(icd9DropLeadingZeroesDecimal("E912"), "E912")
  expect_equal(icd9DropLeadingZeroesDecimal("E912."), "E912.")
  expect_equal(icd9DropLeadingZeroesDecimal("E912.7"), "E912.7")

  expect_equal(
    icd9DropLeadingZeroesDecimal(c("V12.78", " E898.", "02", "034.5")),
    c("V12.78", "E898.", "2", "34.5"))
})

test_that("deprecated - strip leading zero from short numeric only", {

  expect_equal(icd9DropLeadingZeroesShort(NA_character_), NA_character_)
  expect_equal(icd9DropLeadingZeroesShort("010"), "10")
  expect_equal(icd9DropLeadingZeroesShort("009"), "9")

  # must have zero to be valid (001.2)
  expect_equal(icd9DropLeadingZeroesShort("0012"), "0012")
  # must have zero to be valid (001.23)
  expect_equal(icd9DropLeadingZeroesShort("00123"), "00123")
  expect_equal(icd9DropLeadingZeroesShort("0124"), "0124")
  expect_equal(icd9DropLeadingZeroesShort("01278"), "01278")
  expect_equal(icd9DropLeadingZeroesShort("1239"), "1239")
  expect_equal(icd9DropLeadingZeroesShort("12387"), "12387")


  # check other way
  expect_equal(icd9DropLeadingZeroes("1239", isShort = TRUE), "1239")
})

test_that("deprecated - strip leading zero from decimal V and E", {

  expect_equal(icd9DropLeadingZeroesShort("V1"), "V1")
  expect_equal(icd9DropLeadingZeroesShort("V12"), "V12")
  expect_equal(icd9DropLeadingZeroesShort("V123"), "V123")
  # cannot drop zero and be the same code. This is an important test!
  expect_equal(icd9DropLeadingZeroesShort("V012"), "V012")
  expect_equal(icd9DropLeadingZeroesShort("V1278"), "V1278")
  expect_equal(icd9DropLeadingZeroesShort("E912"), "E912")
  expect_equal(icd9DropLeadingZeroesShort("E9127"), "E9127")

  test_that("deprecated - mixed vector drop leading zero short", {
    expect_equal(
      icd9DropLeadingZeroesShort(c("V1278", " E898", "02", "0345")),
      c("V1278", "E898", "2", "0345"))
  })
})

test_that("deprecated - drop leading zeroes from majors: invalid input", {
  # this is a little dangerous. dropping zeroes from a major is only valid for
  # short codes if the minor is empty, but this function is unaware of this.
  expect_equal(icd9DropLeadingZeroesMajor(""), "")
  expect_true(is.na(icd9DropLeadingZeroesMajor(NA_character_)))
  expect_true(is.na(icd9DropLeadingZeroesMajor(NA)))
  # dropping leading zeroes from an invalid code is undefined, so no tests.
})

test_that("deprecated - drop leading zeroes from majors: numeric input", {
  expect_equal(icd9DropLeadingZeroesMajor(1), "1")
  expect_equal(icd9DropLeadingZeroesMajor(20), "20")
  expect_equal(icd9DropLeadingZeroesMajor(333), "333")
  expect_equal(icd9DropLeadingZeroesMajor("1"), "1")
  expect_equal(icd9DropLeadingZeroesMajor("20"), "20")
  expect_equal(icd9DropLeadingZeroesMajor("333"), "333")
  expect_equal(icd9DropLeadingZeroesMajor("01"), "1")
  expect_equal(icd9DropLeadingZeroesMajor(" 01 "), "1")
  expect_equal(icd9DropLeadingZeroesMajor("001"), "1")
  expect_equal(icd9DropLeadingZeroesMajor("020"), "20")
})

test_that("deprecated - drop leading zeroes from majors: V codes", {
  expect_equal(icd9DropLeadingZeroesMajor("V1"), "V1")
  expect_equal(icd9DropLeadingZeroesMajor("V01"), "V1")
  expect_equal(icd9DropLeadingZeroesMajor(" V12"), "V12")
})

test_that("deprecated - drop leading zeroes from majors: V codes preserves lower case v", {
  # no strong reason to force this, but seems reasonable
  expect_equal(icd9DropLeadingZeroesMajor(" v01 "), "v1")
  expect_equal(icd9DropLeadingZeroesMajor(" v9 "), "v9")
})

test_that("deprecated - drop leading zeroes from majors: E codes", {
  expect_equal(icd9DropLeadingZeroesMajor("E800"), "E800")
  expect_equal(icd9DropLeadingZeroesMajor(" e812 "), "e812")
})
