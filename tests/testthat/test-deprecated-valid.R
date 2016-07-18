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

context("deprecated icd9 validation")

test_that("deprecated - warning for deprecation of icd9ValidDecimal, not given in test mode!", {
  expect_warning(icd9ValidDecimal(c("1", "2")), regexp = NA)
  expect_warning(icd9ValidShort(c("1", "2")), regexp = NA)
  expect_warning(icd9Valid(c("1", "2"), isShort = TRUE), regexp = NA)
})

test_that("deprecated - icd9IsValidDecimal - rubbish input", {
  expect_error(icd9IsValidDecimal(list(1230, c(12323, 2323), c("nonesnseses"))))
  expect_error(icd9IsValidDecimal(c(10.1, 200)))
  expect_equal(icd9IsValidDecimal(character()), logical())
  expect_false(icd9IsValidDecimal("."))
  expect_equal(icd9IsValidDecimal(c("100", "chestnut")), c(TRUE, FALSE))
  expect_false(icd9IsValidDecimal("100, 200")) # note this is a string with two numbers in it.
  expect_false(icd9IsValidDecimal(NA_character_))
  expect_equal(icd9IsValidDecimal(c("two", "things")), c(FALSE, FALSE))
})

test_that("deprecated - icd9IsValidDecimal numeric-only", {
  expect_false(icd9IsValidDecimal(""))
  expect_true(icd9IsValidDecimal("0"))
  expect_true(icd9IsValidDecimal("00"))
  expect_true(icd9IsValidDecimal("000"))
  expect_true(icd9IsValidDecimal("0.00")) # maybe warn for this one?
  expect_true(icd9IsValidDecimal("000.00"))
  expect_false(icd9IsValidDecimal("0000"))
  expect_true(icd9IsValidDecimal("100"))
  expect_true(icd9IsValidDecimal("010")) # a bit weird, but should validate
  # not enough zero padding? but not ambiguous.
  expect_true(icd9IsValidDecimal("01"))
  expect_true(icd9IsValidDecimal("1.1")) # a subtype of cholera
  expect_true(icd9IsValidDecimal("01.1")) # a subtype of cholera
  expect_true(icd9IsValidDecimal("001.1")) # a subtype of cholera
  expect_true(icd9IsValidDecimal("01.10"))
  expect_true(icd9IsValidDecimal("999.99"))
  expect_true(icd9IsValidDecimal(" 22.2 "))
  expect_true(icd9IsValidDecimal(" 33 "))
  expect_true(icd9IsValidDecimal("01.10"))
  expect_true(icd9IsValidDecimal("01.10"))
  expect_true(icd9IsValidDecimal("100."))
  expect_equal(
    icd9IsValidDecimal(c("100", "200.55", "V01.11")),
    c(TRUE, TRUE, TRUE))
  expect_equal(
    icd9IsValidDecimal(as.factor(c("0", "100", "222.22", "100", "1", "0"))),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(
    icd9IsValidDecimal(c("10.1", "100", "999.99", "0.01")),
    c(TRUE, TRUE, TRUE, TRUE))
})


test_that("deprecated - icd9IsValidDecimal numeric-only direct fun", {
  expect_false(icd9IsValidDecimalN(""))
  expect_true(icd9IsValidDecimalN("0"))
  expect_true(icd9IsValidDecimalN("00"))
  expect_true(icd9IsValidDecimalN("000"))
  expect_true(icd9IsValidDecimalN("0.00")) # maybe warn for this one?
  expect_true(icd9IsValidDecimalN("000.00"))
  expect_false(icd9IsValidDecimalN("0000"))
  expect_true(icd9IsValidDecimalN("100"))
  expect_true(icd9IsValidDecimalN("010")) # a bit weird, but should validate
  # not enough zero padding? but not ambiguous.
  expect_true(icd9IsValidDecimalN("01"))
  expect_true(icd9IsValidDecimalN("1.1")) # a subtype of cholera
  expect_true(icd9IsValidDecimalN("01.1")) # a subtype of cholera
  expect_true(icd9IsValidDecimalN("001.1")) # a subtype of cholera
  expect_true(icd9IsValidDecimalN("01.10"))
  expect_true(icd9IsValidDecimalN("999.99"))
  expect_true(icd9IsValidDecimalN(" 22.2 "))
  expect_true(icd9IsValidDecimalN(" 33 "))
  expect_true(icd9IsValidDecimalN("01.10"))
  expect_true(icd9IsValidDecimalN("01.10"))
  expect_true(icd9IsValidDecimalN("100."))
  expect_equal(
    icd9IsValidDecimalN(c("100", "200.55", "V01.11")),
    c(TRUE, TRUE, FALSE))
  expect_equal(
    icd9IsValidDecimalN(as.factor(c("0", "100", "222.22", "100", "1", "0"))),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(
    icd9IsValidDecimalN(c("10.1", "100", "999.99", "0.01")),
    c(TRUE, TRUE, TRUE, TRUE))
})

test_that("deprecated - icd9IsValidDecimal V codes", {
  expect_true(icd9IsValidDecimal("V55.55"))
  expect_true(icd9IsValidDecimal("V99. "))
  expect_true(icd9IsValidDecimal("V1.")) # looks horrible, but not ambiguous
  expect_false(icd9IsValidDecimal("V0"))
  expect_false(icd9IsValidDecimal("V."))
  expect_false(icd9IsValidDecimal("V.1"))
  expect_false(icd9IsValidDecimal("V123"))

  expect_true(icd9IsValidDecimalV("V55.55"))
  expect_true(icd9IsValidDecimalV("V99. "))
  expect_true(icd9IsValidDecimalV("V1.")) # looks horrible, but not ambiguous
  expect_false(icd9IsValidDecimalV("V0"))
  expect_false(icd9IsValidDecimalV("V."))
  expect_false(icd9IsValidDecimalV("V.1"))
  expect_false(icd9IsValidDecimalV("V123"))
})

test_that("deprecated - validate invalid decimal E codes", {

  expect_error(icd9IsValidDecimalE(8760))

  expect_false(icd9IsValidDecimalE(NA_character_))
  expect_false(icd9IsValidDecimalE("NA"))
  expect_false(icd9IsValidDecimalE(""))
  expect_false(icd9IsValidDecimalE(" "))
  expect_false(icd9IsValidDecimalE("   "))
  expect_false(icd9IsValidDecimalE("."))
  expect_false(icd9IsValidDecimalE("V10.1"))

  expect_false(icd9IsValidDecimal("0E1"))
  expect_false(icd9IsValidDecimal("E"))
  expect_false(icd9IsValidDecimal("E."))
  expect_false(icd9IsValidDecimal("E0000"))
  expect_false(icd9IsValidDecimal("E00000"))
  expect_false(icd9IsValidDecimal("E0000."))
  expect_false(icd9IsValidDecimal("E00000."))
  expect_false(icd9IsValidDecimal("E0000.0"))
  expect_false(icd9IsValidDecimal("E00000.0"))
  expect_false(icd9IsValidDecimal("E1000."))
  expect_false(icd9IsValidDecimal("E10000."))
  expect_false(icd9IsValidDecimal("E1000.0"))
  expect_false(icd9IsValidDecimal("E10000.0"))
  expect_false(icd9IsValidDecimal("E0000.1"))
  expect_false(icd9IsValidDecimal("E00000.1"))
  expect_false(icd9IsValidDecimal("E1000.1"))
  expect_false(icd9IsValidDecimal("E10000.1"))
  expect_false(icd9IsValidDecimal("E.1"))
  expect_false(icd9IsValidDecimal("E00000"))
  expect_false(icd9IsValidDecimal("E1234"))
  expect_false(icd9IsValidDecimal("E000.00"))
  expect_false(icd9IsValidDecimal("E999.12"))
  expect_false(icd9IsValidDecimal("E099.12"))
  expect_false(icd9IsValidDecimal("E99.12"))
  expect_false(icd9IsValidDecimal("E009.12"))
  expect_false(icd9IsValidDecimal("E099.12"))
  expect_false(icd9IsValidDecimal("E009.12"))
  expect_false(icd9IsValidDecimal("E99.9.12"))
  expect_false(icd9IsValidDecimal(".E999.1"))
  expect_false(icd9IsValidDecimal(".E9991"))
  expect_false(icd9IsValidDecimal("E0999"))
  expect_false(icd9IsValidDecimal("E999 9"))
  expect_false(icd9IsValidDecimal("E99 9"))
  expect_false(icd9IsValidDecimal("E9 9 9"))
  expect_false(icd9IsValidDecimal("E00 0"))
  expect_false(icd9IsValidDecimal("E9 9 9"))
  expect_false(icd9IsValidDecimal("E999 9 "))
  expect_false(icd9IsValidDecimal("E99 9 "))
  expect_false(icd9IsValidDecimal("E9 9 9 "))
  expect_false(icd9IsValidDecimal("E00 0 "))
  expect_false(icd9IsValidDecimal("E9 9 9 "))
  expect_false(icd9IsValidDecimal("E00 0 "))
})

test_that("deprecated - icd9IsValidDecimal valid E codes", {
  expect_true(icd9IsValidDecimal("E0")) #E000 is okay
  expect_true(icd9IsValidDecimal("E00"))
  expect_true(icd9IsValidDecimal("E000"))
  expect_true(icd9IsValidDecimal("E0."))
  expect_true(icd9IsValidDecimal("E00."))
  expect_true(icd9IsValidDecimal("E000."))
  expect_true(icd9IsValidDecimal("E0.0"))
  expect_true(icd9IsValidDecimal("E00.0"))
  expect_true(icd9IsValidDecimal("E000.0"))
  expect_true(icd9IsValidDecimal("E0.1"))
  expect_true(icd9IsValidDecimal("E00.1"))
  expect_true(icd9IsValidDecimal("E000.1"))
  expect_true(icd9IsValidDecimal("E999"))
  expect_true(icd9IsValidDecimal(" E999"))
  expect_true(icd9IsValidDecimal("E999 "))
  expect_true(icd9IsValidDecimal("E999. "))
  expect_true(icd9IsValidDecimal("E100")) # E001-E999 are okay, not just E800-E999
  expect_true(icd9IsValidDecimal("E100.1"))
  expect_true(icd9IsValidDecimal("E100."))
  expect_true(icd9IsValidDecimal("E010"))
  expect_true(icd9IsValidDecimal("E010.1"))
  expect_true(icd9IsValidDecimal("E010."))
  expect_true(icd9IsValidDecimal("E10"))
  expect_true(icd9IsValidDecimal("E10.1"))
  expect_true(icd9IsValidDecimal("E10."))
  expect_true(icd9IsValidDecimal("E001"))
  expect_true(icd9IsValidDecimal("E001.0"))
  expect_true(icd9IsValidDecimal("E001."))
  expect_true(icd9IsValidDecimal("E1"))
  expect_true(icd9IsValidDecimal("E1.0"))
  expect_true(icd9IsValidDecimal("E1."))
})

test_that("deprecated - icd9IsValidShort", {
  expect_equal(icd9IsValidShort(character()), logical())
  expect_error(icd9IsValidShort(list(1230, c(12323, 2323), c("nonesnseses"))))
  expect_true(icd9IsValidShort("0"))
  expect_equal(
    icd9IsValidShort(c("0", "00", "000", "0000", "00000")),
    c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_true(icd9IsValidShort("12345"))
  expect_true(icd9IsValidShort("12 "))
  expect_error(icd9IsValidShort(1))
  expect_equal(icd9IsValidShort(c("99999", "1")), c(TRUE, TRUE))
  expect_equal(icd9IsValidShort(c("1", "100", "222.22")), c(TRUE, TRUE, FALSE))
  expect_equal(
    icd9IsValidShort(as.factor(c("1", "100", "222.22", "100", "1.1"))),
    c(TRUE, TRUE, FALSE, TRUE, FALSE))
  expect_true(icd9IsValidShort("V1")) # not zero-padded, but not ambiguous.

  expect_false(icd9IsValidShort("jericho"))
  expect_false(icd9IsValidShort(""))
  expect_false(icd9IsValidShort("123456"))
  expect_false(icd9IsValidShort("10.1"))
  expect_false(icd9IsValidShort("11.22")) # am not expecting decimal points
})

test_that("deprecated - validate short form V codes - invalid codes", {
  expect_false(icd9IsValidShort("V"))
  expect_false(icd9IsValidShort("VV"))
  expect_false(icd9IsValidShort("Vbog"))
  expect_false(icd9IsValidShort(" V0"))
  expect_false(icd9IsValidShort("V00000"))
  expect_false(icd9IsValidShort("V123456"))
})

test_that("deprecated - validate short form V codes - valid codes", {
  expect_true(icd9IsValidShort("V234"))
  expect_true(icd9IsValidShort(" V45"))
})

test_that("deprecated - valid short form E codes - invalid input", {
  # call the E validation directly for these completely non-E codes
  expect_false(icd9IsValidShortE(NA_character_))
  expect_false(icd9IsValidShortE("NA"))
  expect_false(icd9IsValidShortE(""))
  expect_false(icd9IsValidShortE(" "))
  expect_false(icd9IsValidShortE("   "))
  expect_false(icd9IsValidShortE("."))
  # now test by routing through the higher level function (which is really the
  # only one that should be used)
  expect_false(icd9IsValidShort("0E1"))
  expect_false(icd9IsValidShort("E"))
  expect_false(icd9IsValidShort("E."))
  expect_false(icd9IsValidShort("E00000"))
  expect_false(icd9IsValidShort("E0."))
  expect_false(icd9IsValidShort("E00."))
  expect_false(icd9IsValidShort("E000."))
  expect_false(icd9IsValidShort("E0000."))
  expect_false(icd9IsValidShort("E00000."))
  expect_false(icd9IsValidShort("E0.0"))
  expect_false(icd9IsValidShort("E00.0"))
  expect_false(icd9IsValidShort("E000.0"))
  expect_false(icd9IsValidShort("E0000.0"))
  expect_false(icd9IsValidShort("E00000.0"))
  expect_false(icd9IsValidShort("E1000."))
  expect_false(icd9IsValidShort("E10000."))
  expect_false(icd9IsValidShort("E1000.0"))
  expect_false(icd9IsValidShort("E10000.0"))
  expect_false(icd9IsValidShort("E0.1"))
  expect_false(icd9IsValidShort("E00.1"))
  expect_false(icd9IsValidShort("E000.1"))
  expect_false(icd9IsValidShort("E0000.1"))
  expect_false(icd9IsValidShort("E00000.1"))
  expect_false(icd9IsValidShort("E1000.1"))
  expect_false(icd9IsValidShort("E10000.1"))
  expect_false(icd9IsValidShort("E.1"))
  expect_false(icd9IsValidShort("E00000"))
  expect_true(icd9IsValidShort("E1234"))
  expect_false(icd9IsValidShort("E000.00"))
  expect_false(icd9IsValidShort("E999.12"))
  expect_false(icd9IsValidShort("E099.12"))
  expect_false(icd9IsValidShort("E99.12"))
  expect_false(icd9IsValidShort("E009.12"))
  expect_false(icd9IsValidShort("E099.12"))
  expect_false(icd9IsValidShort("E009.12"))
  expect_false(icd9IsValidShort("E99.9.12"))
  expect_false(icd9IsValidShort(".E999.1"))
  expect_false(icd9IsValidShort(".E9991"))
  expect_false(icd9IsValidShort("E98765"))
})

test_that("deprecated - valid short form E codes - valid input", {
  # E000 is valid!
  # http://www.icd9data.com/2012/Volume1/E000-E999/E000-E000/E000/default.htm
  expect_true(icd9IsValidShort("E0"))
  expect_true(icd9IsValidShort("E00"))
  expect_true(icd9IsValidShort("E000"))
  expect_true(icd9IsValidShort("E0000"))
  expect_true(icd9IsValidShort("E999"))
  expect_true(icd9IsValidShort("E0999"))
  expect_true(icd9IsValidShort("e800"))
  expect_true(icd9IsValidShort(" E999"))
  expect_true(icd9IsValidShort("E999 "))
  expect_true(icd9IsValidShort("  E999 "))
  expect_true(icd9IsValidShort("E100")) # E001-E999 are okay, not just E800-E999
  expect_true(icd9IsValidShort("E1001"))
  expect_true(icd9IsValidShort("E010"))
  expect_true(icd9IsValidShort("E0101"))
  expect_true(icd9IsValidShort("E10"))
  expect_true(icd9IsValidShort("E001"))
  expect_true(icd9IsValidShort("E0010"))
  expect_true(icd9IsValidShort("E1"))
})

test_that("deprecated - test valid major numeric, valid", {
  expect_equal(
    icd9IsValidMajorN(c("1", "22", "333", "4444", "55555",
                        "1.1", "22.22", "333.333")),
    c(T, T, T, F, F, F, F, F)
  )
  expect_equal(
    icd9IsValidMajorN(c("01", "001", "22", "022", "0333", "04444",
                        "0055555", "01.1", "022.22", "00333.333")),
    c(T, T, T, T, F, F, F, F, F, F)
  )
})

test_that("deprecated - test major validation", {
  expect_equal(
    icd9IsValidMajor(c("", "1", "22", "333", "4444", "V", "V2", "V34",
                       "V567", "E", "E1", "E000", "E70", "E300", "E876")),
    c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE,
      FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
  expect_equal(icd9IsValidMajorE(c("E", "E1", "E000", "E70", "E300", "E876")),
               c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE))

  expect_equal(icd9IsValidMajorV(c("", "1", "22", "333", "4444", "V", "V2", "V34",
                                   "V567", "E", "E1", "E000", "E70", "E300", "E876",
                                   "V1.1", "V2.89", "V12.4", "V23.45")),
               c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
                 FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                 FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("deprecated - icd-9 code is really in the list, not just syntactically valid", {
  expect_true(icd9IsRealShort("8027"))
  expect_true(icd9IsRealShort("E9329"))
  expect_false(icd9IsRealShort("J8027"))
  expect_false(icd9IsRealShort("802.7"))
  expect_true(icd9IsRealDecimal("802.7"))
  expect_true(icd9IsRealDecimal("E932.9"))
  expect_false(icd9IsRealDecimal("E9329"))
  expect_false(icd9IsRealDecimal("J80.27"))
  expect_false(icd9IsRealDecimal("V802.7"))
  expect_false(icd9IsReal("V802.7", isShort = FALSE))

  expect_equal(icd9IsRealDecimal("V802.7"), FALSE)
  expect_equal(
    icd9IsReal(c("8027", "E9329", "E000", "armitage"), isShort = TRUE),
    c(TRUE, TRUE, TRUE, FALSE)
  )
})

pts_invalid_mix <- data.frame(
  visit_id = c(1000, 1000, 1001),
  icd9 = c("27801", "invalides", "25001"),
  poa = factor(c("Y", "N", "Y"))
)

test_that("deprecated - filter valid - bad input", {
  expect_error(icd9FilterValid())
  expect_error(icd9FilterValid(list(j = "k")))
})

test_that("deprecated - get valid - vector input", {

  expect_equal(icd9GetValid("100"), "100")
  expect_equal(icd9GetValid("nolk"), character())
  expect_equal(icd9GetValid(c("100", "nolk")), "100")
  expect_equal(icd9GetValidDecimal(c("10.0", "100.x")), "10.0")
  expect_equal(icd9GetValidShort("nolk"), character())
  expect_equal(icd9GetValidShort(c("V100", "nolk")), "V100")
  expect_equal(icd9GetInvalid(c("100", "nolk")), "nolk")
})

test_that("deprecated - filter valid - data frame input", {

  expect_equal(icd9FilterValid(pts_invalid_mix), pts_invalid_mix[c(1, 3), ])

  expect_equal(icd9FilterInvalid(pts_invalid_mix), pts_invalid_mix[2, ])
  expect_equal(icd9FilterValid(pts_invalid_mix, invert = TRUE),
               pts_invalid_mix[2, ])

  # no non-short so all are invalid:
  expect_equal(icd9FilterValid(pts_invalid_mix, invert = TRUE, isShort = FALSE),
               pts_invalid_mix)
  # arg order irrelevant, but can be mixed up in S3 dispatch.
  expect_equal(icd9FilterValid(pts_invalid_mix, isShort = FALSE, invert = TRUE),
               pts_invalid_mix)

  # use invert and isShort args:
  expect_equal(icd9FilterValid(pts_invalid_mix, isShort = TRUE, invert = TRUE),
               pts_invalid_mix[2, ])
  expect_equal(icd9FilterValid(pts_invalid_mix, isShort = TRUE, invert = FALSE),
               pts_invalid_mix[c(1, 3 ), ])
})

test_that("deprecated - validate mappings", {
  expect_true(icd9IsValidMappingDecimal(list(a = "100.1", b = "202.3")))
  expect_true(icd9IsValidMappingShort(list(a = "1001", b = "2023")))
  expect_false(icd9IsValidMappingDecimal(list(a = "1001", b = "2023")))
  expect_false(icd9IsValidMappingShort(list(a = "100.1", b = "202.3")))

  expect_false(icd9IsValidMapping(list(a = "car", b = "et"), isShort = FALSE))
  expect_true(icd9IsValidMapping(list(a = "1001", b = "2023"), isShort = TRUE))
})

test_that("deprecated - get invalid codes in a decimal map", {

  expect_equal(
    icd9GetInvalidMappingDecimal(list(a = "100.1", b = "2023")),
    list(b = "2023")
  )

})

test_that("deprecated - get invalid codes in a short code map", {

  expect_equal(
    icd9GetInvalidMappingShort(list(a = "100.1", b = "2023")),
    list(a = "100.1")
  )

})

test_that("deprecated - get invalid decimals", {
  expect_equal(icd9GetInvalidDecimal(c("10.1", "rhubarb", "3000")), c("rhubarb", "3000"))
})

test_that("deprecated - get real codes from a longer list", {
  expect_equal(icd9GetRealShort(c("003", "0031", "0032"), onlyBillable = TRUE), "0031")
  expect_equal(icd9GetRealDecimal(c("003", "003.1", "3.2"), onlyBillable = TRUE), "003.1")
})

test_that("deprecated - get real codes which are less than two digit major", {
  expect_equal(icd9GetRealShort(c("3", "11", "V2"), onlyBillable = FALSE), c("3", "11", "V2"))
  expect_equal(icd9GetRealDecimal(c("3", "11", "V2"), onlyBillable = FALSE), c("3", "11", "V2"))
}
)
test_that("deprecated - illable codes are identified", {
  expect_true(icd9IsBillable("1000"))
  expect_false(icd9IsBillable("1008"))
  expect_true(icd9IsBillable("1009"))
})

test_that("deprecated - test valid alias", {
  x <- generate_random_decimal_icd9(200)
  expect_identical(icd9Valid(x, isShort = FALSE), icd9IsValid(x, isShort = FALSE))

  x <- generate_random_short_icd9(200)
  expect_identical(icd9Valid(x, isShort = TRUE), icd9IsValid(x, isShort = TRUE))

})

# backported
#
test_that("get subset of billable codes", {
  x <- c("410", "4100", "41000")
  expect_equivalent(icd9GetBillable(x) %>% unclass, "41000")
  expect_equivalent(icd9GetBillableShort(x) %>% unclass, "41000")
  expect_equivalent(icd9GetBillableDecimal(x) %>% unclass, character(0))
  x <- c("410", "410.0", "410.00")
  expect_equivalent(icd9GetBillableDecimal(x) %>% unclass, "410.00")
})

test_that("get inverted subset of billable codes", {
  x <- c("410", "410.0", "410.00")
  expect_equivalent(icd9GetBillable(x, invert = TRUE) %>% unclass, c("410", "410.0"))
})

test_that("NonBillable", {
  x <- c("410", "4100", "41000")
  expect_equal(icd9GetNonBillable(x), as.icd9cm(as.icd_short_diag(c("410", "4100"))))
  expect_equal(icd9GetNonBillableShort(x), as.icd9cm(as.icd_short_diag(c("410", "4100"))))
  expect_equivalent(icd9GetNonBillableDecimal(x), as.icd9cm(as.icd_short_diag(x)))
  x <- c("410", "410.0", "410.00")
  expect_equivalent(icd9GetNonBillableDecimal(x), as.icd9cm(as.icd_short_diag(c("410", "410.0"))))
})

test_that("deprecated - valid short n", {
  x <- icd9RandomShortN(100)
  expect_true(all(icd9IsValidShortN(x)))
})

test_that("deprecated - valid short v", {
  x <- icd9RandomShortV(100)
  expect_true(all(icd9IsValidShortV(x)))
})

test_that("deprecated - valid short e", {
  x <- icd9RandomShortE(100)
  expect_true(all(icd9IsValidShortE(x)))
})

