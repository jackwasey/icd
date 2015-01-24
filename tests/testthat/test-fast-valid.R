context("icd9 validation")

test_that("icd9IsValidDecimal - rubbish input", {
  expect_error(icd9IsValidDecimal(list(1230, c(12323, 2323), c("nonesnseses"))))
  expect_equal(icd9IsValidDecimal(character()), logical())
  expect_false(icd9IsValidDecimal("."))
  expect_equal(icd9IsValidDecimal(c("100", "chestnut")), c(TRUE, FALSE))
  #expect_warning(naVal <- icd9IsValidDecimal("100, 200")) # note this is a string
  #with two numbers in it... TODO? could warn if any commas or other separators.
  #expect_equal(naVal, NA)
  expect_equal(icd9IsValidDecimal(c("two", "things")), c(FALSE, FALSE))
})

test_that("icd9IsValidDecimal numeric-only", {
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

test_that("icd9IsValidDecimal V codes", {
  expect_true(icd9IsValidDecimal("V55.55"))
  expect_true(icd9IsValidDecimal("V99. "))
  expect_true(icd9IsValidDecimal("V1.")) # looks horrible, but not ambiguous
  expect_false(icd9IsValidDecimal("V0"))
  expect_false(icd9IsValidDecimal("V."))
  expect_false(icd9IsValidDecimal("V.1"))
  expect_false(icd9IsValidDecimal("V123"))
})

test_that("validate invalid decimal E codes", {

  # okay, maybe this should error, but the vast majority of the time, only
  # character input will be presented to this function
  expect_false(icd9IsValidDecimalE(8760))

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

test_that("icd9IsValidDecimal valid E codes", {
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

test_that("icd9IsValidShort", {
  expect_equal(icd9IsValidShort(character()), logical())
  expect_error(icd9IsValidShort(list(1230, c(12323, 2323), c("nonesnseses"))))
  #expect_false(icd9IsValidShort("0"))
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

# TODO: more V code tests
test_that("validate short form V codes - invalid codes", {
  expect_false(icd9IsValidShort("V"))
  expect_false(icd9IsValidShort("VV"))
  expect_false(icd9IsValidShort("V0"))
  expect_false(icd9IsValidShort("V00000"))
  expect_false(icd9IsValidShort("V123456"))
})

test_that("validate short form V codes - valid codes", {
  expect_true(icd9IsValidShort("V234"))
  expect_true(icd9IsValidShort(" V45"))
})

test_that("valid short form E codes - invalid input", {
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
  #expect_true(icd9IsValidShort("E1234"))
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

test_that("valid short form E codes - valid input", {
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

test_that("test valid major numeric, valid", {
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

test_that("test major validation", {
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

test_that("icd-9 code is really in the list, not just syntactically valid", {
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
  expect_equal(icd9IsReal(c("8027", "E9329", "E000", "armitage"),
                          isShort = TRUE), c(TRUE, TRUE, TRUE, FALSE))
})

mixInvalidPts <- data.frame(
  visitId = c(1000, 1000, 1001),
  icd9 = c("27801", "invalides", "25001"),
  poa = factor(c("Y", "N", "Y"))
)

test_that("filter valid - bad input", {
  expect_error(icd9FilterValid())
  expect_error(icd9FilterValid(list(j = "k")))
})

test_that("filter valid - vector input", {

  expect_equal(icd9FilterValid("100"), "100")
  expect_equal(icd9FilterValid("nolk"), character())
  expect_equal(icd9FilterValid(c("100", "nolk")), "100")
  expect_equal(icd9FilterInvalid(c("100", "nolk")), "nolk")
  expect_equal(icd9FilterValid(c("100", "nolk"), invert = TRUE), "nolk")
})

test_that("filter valid - data frame input", {

  expect_equal(icd9FilterValid(mixInvalidPts), mixInvalidPts[c(1,3), ])

  expect_equal(icd9FilterInvalid(mixInvalidPts), mixInvalidPts[2, ])
  expect_equal(icd9FilterValid(mixInvalidPts, invert = TRUE),
               mixInvalidPts[2, ])

  # no non-short so all are invalid:
  expect_equal(icd9FilterValid(mixInvalidPts, invert = TRUE, isShort = FALSE),
               mixInvalidPts)
  # arg order irrelevant, but can be mixed up in S3 dispatch.
  expect_equal(icd9FilterValid(mixInvalidPts, isShort = FALSE, invert = TRUE),
               mixInvalidPts)

  # use invert and isShort args:
  expect_equal(icd9FilterValid(mixInvalidPts, isShort = TRUE, invert = TRUE),
               mixInvalidPts[2, ])
  expect_equal(icd9FilterValid(mixInvalidPts, isShort = TRUE, invert = FALSE),
               mixInvalidPts[c(1,3), ])
})

test_that("validate mappings", {
  # TODO: check all real, also?
  expect_true(icd9IsValidMappingDecimal(list(a="100.1", b="202.3")))
  expect_true(icd9IsValidMappingShort(list(a="1001", b="2023")))
  expect_false(icd9IsValidMappingDecimal(list(a="1001", b="2023")))
  expect_false(icd9IsValidMappingShort(list(a="100.1", b="202.3")))

  expect_false(icd9IsValidMapping(list(a="car", b="et"), isShort = FALSE))
  expect_true(icd9IsValidMapping(list(a="1001", b="2023"), isShort = TRUE))
})

test_that("get invalid decimals", {
  expect_equal(icd9GetInvalidDecimal(c("10.1", "rhubarb", "3000")), c("rhubarb", "3000"))
})

test_that("get real codes from a longer list", {
  expect_equal(icd9GetRealShort(c("003", "0031", "0032"), majorOk = FALSE), "0031")
  expect_equal(icd9GetRealDecimal(c("003", "003.1", "3.2"), majorOk = FALSE), "003.1")
})
