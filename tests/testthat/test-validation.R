context("icd9 validation")

test_that("icd9ValidDecimal - rubbish input", {
  expect_error(icd9ValidDecimal(list(1230, c(12323, 2323), c("nonesnseses"))))
  expect_null(icd9ValidDecimal(c()))
  expect_false(icd9ValidDecimal("."))
  expect_equal(icd9ValidDecimal(c("100", "chestnut")), c(TRUE, FALSE))
  #expect_warning(naVal <- icd9ValidDecimal("100, 200")) # note this is a string with two numbers in it... TODO? could warn if any commas or other separators.
  #expect_equal(naVal, NA)
  expect_equal(icd9ValidDecimal(c("two", "things")), c(FALSE, FALSE))
})

test_that("icd9ValidDecimal numeric-only", {
  expect_false(icd9ValidDecimal(""))
  expect_true(icd9ValidDecimal("0"))
  expect_true(icd9ValidDecimal("00"))
  expect_true(icd9ValidDecimal("000"))
  expect_true(icd9ValidDecimal("0.00")) # maybe warn for this one?
  expect_true(icd9ValidDecimal("000.00"))
  expect_false(icd9ValidDecimal("0000"))
  expect_true(icd9ValidDecimal("100"))
  expect_true(icd9ValidDecimal("010")) # a bit weird, but should validate
  expect_true(icd9ValidDecimal("01")) # not enough zero padding? but not ambiguous.
  expect_true(icd9ValidDecimal("1.1")) # a subtype of cholera
  expect_true(icd9ValidDecimal("01.1")) # a subtype of cholera
  expect_true(icd9ValidDecimal("001.1")) # a subtype of cholera
  expect_true(icd9ValidDecimal("01.10"))
  expect_true(icd9ValidDecimal("999.99"))
  expect_true(icd9ValidDecimal(" 22.2 "))
  expect_true(icd9ValidDecimal(" 33 "))
  expect_true(icd9ValidDecimal("01.10"))
  expect_true(icd9ValidDecimal("01.10"))
  expect_true(icd9ValidDecimal("100."))
  expect_equal(icd9ValidDecimal(c("100", "200.55", "V01.11")), c(TRUE, TRUE, TRUE))
  expect_equal(icd9ValidDecimal(as.factor(c("0", "100", "222.22", "100", "1", "0"))), c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(icd9ValidDecimal(c("10.1", "100", "999.99", "0.01")), c(TRUE, TRUE, TRUE, TRUE))
})

test_that("icd9ValidDecimal V codes", {
  expect_true(icd9ValidDecimal("V55.55"))
  expect_true(icd9ValidDecimal("V99. "))
  expect_true(icd9ValidDecimal("V1.")) # looks horrible, but not ambiguous
  expect_false(icd9ValidDecimal("V0"))
  expect_false(icd9ValidDecimal("V."))
  expect_false(icd9ValidDecimal("V.1"))
  expect_false(icd9ValidDecimal("V123"))
})

test_that("icd9ValidDecimal E codes", {
  expect_true(icd9ValidDecimal("E999"))
  expect_true(icd9ValidDecimal(" E999"))
  expect_true(icd9ValidDecimal("E999 "))
  expect_true(icd9ValidDecimal("E999. "))
  expect_false(icd9ValidDecimal("E1"))
  expect_false(icd9ValidDecimal("E."))
  expect_false(icd9ValidDecimal("E.1"))
  expect_false(icd9ValidDecimal("E1."))
  expect_false(icd9ValidDecimal("E00000"))
  expect_false(icd9ValidDecimal("E000.00"))
  expect_false(icd9ValidDecimal("E100")) # E800-E999 defined
  expect_false(icd9ValidDecimal("E0999")) # this is 099.9 which is not a valid E code
  # TODO: more E validation
})

test_that("icd9ValidShort", {
  expect_error(icd9ValidShort(list(1230, c(12323, 2323), c("nonesnseses"))))
  #expect_false(icd9ValidShort("0"))
  expect_true(icd9ValidShort("0"))
  expect_equal(icd9ValidShort(c("0", "00", "000", "0000", "00000")), c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_true(icd9ValidShort("12345"))
  expect_true(icd9ValidShort("12 "))
  expect_error(icd9ValidShort(1))
  expect_equal(icd9ValidShort(c("99999", "1")), c(TRUE, TRUE))
  expect_equal(icd9ValidShort(c("1", "100", "222.22")), c(TRUE, TRUE, FALSE))
  expect_equal(icd9ValidShort(as.factor(c("1", "100", "222.22", "100", "1.1"))), c(TRUE, TRUE, FALSE, TRUE, FALSE))
  expect_true(icd9ValidShort("V1")) # not zero-padded, but not ambiguous. Should pass.

  expect_false(icd9ValidShort("jericho"))
  expect_false(icd9ValidShort(""))
  expect_false(icd9ValidShort("123456"))
  expect_false(icd9ValidShort("10.1"))
  expect_false(icd9ValidShort("11.22")) # am not expecting decimal points
  expect_false(icd9ValidShort("V"))
  expect_false(icd9ValidShort("VV"))
  expect_false(icd9ValidShort("V0"))
  expect_false(icd9ValidShort("V00000"))
  expect_false(icd9ValidShort("V123456"))
  expect_true(icd9ValidShort("V234"))
  expect_true(icd9ValidShort(" V45"))

  expect_false(icd9ValidShort("0E111"))
  expect_false(icd9ValidShort(" E0345"))

})

test_that("test major validation", {
  expect_equal(
    icd9ValidMajor(c("", "1", "22", "333", "4444", "V", "V2", "V34", "V567", "E", "E1", "E70", "E300", "E876")),
    c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("stop if invalid", {
  expect_error(stopIfInvalidIcd9('notvalidicd9', short = TRUE))
})

test_that("warn if invalid", {
  expect_warning(warnIfInvalidIcd9('notvalidicd9', short = TRUE))
})

test_that("NA warn stop switch handles NA values", {

  expect_that(icd9ValidNaWarnStopShort(NA_character_, invalidAction = "ignore"), not(gives_warning()))
  expect_that(icd9ValidNaWarnStopShort(NA_character_, invalidAction = "silent"), not(gives_warning()))
  expect_that(warnNa <- icd9ValidNaWarnStopShort(NA_character_, invalidAction = "warn"), gives_warning())
  expect_that(icd9ValidNaWarnStopShort(NA_character_, invalidAction = "stop"), throws_error())
  expect_that(icd9ValidNaWarnStopShort(NA, invalidAction = "ignore"), not(throws_error()))
  expect_that(icd9ValidNaWarnStopShort(NA, invalidAction = "silent"), throws_error()) # this is now an incorrect data type (logical/numeric NA, not NA_character_
  expect_that(icd9ValidNaWarnStopShort(NA, invalidAction = "warn"), throws_error())
  expect_that(icd9ValidNaWarnStopShort(NA, invalidAction = "stop"), throws_error())

  expect_equal(icd9ValidNaWarnStopShort(NA, invalidAction = "ignore"), NA) # ignore just passes through, but this is really not something test-worthy...
  expect_equal(icd9ValidNaWarnStopShort(NA_character_, invalidAction = "ignore"), NA_character_)
  expect_equal(icd9ValidNaWarnStopShort(NA_character_, invalidAction = "silent"), NA_character_)
  expect_equal(warnNa, NA_character_)
})
