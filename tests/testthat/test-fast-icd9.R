context("test icd9 package")

test_that("zero pad decimal - bad input", {
  expect_equal(icd9AddLeadingZeroesDecimal(character()), character())
  expect_equal(icd9AddLeadingZeroesDecimal(NA_character_), NA_character_)
})

test_that("zero pad decimal, numeric only", {

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

test_that("zero pad decimel V and E codes", {
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

test_that("zero pad short invalid codes", {

  expect_equal(icd9AddLeadingZeroesShort(character()), character())
  # behavior of "ignore" for incorrect input is undefined, so don't test:
  # expect_equal(
  #   icd9AddLeadingZeroesShort("anything", invalidAction = "ignore"),
  #   "anything")
  expect_equal(icd9AddLeadingZeroesShort("anything"),
               NA_character_)
  expect_equal(icd9AddLeadingZeroesShort("anything"), NA_character_)
  expect_equal(icd9AddLeadingZeroesShort(NA_character_), NA_character_)
  # this is just re-checking the validation code...
  expect_equal(icd9AddLeadingZeroesShort("V012"), "V012")
  expect_equal(icd9AddLeadingZeroesShort("V199"), "V199")
  # at least we confirm we follow the invalidAction directive correctly.
  #expect_error(icd9AddLeadingZeroesShort("1.1"))
})

test_that("zero pad short", {
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

test_that("icd9 parts to short form numeric input", {
  expect_equal(icd9AddLeadingZeroesMajor(1L), "001")
  expect_equal(icd9AddLeadingZeroesMajor(10L), "010")
  expect_equal(icd9AddLeadingZeroesMajor(999L), "999")
  expect_equal(icd9AddLeadingZeroesMajor(10.1), NA_character_)
})

test_that("add leading zeroes to V (and E) majors", {

  expect_equal(icd9AddLeadingZeroesMajor("V1"), "V01")
  expect_equal(icd9AddLeadingZeroesMajor("V2"), "V02")
  expect_equal(icd9AddLeadingZeroesMajor("V03"), "V03")
  expect_equal(icd9AddLeadingZeroesMajor(c("10", "V05")),
               c("010", "V05"))
  expect_equal(icd9AddLeadingZeroesMajor("E915"), "E915")
})

test_that("add leading zeroes to majors, invalid input", {
  expect_equal(icd9AddLeadingZeroesMajor("E9"), "E009")
  # should be minimally valid code
  expect_equal(icd9AddLeadingZeroesMajor("E"), NA_character_)
  expect_equal(icd9AddLeadingZeroesMajor("V"), NA_character_)
  expect_equal(icd9AddLeadingZeroesMajor("jasmine"), NA_character_)
  # error if validating
  expect_equal(icd9AddLeadingZeroesMajor("E"), NA_character_)
  expect_equal(icd9AddLeadingZeroesMajor("V"), NA_character_)
  expect_equal(icd9AddLeadingZeroesMajor("jasmine"), NA_character_)
  # minimal validation of major, should just give back E codes.
  # expect_equal(icd9AddLeadingZeroesMajor("E9"), "E9")
})

test_that("all generated icd9 lookup tables are valid!", {

})

test_that("icd9ExtractAlphaNumeric", {

  expect_equal(icd9ExtractAlphaNumeric("V12"),
               matrix(data = c("V", "12"), ncol = 2))
  expect_equal(icd9ExtractAlphaNumeric(c("V12", 34)),
               t(matrix(data = c("V", "12", "", "34"), ncol = 2)))

})

test_that("strip leading zeroes: errors", {

  #TODO: decide whether to validate these, and make NAs, or just produce garbage.
  #expect_equal(icd9DropLeadingZeroesDecimal("sandwiches"), NA_character_)
  #expect_equal(icd9DropLeadingZeroesDecimal("VE123456.789"), NA_character_)
  #expect_equal(icd9DropLeadingZeroesDecimal("V10.1,E989.2"), NA_character_)
})

test_that("strip leading zero from decimal numeric only", {

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
})

test_that("strip leading zero from decimal V and E", {

  expect_equal(icd9DropLeadingZeroesDecimal("V1"), "V1")
  expect_equal(icd9DropLeadingZeroesDecimal("V01"), "V1")
  #TODO: expect_equal(icd9DropLeadingZeroesDecimal("V1."), "V1")
  #TODO: expect_equal(icd9DropLeadingZeroesDecimal("V01."), "V1")
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

test_that("strip leading zero from short numeric only", {

  # TODO: expect_equal(icd9DropLeadingZeroesShort(NA_character_), NA_character_)
  # must have zero to be valid (001.2)
  expect_equal(icd9DropLeadingZeroesShort("0012"), "0012")
  # must have zero to be valid (001.23)
  expect_equal(icd9DropLeadingZeroesShort("00123"), "00123")
  expect_equal(icd9DropLeadingZeroesShort("0124"), "0124")
  expect_equal(icd9DropLeadingZeroesShort("01278"), "01278")
  expect_equal(icd9DropLeadingZeroesShort("1239"), "1239")
  expect_equal(icd9DropLeadingZeroesShort("12387"), "12387")
})

test_that("strip leading zero from decimal V and E", {

  expect_equal(icd9DropLeadingZeroesShort("V1"), "V01")
  expect_equal(icd9DropLeadingZeroesShort("V12"), "V12")
  expect_equal(icd9DropLeadingZeroesShort("V123"), "V123")
  # cannot drop zero and be the same code. This is an important test!
  expect_equal(icd9DropLeadingZeroesShort("V012"), "V012")
  expect_equal(icd9DropLeadingZeroesShort("V1278"), "V1278")
  expect_equal(icd9DropLeadingZeroesShort("E912"), "E912")
  expect_equal(icd9DropLeadingZeroesShort("E9127"), "E9127")

  test_that("mixed vector drop leading zero short", {
    expect_equal(icd9DropLeadingZeroesShort(c("V1278", " E898", "02", "0345")),
                 c("V1278", "E898", "002", "0345"))
  })
})

test_that("drop leading zeroes from majors: invalid input", {
  # this is a little dangerous. dropping zeroes from a major is only valid for
  # short codes if the minor is empty, but this function is unaware of this.
  # TODO expect_equal(icd9DropLeadingZeroesMajor(""), NA_character_)
  expect_equal(icd9DropLeadingZeroesMajor(NA), NA_character_)
#   expect_error(icd9DropLeadingZeroesMajor("54321"))
#   expect_error(icd9DropLeadingZeroesMajor(1.5))
#   expect_error(icd9DropLeadingZeroesMajor(pi))
#   expect_error(icd9DropLeadingZeroesMajor("V10.20"))
#   expect_error(icd9DropLeadingZeroesMajor("E9127"))
#   expect_error(icd9DropLeadingZeroesMajor("rhubarb"))
})

test_that("drop leading zeroes from majors: numeric input", {
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

test_that("drop leading zeroes from majors: V codes", {
  expect_equal(icd9DropLeadingZeroesMajor("V1"), "V1")
  #TODO: expect_equal(icd9DropLeadingZeroesMajor(" v01 "), "V1")
  expect_equal(icd9DropLeadingZeroesMajor("V01"), "V1")
  expect_equal(icd9DropLeadingZeroesMajor("V12"), "V12")
})

test_that("drop leading zeroes from majors: E codes", {
  expect_equal(icd9DropLeadingZeroesMajor("E800"), "E800")
  expect_equal(icd9DropLeadingZeroesMajor(" e812 "), "e812")
})
