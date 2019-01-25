context("icd9 type conversions")

test_that("extract decimal parts - invalid or empty input", {
  expect_equal(decimal_to_parts.icd9(character()), list(mjr = character(),
                                                        mnr = character()))
  expect_equal(decimal_to_parts.icd10(character()), list(mjr = character(),
                                                         mnr = character()))
  expect_equal(decimal_to_parts.icd9(""),
               list(mjr = NA_character_, mnr = NA_character_))
  expect_equal(decimal_to_parts.icd10(""),
               list(mjr = NA_character_, mnr = NA_character_))
  expect_equal(decimal_to_parts(""),
               list(mjr = NA_character_, mnr = NA_character_))
  expect_equal(
    decimal_to_parts.icd9("", mnr_empty = NA_character_),
    list(mjr = NA_character_, mnr = NA_character_)
  )
  expect_equal(
    decimal_to_parts.icd10("", mnr_empty = NA_character_),
    list(mjr = NA_character_, mnr = NA_character_)
  )
})

test_that("extract decimal parts - valid inputs", {
  expect_equal(decimal_to_parts.icd9("0"), list(mjr = "000", mnr = ""))
  expect_equal(decimal_to_parts.icd9("000"), list(mjr = "000", mnr = ""))
  expect_equal(decimal_to_parts.icd9("V1.2"), list(mjr = "V01", mnr = "2"))
  expect_equal(decimal_to_parts.icd9("1.1"), list(mjr = "001", mnr = "1"))
  expect_equal(decimal_to_parts.icd9("001.1"), list(mjr = "001", mnr = "1"))
  expect_equal(decimal_to_parts.icd9("22.22"), list(mjr = "022", mnr = "22"))
  expect_equal(decimal_to_parts.icd9("022.22"), list(mjr = "022", mnr = "22"))
  expect_equal(decimal_to_parts.icd9("333.3"), list(mjr = "333", mnr = "3"))
  expect_equal(decimal_to_parts.icd9("444"), list(mjr = "444", mnr = ""))
  expect_equal(decimal_to_parts.icd9("444", mnr_empty = NA_character_),
               list(mjr = "444", mnr = NA_character_))
  expect_equal(decimal_to_parts.icd9("444", mnr_empty = ""),
               list(mjr = "444", mnr = ""))
  expect_equal(decimal_to_parts.icd9(c("9.9", "88.88", "777.6")),
               list(
                 mjr = c("009", "088", "777"),
                 mnr = c("9", "88", "6")
               )
  )

  expect_equal(decimal_to_parts.icd9(c("009.9", "088.88", "777.6")),
               list(
                 mjr = c("009", "088", "777"),
                 mnr = c("9", "88", "6")
               )
  )
  expect_equal(
    decimal_to_parts.icd9(c("9.9", "88", "777.6"), mnr_empty = NA_character_),
    list(
      mjr = c("009", "088", "777"),
      mnr = c("9", NA_character_, "6")
    )
  )

  expect_equal(decimal_to_parts.icd9(c("9.9", "88", "777.6"), mnr_empty = ""),
               list(
                 mjr = c("009", "088", "777"),
                 mnr = c("9", "", "6")
               )
  )

  expect_equal(
    decimal_to_parts.icd9(c("001", "", "991.23"), mnr_empty = NA_character_),
    list(
      mjr = c("001", NA_character_, "991"),
      mnr = c(NA_character_, NA_character_, "23")
    )
  )
})

test_that("icd9 decimal to short form, bad codes", {
  expect_equal_no_icd(decimal_to_short.icd9(character()), character())
})

test_that("icd9 decimal to short form", {
  expect_equal_no_icd(decimal_to_short.icd9("1"), "001")
  expect_equal_no_icd(decimal_to_short.icd9("1.1"), "0011")
  expect_equal_no_icd(decimal_to_short.icd9("1.23"), "00123")
  expect_equal_no_icd(decimal_to_short.icd9("81"), "081")
  expect_equal_no_icd(decimal_to_short.icd9("81.1"), "0811")
  expect_equal_no_icd(decimal_to_short.icd9("81.23"), "08123")
  expect_equal_no_icd(decimal_to_short.icd9("991"), "991")
  expect_equal_no_icd(decimal_to_short.icd9("991.1"), "9911")
  expect_equal_no_icd(decimal_to_short.icd9("991.23"), "99123")

  expect_equal_no_icd(
    decimal_to_short.icd9(c("1", "991.23")), c("001", "99123"))
  expect_equal_no_icd(
    decimal_to_short.icd9(c("1.", "991.23")), c("001", "99123"))
  expect_equal_no_icd(
    decimal_to_short.icd9(c("1", NA, "991.23")), c("001", NA, "99123"))
})

test_that("icd9 decimal to short with factor input", {
  expect_is(decimal_to_short(factor(c("1.", "991.23"))), "factor")
})

test_that("icd10 decimal to short with factor input", {
  expect_is(decimal_to_short(factor(c("A10.1", "Z11.9AX"))), "factor")
})

test_that("short to decimal, numbers", {
  # if there is anything after decimal, zeroes must be there!
  expect_equal_no_icd(decimal_to_short.icd9("1.0"), "0010")
  expect_equal_no_icd(decimal_to_short.icd9("1"), "001")
  expect_equal_no_icd(decimal_to_short.icd9("22"), "022")
  expect_equal_no_icd(decimal_to_short.icd9("345"), "345")
  expect_equal_no_icd(short_to_decimal.icd9("013"), "013")
  expect_equal_no_icd(short_to_decimal.icd9("V013"), "V01.3")
})

test_that("short to decimal bad input", {
  expect_equal_no_icd(short_to_decimal.icd9(character()), character())
  expect_equal_no_icd(short_to_decimal.icd9("valsalva"), NA_character_)
  expect_equal_no_icd(short_to_decimal.icd9("123456"), NA_character_)
  expect_equal_no_icd(short_to_decimal.icd9(""), NA_character_)
  # NA is not character type, so expect error.
  expect_equal_no_icd(short_to_decimal.icd9(NA), NA_character_)
  # NA is not character type, so expect error.
  expect_equal_no_icd(short_to_decimal.icd9(c("000000", "0ab1bc2d")),
                      c(NA_character_, NA_character_))
})

test_that("decimal to short with single value, no dispatch", {
  # intended behavior for unspecified type not to pad, to avoid expensive guess
  expect_equal_no_icd(decimal_to_short("1"), decimal_to_short(as.icd9("1")))
})

test_that("icd10 short to decimal", {
  expect_true(is.decimal_diag(short_to_decimal("A00")))
  expect_true(is.decimal_diag(short_to_decimal.icd10("A00")))
  expect_true(is.decimal_diag(short_to_decimal.icd10cm("A00")))
  expect_true(is.decimal_diag(short_to_decimal("A009")))
  expect_true(is.decimal_diag(short_to_decimal.icd10("A009")))
  expect_true(is.decimal_diag(short_to_decimal.icd10cm("A009")))

  expect_equal(
    short_to_decimal("A00"), as.icd10("A00") %>% as.decimal_diag)
  expect_equal(
    short_to_decimal.icd10("A00"), as.icd10("A00") %>% as.decimal_diag)
  expect_equal(
    short_to_decimal("A000"), as.icd10("A00.0") %>% as.decimal_diag)
  expect_equal(
    short_to_decimal.icd10("A000"), as.icd10("A00.0") %>% as.decimal_diag)

})

test_that("#97 is fixed", {
  expect_warning(res <- short_to_decimal(icd10_map_elix$CHF), NA)
  expect_true("P29.0" %in% res)
  expect_false("I43." %in% res)
})

test_that("icd10 short to decimal for multiple codes", {
  # pick up specific bug where a warning was given for muliple codes
  expect_warning(
    res <- short_to_decimal(
      c("O9A119", "O9A53", "S0000XA", "T3299", "P150", "P159",
        "Z9989", "Z950", "C7A098", "C7A8")), regexp = NA)

  expect_equal(
    as.vector(res),
    c("O9A.119", "O9A.53", "S00.00XA", "T32.99", "P15.0", "P15.9",
      "Z99.89", "Z95.0", "C7A.098", "C7A.8"))
  expect_true(is.decimal_diag(res))

  # is the icd10cm class preserved?
  expect_warning(
    res <- short_to_decimal(
      icd10cm(c("O9A119", "O9A53", "S0000XA", "T3299", "P150", "P159",
                "Z9989", "Z950", "C7A098", "C7A8"))), regexp = NA)

  expect_true(is.decimal_diag(res))
  expect_true(is.icd10cm(res))

})

test_that("icd10 short to decimal and back", {
  expect_identical(
    short_to_decimal(decimal_to_short("A00.0")),
    as.icd10("A00.0") %>% as.decimal_diag)
  expect_identical(
    decimal_to_short(short_to_decimal("A000")),
    as.icd10("A000") %>% as.short_diag)
  expect_identical(
    short_to_decimal.icd10(decimal_to_short.icd10("A00.0")),
    as.icd10("A00.0") %>% as.decimal_diag)
  expect_identical(
    decimal_to_short.icd10(short_to_decimal.icd10("A000")),
    as.icd10("A000") %>% as.short_diag)
})

test_that("icd9 short to major part, E codes", {
  expect_equal_no_icd(get_major.icd9(short_code = TRUE, "E000"), "E000")
  expect_equal_no_icd(get_major.icd9(short_code = TRUE, "E00"), "E000")
  expect_equal_no_icd(get_major.icd9(short_code = TRUE, "E0"), "E000")
  expect_equal_no_icd(get_major.icd9(short_code = TRUE, "E1"), "E001")
  expect_equal_no_icd(get_major.icd9(short_code = TRUE, "E001"), "E001")
  expect_equal_no_icd(get_major.icd9(short_code = TRUE, "E0123"), "E012")
  expect_equal_no_icd(get_major.icd9(short_code = TRUE, "E100"), "E100")
  expect_equal_no_icd(get_major.icd9(short_code = TRUE, "E1234"), "E123")

})

test_that("short to decimal before and after expansion of ICD-9 codes same", {
  icd9List <- icd9_map_ahrq #todo SUBSET OR EXTRA MAPPINGS?
  for (i in names(icd9List)) {
    expect_equal_no_icd(
      decimal_to_short.icd9(short_to_decimal.icd9(icd9List[[i]])),
      icd9_add_leading_zeroes(icd9List[[i]], short_code = TRUE),
      info = paste("step 1, iteration:", i)
    )

    expect_equal_no_icd(
      decimal_to_short(short_to_decimal(icd9List[[i]])),
      icd9_add_leading_zeroes(icd9List[[i]], short_code = TRUE),
      info = paste("step 2, iteration:", i)
    )
  }

  n <- 50
  set.seed(1441)
  rd9 <- paste(
    sprintf("%d", round(stats::runif(min = 1, max = 199, n = n))),
    sample(expand_minor.icd9("", is_e = FALSE), replace = TRUE, size = n)[-1],
    sep = "."
  )
  rd9pad <- icd9_add_leading_zeroes(rd9)
  expect_equal_no_icd(
    short_to_decimal.icd9(decimal_to_short.icd9(rd9)),
    rd9pad
  )

  # same with function to dispatch appropriately
  expect_equal_no_icd(
    short_to_decimal(decimal_to_short(rd9)),
    rd9pad
  )

  # test without decimal, too... starting with non-zero-spaced shorts
  set.seed(1441)
  rd2 <- as.character(round(stats::runif(min = 1, max = 999, n = n)))
  set.seed(1441)
  rd2pad <- sprintf("%03d", round(stats::runif(min = 1, max = 999, n = n)))
  expect_equal_no_icd(
    short_to_decimal.icd9(decimal_to_short.icd9(rd2)), rd2pad)
  expect_equal_no_icd(
    decimal_to_short.icd9(short_to_decimal.icd9(rd2)), rd2pad)
  expect_equal_no_icd(
    short_to_decimal.icd9(decimal_to_short.icd9(rd2pad)), rd2pad)
  expect_equal_no_icd(
    decimal_to_short.icd9("123."), "123")
})

test_that("show why padding zeroes before decimal conv without class matters", {
  expect_equal_no_icd(short_to_decimal(decimal_to_short("12.3")), "012.3")
})

test_that("short to decimal conversions also convert class", {
  expect_true(
    is.short_diag(
      decimal_to_short(
        as.decimal_diag("12.34")
      )
    )
  )
  expect_true(
    is.decimal_diag(
      short_to_decimal(
        as.short_diag("1234")
      )
    )
  )
  expect_true(
    is.short_diag(
      decimal_to_short.icd9(
        as.decimal_diag("12.34")
      )
    )
  )
  expect_true(
    is.decimal_diag(
      short_to_decimal.icd9(
        as.short_diag("1234")
      )
    )
  )
})

test_that("parts to decimal", {
  expect_equal(icd9PartsToDecimal(data.frame(mjr = "100", mnr = NA)), "100")
  expect_equal(icd9PartsToDecimal(data.frame(mjr = "100", mnr = "")), "100")
  expect_equal(icd9PartsToDecimal(data.frame(mjr = "100", mnr = "1")), "100.1")
  expect_equal(icd9MajMinToDecimal("100", NA), "100")
  expect_equal(icd9MajMinToDecimal("100", ""), "100")
  expect_equal(icd9MajMinToDecimal("100", "1"), "100.1")
})

test_that("parts to short invalid inputs", {
  dfempty <- list(mjr = character(), mnr = character())
  dfe2 <- list(mjr = "", mnr = "")
  expect_equal(icd9PartsToShort(dfempty), character())
  expect_equal(icd9PartsToShort(dfe2), NA_character_)
  expect_equal(icd9PartsToShort(list(mjr = NA, mnr = "trout")), NA_character_)
  expect_equal(icd9PartsToShort(list(mjr = NA, mnr = "23")), NA_character_)
  expect_equal(icd9PartsToShort(list(mjr = NA, mnr = "")), NA_character_)
  expect_equal(icd9PartsToShort(list(mjr = NA, mnr = NA)), NA_character_)
})

test_that("parts to valid short with empty or NA minor", {
  for (mn in c("", NA)) {
    # leading zeroes should default to true for codes <100 without minor part:
    # more consistent, because any code <100 with a minor must be zero padded to
    # avoid ambiguity.
    expect_equal(icd9PartsToShort(list(mjr = "100", mnr = mn)), "100")
    expect_equal(icd9PartsToShort(list(mjr = "10", mnr = mn)), "010")
    expect_equal(icd9PartsToShort(list(mjr = "010", mnr = mn)), "010")
    expect_equal(icd9PartsToShort(list(mjr = "001", mnr = mn)), "001")
    expect_equal(icd9PartsToShort(list(mjr = "01", mnr = mn)), "001")
    expect_equal(icd9PartsToShort(list(mjr = "1", mnr = mn)), "001")
  }
})

test_that("parts to valid simple numeric inputs", {
  expect_equal(
    icd9PartsToShort(list(mjr = "1", mnr = "23")), "00123")
  expect_equal(
    icd9PartsToShort(list(mjr = "01", mnr = "23", stringsAsFactors = TRUE)),
    "00123")
  expect_equal(
    icd9PartsToShort(list(mjr = "001", mnr = "23")), "00123")
  expect_equal(
    icd9PartsToShort(list(mjr = "10", mnr = "23", stringsAsFactors = TRUE)),
    "01023")
  expect_equal(
    icd9PartsToShort(list(mjr = "010", mnr = "23")), "01023")
  expect_equal(
    icd9PartsToShort(list(mjr = "100", mnr = "23", stringsAsFactors = TRUE)),
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
  expect_error(icd9MajMinToShort(c("123", "34", "56"), c("1", "20")),
               regexp = "length")
  # causes hang only when compiled with MinGW GCC 4.9 in Rtools 3.2 on 64 bit
  expect_error(icd9MajMinToShort(c("123", "34"), c("1", "20", "45")),
               regexp = "length")
})

test_that("Windows Rtools 3.2 hang test - also triggers bug #75", {
  expect_error(icd9MajMinToShort(c("123", "34"), c("1", "20", "45")),
               regexp = "equal")
  # see Rcpp issue #276.
})

test_that("icd9 parts to short form V and E input, mismatched lengths", {
  expect_equal(icd9MajMinToShort(10L, "20"), "01020")
  expect_equal(icd9MajMinToShort("V10", c("0", "1")), c("V100", "V101"))
  expect_equal(icd9MajMinToShort("V01", c("0", "1")), c("V010", "V011"))
})

# some functions are only called via C++ (at present), so the path through
# RcppExports is not tested. Also, compare slower functions for identical
# results as a regression test.
test_that("code routes through RcppExports.R and slower versions", {
  expect_equal(short_to_parts.icd9("1001"),
               data.frame(mjr = "100", mnr = "1", stringsAsFactors = FALSE))
  expect_equal(short_to_parts.icd9(c("99999", "0011")),
               data.frame(mjr = c("999", "001"), mnr = c("99", "1"),
                          stringsAsFactors = FALSE))
})


context("icd10 conversions")

test_that("decimal ICD-10 to parts", {
  expect_identical(
    decimal_to_parts("A00.0"),
    list(mjr = "A00", mnr = "0")
  )
  expect_identical(
    decimal_to_parts("C7A.020"),
    list(mjr = "C7A", mnr = "020")
  )
  expect_identical(
    decimal_to_parts(icd10cm("E89.89")),
    list(mjr = "E89", mnr = "89")
  )
})

test_that("icd10 short to parts", {
  expect_equal(short_to_parts("A0101"),
               data.frame(mjr = "A01", mnr = "01", stringsAsFactors = FALSE))
  # for V and E codes, we can't just assume ICD-10 will work with the ICD-9
  # function:
  expect_equal(short_to_parts(as.icd10cm("E8989")),
               data.frame(mjr = "E89", mnr = "89", stringsAsFactors = FALSE))
})
