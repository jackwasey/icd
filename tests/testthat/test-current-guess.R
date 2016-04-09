context("guesss ICD type and version")

test_that("tricky ICD-10 codes", {
  expect_equal(icd_guess_version("C7B00"), "icd10")
  expect_equal(icd_guess_version("C7A024"), "icd10")
})

test_that("default guess of short code if major - ICD-9", {
  # if major is given, we can't tell if it is short or long. Give warning and pick short.
  expect_true(icd_guess_short("101"))
  expect_true(icd_guess_short(c("101", "441")))
})

test_that("guess if major and decimal- ICD-9", {
  expect_false(icd_guess_short(c("101", "441.0")))
})

test_that("default guess of short code if major - ICD-10", {
  expect_true(icd_guess_short("A01"))
  expect_true(icd_guess_short(c("A01", "R20")))
})
test_that("guess if major and decimal - ICD-10", {
  expect_false(icd_guess_short(c("A01", "R20.1")))
})

test_that("short_code is respected even if code is opposite", {
  expect_true(icd_guess_short("100.20", short_code = TRUE))
  expect_true(icd_guess_short("A00.01", short_code = TRUE))

  expect_false(icd_guess_short("10020", short_code = FALSE))
  expect_false(icd_guess_short("A0001", short_code = FALSE))
})

test_that("guess type from a data frame with no class set on column", {
pts <- generate_random_pts(5)
class(pts$code) <- "character"

expect_true(icd_guess_short(pts))

no_pts <- structure(list(visit_id = integer(0),
                         code = character(0),
                         poa = structure(integer(0),
                                         .Label = character(0), class = "factor")),
                    .Names = c("visit_id", "code", "poa"), row.names = integer(0), class = "data.frame")

# no error?
icd_guess_short(no_pts)

})

test_that("guess on a zero length character vector works", {
 expect_true(is.logical(icd_guess_short(character(0))))
})

test_that("lists of ICD-10 decimal codes are identified correctly as decimal", {
  expect_false(icd_guess_short(icd10_each_quan_elix_cmb))
  expect_false(icd_guess_short(unclass(icd10_each_quan_elix_cmb)))

  expect_false(icd_guess_short(icd10_each_ahrq_cmb))
  expect_false(icd_guess_short(unclass(icd10_each_ahrq_cmb)))
})

test_that("guessing type of NA values defaults to short, and doesn't error", {
  expect_true(icd_guess_short(icd9(NA)))
  expect_true(icd_guess_short(icd9cm(NA)))
  expect_true(icd_guess_short(icd10(NA)))
  expect_true(icd_guess_short(icd10cm(NA)))
  expect_true(icd_guess_short(icd9(c(NA, NA))))
  expect_true(icd_guess_short(icd9cm(c(NA, NA))))
  expect_true(icd_guess_short(icd10(c(NA, NA))))
  expect_true(icd_guess_short(icd10cm(c(NA, NA))))

  expect_error(icd_guess_short(icd9(c(NA, TRUE))))
  expect_error(icd_guess_short(icd9cm(c(NA, TRUE))))
  expect_error(icd_guess_short(icd10(c(NA, TRUE))))
  expect_error(icd_guess_short(icd10cm(c(NA, TRUE))))
})
