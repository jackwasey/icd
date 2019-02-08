context("guesss ICD type and version")

test_that("tricky ICD-10 codes", {
  expect_equal(guess_version("C7B00"), "icd10")
  expect_equal(guess_version("C7A024"), "icd10")
})

test_that("default guess of short code if major - ICD-9", {
  # if major is given, we can't tell if it is short or long. Give warning and
  # pick short.
  expect_true(guess_short("101"))
  expect_true(guess_short(c("101", "441")))
})

test_that("guess if major and decimal- ICD-9", {
  expect_false(guess_short(c("101", "441.0")))
})

test_that("default guess of short code if major - ICD-10", {
  expect_true(guess_short("A01"))
  expect_true(guess_short(c("A01", "R20")))
})
test_that("guess if major and decimal - ICD-10", {
  expect_false(guess_short(c("A01", "R20.1")))
})

test_that("short_code is respected even if code is opposite", {
  expect_true(guess_short("100.20", short_code = TRUE))
  expect_true(guess_short("A00.01", short_code = TRUE))

  expect_false(guess_short("10020", short_code = FALSE))
  expect_false(guess_short("A0001", short_code = FALSE))
})

test_that("guess type from a data frame with no class set on column", {
  pts <- generate_random_pts(5)
  expect_true(guess_short(pts))

  no_pts <- structure(
    list(visit_id = integer(0),
         code = character(0),
         poa = structure(integer(0), .Label = character(0), class = "factor")),
    .Names = c("visit_id", "code", "poa"),
    row.names = integer(0), class = "data.frame")

  # no error?
  guess_short(no_pts)

})

test_that("guess on a zero length character vector works", {
  # we always give a true or false response even if we have no data
  expect_logical(guess_short(character(0)), len = 1L)
})

test_that("lists of ICD-10 decimal codes are identified correctly as decimal", {
  expect_false(guess_short(icd10_each_quan_elix_cmb))
  expect_false(guess_short(unclass(icd10_each_quan_elix_cmb)))

  expect_false(guess_short(icd10_each_ahrq_cmb))
  expect_false(guess_short(unclass(icd10_each_ahrq_cmb)))
})

test_that("guessing type of NA values defaults to short, and doesn't error", {
  expect_true(guess_short(icd9(NA)))
  expect_true(guess_short(icd9cm(NA)))
  expect_true(guess_short(icd10(NA)))
  expect_true(guess_short(icd10cm(NA)))
  expect_true(guess_short(icd9(c(NA, NA))))
  expect_true(guess_short(icd9cm(c(NA, NA))))
  expect_true(guess_short(icd10(c(NA, NA))))
  expect_true(guess_short(icd10cm(c(NA, NA))))

  expect_error(guess_short(icd9(c(NA, TRUE))))
  expect_error(guess_short(icd9cm(c(NA, TRUE))))
  expect_error(guess_short(icd10(c(NA, TRUE))))
  expect_error(guess_short(icd10cm(c(NA, TRUE))))
})

test_that("guess and update version", {
  res <- guess_version_update("1234", TRUE)
  expect_true(is.icd9(res))
  expect_false(is.icd10(res))

  res <- guess_version_update("12.34", FALSE)
  expect_true(is.icd9(res))
  expect_false(is.icd10(res))


  res <- guess_version_update("A010", TRUE)
  expect_true(is.icd10(res))
  expect_false(is.icd9(res))

  res <- guess_version_update("A01.0", FALSE)
  expect_true(is.icd10(res))
  expect_false(is.icd9(res))
})

test_that("guess uranium codes are long", {
  expect_false(guess_short(icd.data::uranium_pathology$icd10))
})

test_that("we should allow integer ids which get passed to guessing code", {
  pts <- data.frame(id = 1:20, weirdname = rep("0932", 20))
  expect_no_error(icd9_comorbid_ahrq(pts))
})

test_that("get icd dx name from nhds", {
  skip_if_not_installed("nhds")
  expect_true(all(grepl("dx", get_icd_name(nhds::nhds2010))))
  expect_true(all(grepl("pc", get_icd_pc_name(nhds::nhds2010))))
})
