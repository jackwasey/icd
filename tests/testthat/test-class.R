context("test class functions")

test_that("classes are created and identified correctly", {
  expect_true(is.icd9(icd9("V10")))
  expect_true(is.icd9cm(icd9cm("V10")))
  expect_true(is.icd10(icd10("V10")))
  expect_true(is.icd10cm(icd10cm("V10")))
  expect_true(is.icd10who(icd10who("V10")))
})

test_that("subclasses still have parent class", {
  expect_true(is.icd9(icd9cm("V10")))
  expect_true(is.icd10(icd10cm("V10")))
  expect_true(is.icd10(icd10who("V10")))
})

test_that("setting conflicting icd data class gives error", {
  expect_error(icd10(icd9("V10")))
  expect_error(icd10cm(icd9("V10")))
  expect_error(icd10who(icd9("V10")))
  expect_error(icd10(icd9cm("V10")))
  expect_error(icd10cm(icd9cm("V10")))
  expect_error(icd10who(icd9cm("V10")))
  expect_error(icd9(icd10("V10")))
  expect_error(icd9(icd10cm("V10")))
  expect_error(icd9(icd10who("V10")))
  expect_error(icd9cm(icd10("V10")))
  expect_error(icd9cm(icd10cm("V10")))
  expect_error(icd9cm(icd10who("V10")))
})

x <- icd9::quanElixComorbid

test_that("constructing a comorbidity map works", {
  expect_equal(icd_map(x), x)
  expect_equivalent(as.list(x), x)
  expect_equivalent(icd_map(as.list(x)), x)
})

test_that("constructing a comorbidity map with unnamed list, etc. fails", {
  expect_error(icd_map(unname(unclass(x))))
  # and data frames should definitely fail
  expect_error(icd_map(icd9::vermont_dx))
  expect_error(icd_map(icd9::uranium_pathology))
})

test_that("constructing wide data works", {
  expect_equal(icd_wide_data(icd9::vermont_dx), icd9::vermont_dx)
  expect_equivalent(icd_wide_data(icd9::vermont_dx), icd9::vermont_dx)
  expect_equivalent(icd_wide_data(as.data.frame(icd9::vermont_dx)), icd9::vermont_dx)
})

test_that("constructing long data works", {
  expect_equal(icd_long_data(icd9::uranium_pathology), icd9::uranium_pathology)
  expect_equivalent(icd_long_data(icd9::uranium_pathology), icd9::uranium_pathology)
  expect_equivalent(icd_long_data(as.data.frame(icd9::uranium_pathology)), icd9::uranium_pathology)
})

test_that("constructing wide or long format for non-data frame gives error", {
  expect_error(icd_wide_data(e))
  expect_error(icd_wide_data(letters))
})

test_that("subsetting data frame works", {
  expect_equal(unclass(icd9::vermont_dx[1, 6]), "27801")
  expect_equal(icd9::vermont_dx[1, 6], icd9("27801"))
  expect_equal(unclass(icd9::vermont_dx[[1, 6]]), "27801")
  expect_equal(icd9::vermont_dx[[1, 6]], icd9("27801"))
  expect_is(icd9::vermont_dx[1, 6], "icd9")
  expect_is(icd9::vermont_dx[[1, 6]], "icd9")
  # columns
  expect_is(icd9::vermont_dx[6], "icd9")
  expect_is(icd9::vermont_dx[[6]], "icd9")
})

test_that("data frame subsetting doesn't incorrectly set class on columns", {
  expect_is(mixInvalidPts[c(T,T,T), "visitId"], "numeric")
  expect_false(inherits(mixInvalidPts[c(T,T,T), "visitId"], "icd9"))
  mixInvalidPts[c(T,T,T),]
})
