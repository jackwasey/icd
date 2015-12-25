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

x <- icd9::icd9_map_quan_elix

test_that("constructing a comorbidity map works", {
  expect_equal(icd_comorbidity_map(x), x)
  expect_equivalent(as.list(x), x)
  expect_equivalent(icd_comorbidity_map(as.list(x)), x)
})

test_that("constructing a comorbidity map with unnamed list, etc. fails", {
  expect_error(icd_comorbidity_map(unname(unclass(x))))
  # and data frames should definitely fail
  expect_error(icd_comorbidity_map(icd9::vermont_dx))
  expect_error(icd_comorbidity_map(icd9::uranium_pathology))
})

test_that("subsetting a comorbidity map gives the right class", {

  wonky_map <- icd_comorbidity_map(icd_short_code(icd9cm(list(a = icd9cm("100"), b = icd9("V22")))))

  expect_is(wonky_map, "icd_comorbidity_map")
  expect_is(wonky_map, "icd_short_code")
  expect_is(wonky_map, "icd9cm")
  expect_is(wonky_map, "icd9")

  expect_true(!inherits(wonky_map[[1]], "icd_comorbidity_map"))
  expect_is(wonky_map[[1]], "icd_short_code")
  expect_is(wonky_map[[1]], "icd9cm")
  expect_is(wonky_map[[1]], "icd9")
  expect_is(wonky_map[[1]], "character")

  expect_true(!inherits(wonky_map[[2]], "icd_comorbidity_map"))
  expect_is(wonky_map[[2]], "icd_short_code")
  expect_is(wonky_map[[2]], "icd9cm")
  expect_is(wonky_map[[2]], "icd9")
  expect_is(wonky_map[[2]], "character")
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
  expect_equal(unclass(icd9::vermont_dx[1, "DX1"]), "27801")
  expect_is(icd9::vermont_dx[1, "DX1"], c("icd9cm", "icd9", "character"))
  expect_equal(unclass(icd9::vermont_dx[[1, 6]]), "27801")
  expect_is(icd9::vermont_dx[[1, "DX1"]], c("icd9cm", "icd9", "character"))
  # columns
  expect_is(icd9::vermont_dx[6], c("icd9cm", "icd9", "data.frame")) # not necessarily wide anymore...
  expect_is(icd9::vermont_dx[[6]], c("icd9cm", "icd9", "character"))
})

test_that("data frame subsetting doesn't incorrectly set class on columns", {
  expect_is(mixInvalidPts[c(T,T,T), "visitId"], "numeric")
  expect_false(inherits(mixInvalidPts[c(T,T,T), "visitId"], "icd9"))
  expect_equal(sapply(mixInvalidPts[c(T,T,T),], class),
               structure(list(visitId = "numeric", icd9 = c("icd9", "character"
               ), poa = "factor"), .Names = c("visitId", "icd9", "poa"))
  )
})

test_that("subset with double bracket doesn't override the underlying class", {
  x <- icd9(list(my_codes = c("V10.1", "441.1")))
  expect_false(inherits(x[[1]], "list"))
  expect_false(inherits(x[[1]][2], "list"))

  y <- icd10(list(thine = icd10cm(c("A01", "B0234"))))
  expect_true(inherits(y[1], "list"))

  expect_false(inherits(y[[1]], "list"))
  expect_false(inherits(y[[1]][1], "list"))
})
