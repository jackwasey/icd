context("sorting, ordering ICD-9")

test_that("sorting preserves order of names", {
  a <- c("Bad disease" = "100", "Another bad disease" = "200")
  b <- sort.icd9(a)
  expect_identical(a[["Bad disease"]], b[["Bad disease"]])
  expect_identical(a[["Another bad disease"]], b[["Another bad disease"]])
})

test_that("sorting of icd9 object", {
  j <- icd9(c("Bad disease" = "500", "Another bad disease" = "400"))
  k <- sort(j)
  expect_identical(j[2], k[1])
  expect_identical(j[1], k[2])
})

test_that("order some icd-9 codes", {
  expect_equal(
    order.icd9(c("1002", "1001")),
    c(2L, 1L)
  )
})


test_that("NAs are ordered at end", {
  expect_identical(
    order.icd9(c(NA_character_, "100")),
    c(2L, 1L)
  )
})

context("sorting, ordering icd10cm")

test_that("basic compare works", {
  expect_false(icd9_compare_rcpp(NA_character_, NA_character_))
  expect_false(icd9_compare_rcpp("010", "010"))
  expect_false(icd9_compare_rcpp(NA_character_, "010"))
  expect_true(icd9_compare_rcpp("010", NA_character_))
})

test_that("sorting of icd10 object", {
  a <- c("Bad disease" = "I119", "Another bad disease" = "I110")
  j <- icd10cm(a)
  k <- sort(j)
  expect_identical(j[2], k[1])
  expect_identical(j[1], k[2])
  expect_identical(icd:::sort.icd10(a), unclass(k))
  expect_identical(icd:::sort.icd10cm(a), unclass(k))
})

test_that("compare icd10", {
  expect_false(icd10cm_compare_rcpp("A100", "A100"))
  expect_true(icd10cm_compare_rcpp("A100", "A200"))
  expect_false(icd10cm_compare_rcpp("A190", "A100"))
  expect_true(icd10cm_compare_rcpp("A100", NA_character_))
  expect_false(icd10cm_compare_rcpp(NA_character_, "A100"))
  # std
  expect_false(icd10cm_compare_std("A100", "A100"))
  expect_true(icd10cm_compare_std("A100", "A200"))
  expect_false(icd10cm_compare_std("A190", "A100"))
})

test_that("sort icd10", {
  i10 <- as.icd10cm(c("Z00", "A99", "J4C"))
  expect_equal(
    order.icd10cm(i10),
    c(2, 3, 1)
  )
  expect_equal(
    sort(i10),
    as.icd10cm(c("A99", "J4C", "Z00"))
  )
  expect_equal(sort(as.icd10cm("Z04")), as.icd10cm("Z04"))
})

test_that("duplicates are accounted for", {
  expect_identical(
    order.icd10cm(c("A10", "B20", "A10")),
    c(1L, 3L, 2L)
  )
})
