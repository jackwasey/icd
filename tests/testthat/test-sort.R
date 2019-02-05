context("sorting")

test_that("sorting preserves order of names", {
  a <- c("Bad disease" = "100", "Another bad disease" = "200")
  b <- sort_icd.icd9(a)
  expect_identical(a[["Bad disease"]], b[["Bad disease"]])
  expect_identical(a[["Another bad disease"]], b[["Another bad disease"]])
})

test_that("sorting of icd9 object", {
  j <- icd9(c("Bad disease" = "500", "Another bad disease" = "400"))
  k <- sort_icd(j)
  expect_identical(j[2], k[1])
  expect_identical(j[1], k[2])
})

test_that("sorting of icd10 object", {
  a <- c("Bad disease" = "I119", "Another bad disease" = "I110")
  j <- icd10cm(a)
  k <- sort_icd(j)
  expect_identical(j[2], k[1])
  expect_identical(j[1], k[2])
  expect_identical(sort_icd(a), unclass(k))
})

test_that("warn if NA when ordering ICD-9 codes", {
  expect_warning(order.icd9(c("a", NA)))
  expect_identical(
    expect_warning(order.icd9(NA)), character())
})

test_that("order some icd-9 codes", {
  expect_equal(
    order.icd9(c("1002", "1001")),
    c(2L, 1L)
    )
})
