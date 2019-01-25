context("ICD-10 ranges")

test_that("very bad input data fails completely", {
  expect_error(expand_range(data.frame(a = 1, b = "b"), factor(1, 2, 3)))
})

test_that("top level ICD-10 ranges of single code are expanded to real codes", {
  expect_true(all(is_defined.icd10cm(expand_range(icd10cm("J11"), icd10cm("J11")))))
})

test_that("simple ranges of real values works", {
  expect_equal_no_icd(expand_range.icd10cm("A00", "A001"),
                      c("A00", "A000", "A001"))
  expect_equal_no_icd(expand_range.icd10cm("A00", "A009", short_code = TRUE),
                      c("A00", "A000", "A001", "A009"))
  # same with S3 dispatch
  expect_equal_no_icd(expand_range("A00", "A001"),
                      c("A00", "A000", "A001"))
  expect_equal_no_icd(expand_range("A00", "A009", short_code = TRUE),
                      c("A00", "A000", "A001", "A009"))
})

test_that("range style used by Quan is accepted", {
  quan_ranges <- list(
    c("M310", "M313"),
    c("M32x", "M35x"),
    c("D65", "D68x"),
    c("M45x", "M45x"),
    c("M461", "M461")
  )
  for (pair in quan_ranges) {
    # start with a low bar!
    expect_error(
      expand_range.icd10cm(pair[1], pair[2], short_code = TRUE), NA,
      info = paste(pair, collapse = "-")
    )
  }

})

################################################################################
# ranges for real children
################################################################################

context("ICD-10 ranges - defined children")

test_that("completely invalid input fails", {
  expect_error(children_defined.icd10cm(data.frame(a = 1, b = "b"), short_code = TRUE))
})

test_that("single value gives correct range", {
  expect_equal(res <- children_defined.icd10cm("A00", short_code = TRUE),
               structure(c("A00", "A000", "A001", "A009"),
                         class = c("icd10cm", "icd10", "character"),
                         icd_short_diag = TRUE)
  )
  expect_true(is.icd10cm(res))
  expect_true(is.icd10(res))
  expect_true(is.short_diag(res))
})

test_that("icd10 range major expansions", {
  expect_identical(expand_range_major.icd10cm("A00", "A01"),
                   structure(c("A00", "A01"), class = c("icd10cm", "icd10", "character"))
  )
  expect_identical(expand_range_major.icd10cm("A00", "A01"),
                   expand_range_major(icd10cm("A00"), "A01"))

  expect_equal(expand_range_major.icd10cm("A99", "B00"), as.icd10cm(c("A99", "B00")))
  expect_equal(expand_range_major.icd10cm("A96", "A96"), as.icd10cm("A96"))

  expect_identical(expand_range_major.icd10cm("a00", "a01"),
                   structure(c("A00", "A01"), class = c("icd10cm", "icd10", "character"))
  )
  expect_identical(expand_range_major.icd10cm("a00", "a01"),
                   expand_range_major(icd10cm("a00"), "a01"))

  expect_equal(expand_range_major.icd10cm("a99", "b00"), as.icd10cm(c("A99", "B00")))
  expect_equal(expand_range_major.icd10cm("a96", "a96"), as.icd10cm("A96"))

  expect_equal_no_icd(expand_range_major("a99", "b00"), c("A99", "B00"))
  expect_equal_no_icd(expand_range_major("a96", "a96"), "A96")
})

test_that("icd10cm range errors", {
  expect_error(expand_range_major.icd10cm("A99", "A96"), "after")
  expect_error(expand_range_major.icd10cm("B00", "A96"), "after")
  expect_error(expand_range_major.icd10cm("A10", "A13"), "not found") # neither exist
  expect_error(expand_range_major.icd10cm("A11", "A99"), "not found") # start missing
  expect_error(expand_range_major.icd10cm("A00", "A12"), "not found") # end missing

  expect_error(expand_range_major.icd10cm("dsa", "A99"), "is not") # start wrong
  expect_error(expand_range_major.icd10cm("A00", "zds"), "is not") # end wrong
  expect_error(expand_range_major.icd10cm("399annc", "611"), "is not") # both wrong

})
