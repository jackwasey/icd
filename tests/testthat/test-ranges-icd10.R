################################################################################
# ranges
################################################################################

context("test icd10 ranges")

test_that("very bad input data fails completely", {
  expect_error(icd10ExpandRangeRealShort(data.frame(a = 1, b = "b"), factor(1,2,3)))
})

test_that("simple ranges of real values works", {
  expect_equal(icd10ExpandRangeRealShort("A00", "A001"),
               c("A00", "A000", "A001"))
  expect_equal(icd10ExpandRangeRealShort("A00", "A009"),
               c("A00", "A000", "A001", "A009"))
})

test_that("range style used by Quan is accepted", {
  # quan_ranges <- list(
  #   c("M31.0", "M31.3"),
  #   c("M32.x", "M35.x"),
  #   c("D65", "D68.x"),
  #   c("M45.x", "M45.x"),
  #   c("M46.1", "M46.1")
  # )
  quan_ranges <- list(
    c("M310", "M313"),
    c("M32x", "M35x"),
    c("D65", "D68x"),
    c("M45x", "M45x"),
    c("M461", "M461")
  )
  for (pair in quan_ranges) {
    # start with a low bar!
      expect_that(
        icd10ExpandRangeRealShort(pair[1], pair[2]),
        testthat::not(throws_error()),
        info = paste(pair, collapse = "-")
        )

    # the input values should be in the output? NOT necessarily true for .x numbers
    # expect_true(
    #   all(pair %in% icd10ExpandRangeRealShort(pair[1], pair[2])),
    #   info = paste(pair, collapse = "-")
    # )

  }

})

################################################################################
# ranges for real children
################################################################################

context("test icd10 ranges - children real")

test_that("completely invalid input fails", {
  expect_error(icd10ChildrenRealShort(data.frame(a = 1, b = "b")))
})

test_that("single value gives correct range", {
  expect_equal(icd10ChildrenRealShort("A00"), c("A00", "A000", "A001", "A009"))
})

################################################################################
# ranges for (some) possible children - low priority
################################################################################

context("test icd10 ranges - children possible")

test_that("completely invalid input fails", {

  expect_error(icd10ChildrenPossibleShort(data.frame(a = 1, b = "b")))

})

test_that("single values give all real children", {
  expect_equal(icd10ChildrenRealShort("A00"), c("A00", "A000", "A001", "A009"))
})

test_that("certainly invalid codes return empty vectors", {
  expect_equal(icd10ChildrenPossibleShort("!"), character(0))
  expect_equal(icd10ChildrenPossibleShort("H"), character(0))
  expect_equal(icd10ChildrenPossibleShort("H7"), character(0))
  # never longer than 7 digits no matter what
  expect_equal(icd10ChildrenPossibleShort("12345678"), character(0))
  expect_equal(icd10ChildrenPossibleShort("A2345678"), character(0))
})

test_that("NA values are handled gracefully", {
  expect_equal(icd10ChildrenPossibleShort(NA_character_), NA_character_)
  expect_equal(icd10ChildrenPossibleShort(c(NA_character_, NA_character_)),
               c(NA_character_, NA_character_))
})

test_that("exact children created for single values", {
  expect_equal(icd10ChildrenPossibleShort("A234567"), "A234567")

  expect_equal(
    icd10ChildrenPossibleShort("123456"),
    c("123456", "1234560", "1234561", "1234562", "1234563", "1234564",
      "1234565", "1234569", "123456A", "123456B", "123456C", "123456D",
      "123456E", "123456F", "123456G", "123456H", "123456J", "123456K",
      "123456M", "123456N", "123456P", "123456Q", "123456R", "123456S"))
})

test_that("right number of children are made for higher level codes", {
  #sapply(minor_chars, length)  [1] 23 14 11 23 (all +1)
  expect_equal(length(icd10ChildrenPossibleShort("Z99")), 24 * 15 * 12 * 24)
  expect_equal(length(icd10ChildrenPossibleShort("Z991")), 15 * 12 * 24)
  expect_equal(length(icd10ChildrenPossibleShort("Z9921")), 12 * 24)
  expect_equal(length(icd10ChildrenPossibleShort("Z99321")), 24)
})

test_that("multiple inputs return ordered results", {
  expect_equal(
    icd10ChildrenPossibleShort(c("A00", "Z99")),
    c(icd10ChildrenPossibleShort("A00"), icd10ChildrenPossibleShort("Z99"))
  )

  expect_equal(
    icd10ChildrenPossibleShort(c("Z99", "A00")),
    c(icd10ChildrenPossibleShort("A00"), icd10ChildrenPossibleShort("Z99"))
  )
})
