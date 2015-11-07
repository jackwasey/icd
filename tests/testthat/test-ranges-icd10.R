context("test icd10 ranges")

test_that("completely invalid input fails", {

  expect_error(icd10ChildrenShort(data.frame(a = 1, b = "b")))

})

test_that("certainly invalid codes return empty vectors", {
  expect_equal(icd10ChildrenShort("!"), character(0))
  expect_equal(icd10ChildrenShort("H"), character(0))
  expect_equal(icd10ChildrenShort("H7"), character(0))
  # never longer than 7 digits no matter what
  expect_equal(icd10ChildrenShort("12345678"), character(0))
  expect_equal(icd10ChildrenShort("A2345678"), character(0))
})

test_that("NA values are handled gracefully", {
  expect_equal(icd10ChildrenShort(NA_character_), NA_character_)
  expect_equal(icd10ChildrenShort(c(NA_character_, NA_character_)),
                                  c(NA_character_, NA_character_))
})

test_that("exact children created for single values", {
  expect_equal(icd10ChildrenShort("A234567"), "A234567")

  expect_equal(
    icd10ChildrenShort("123456"),
    c("123456", "1234560", "1234561", "1234562", "1234563", "1234564",
      "1234565", "1234569", "123456A", "123456B", "123456C", "123456D",
      "123456E", "123456F", "123456G", "123456H", "123456J", "123456K",
      "123456M", "123456N", "123456P", "123456Q", "123456R", "123456S"))
})

test_that("right number of children are made for higher level codes", {
  #sapply(minor_chars, length)  [1] 23 14 11 23 (all +1)
  expect_equal(length(icd10ChildrenShort("Z99")), 24 * 15 * 12 * 24)
  expect_equal(length(icd10ChildrenShort("Z991")), 15 * 12 * 24)
  expect_equal(length(icd10ChildrenShort("Z9921")), 12 * 24)
  expect_equal(length(icd10ChildrenShort("Z99321")), 24)
})

test_that("multiple inputs return ordered results", {
  expect_equal(
    icd10ChildrenShort(c("A00", "Z99")),
    c(icd10ChildrenShort("A00"), icd10ChildrenShort("Z99"))
    )

    expect_equal(
    icd10ChildrenShort(c("Z99", "A00")),
    c(icd10ChildrenShort("A00"), icd10ChildrenShort("Z99"))
  )
})
