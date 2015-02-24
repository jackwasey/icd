context("compare ordered long to wide methods")

pts <- randomOrderedPatients(1000, 10)

test_that("ordered and unordered methods on ordered data are identical", {
  expect_identical(icd9LongToWideMatrixByMap(pts), icd9LongOrderedToWideMatrix(pts))
  expect_identical(icd9LongToWideMatrix(pts), icd9LongOrderedToWideMatrix(pts))
})
