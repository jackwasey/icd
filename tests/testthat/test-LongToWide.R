context("compare ordered long to wide methods")

pts <- randomOrderedPatients(5000, 13)

test_that("ordered and unordered methods on ordered data are identical", {
  #expect_identical(icd9LongToWideMatrixByMap(pts), icd9LongOrderedToWideMatrix(pts))
  #agg <- icd9LongToWideMatrixAggregate(pts)
  #ord <- icd9LongToWideMatrixNoAggregate(pts)
  agg <- icd9LongToWide(pts, aggregate=TRUE)
  ord <- icd9LongToWide(pts, aggregate=FALSE)
  expect_identical(ord, agg)
  expect_true(all(rownames(ord) %in% pts$visitId))
  expect_true(all(rownames(agg) %in% pts$visitId))
  expect_true(all(pts$visitId %in% rownames(ord)))
  expect_true(all(pts$visitId %in% rownames(agg)))
})
