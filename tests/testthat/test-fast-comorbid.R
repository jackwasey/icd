context("quick comorbidity test")
twoPts <- data.frame(visitId = c("v01", "v01", "v02", "v02"),
               icd9 = c("040", "000", "100", "000"), stringsAsFactors = FALSE)
twoMap <- data.frame("malady" = c("100", "2000"), "ailment" = c("003", "040"), stringsAsFactors = FALSE)
twoPtsFac <- data.frame(visitId = c("v01", "v01", "v02", "v02"), icd9 = c("040", "000", "100", "000"), stringsAsFactors = TRUE)
twoMapFac <- data.frame("malady" = c("100", "2000"), "ailment" = c("003", "040"), stringsAsFactors = TRUE)

test_that("comorbid quick test", {
  testres <- icd9Comorbid(twoPts, twoMap)
  trueres <- data.frame("visitId" = c("v01", "v02"),
                        "malady" = c(FALSE, TRUE),
                        "ailment" = c(TRUE, FALSE),
                        stringsAsFactors = FALSE)
  expect_equal(testres, trueres)

  testresfac <- icd9Comorbid(twoPtsFac, twoMapFac)
  trueresfac <- data.frame("visitId" = c("v01", "v02"),
                        "malady" = c(FALSE, TRUE),
                        "ailment" = c(TRUE, FALSE),
                        stringsAsFactors = TRUE)
  expect_equal(testresfac, trueresfac)

})

test_that("control params don't affect result of comorbid calc", {
  pts <- randomPatients(101, 13)
  upts <- length(unique(pts$visitId))
  expect_identical(
    icd9ComorbidShort(pts, ahrqComorbid, threads = 1, chunkSize=32),
    icd9ComorbidShort(pts, ahrqComorbid, threads = 3, chunkSize=32)
  )
  expect_identical(
    icd9ComorbidShort(pts, ahrqComorbid, threads = 3, chunkSize=1),
    icd9ComorbidShort(pts, ahrqComorbid, threads = 3, chunkSize=32)
  )
  expect_identical(
    icd9ComorbidShort(pts, ahrqComorbid, threads = 4, chunkSize=upts-1),
    icd9ComorbidShort(pts, ahrqComorbid, threads = 4, chunkSize=upts)
  )
  expect_identical(
    icd9ComorbidShort(pts, ahrqComorbid, threads = 4, chunkSize=upts-1),
    icd9ComorbidShort(pts, ahrqComorbid, threads = 4, chunkSize=upts+1)
  )
  expect_identical(
    icd9ComorbidShort(pts, ahrqComorbid, threads = 4, chunkSize=upts+1),
    icd9ComorbidShort(pts, ahrqComorbid, threads = 4, chunkSize=upts)
  )
  expect_identical(
    icd9ComorbidShort(pts, ahrqComorbid, threads = 3, chunkSize=upts-2, ompChunkSize = 1),
    icd9ComorbidShort(pts, ahrqComorbid, threads = 3, chunkSize=upts+2, ompChunkSize = 1)
  )
  expect_identical(
    icd9ComorbidShort(pts, ahrqComorbid, threads = 3, chunkSize=upts-2, ompChunkSize = 11),
    icd9ComorbidShort(pts, ahrqComorbid, threads = 3, chunkSize=upts+2, ompChunkSize = 11)
  )
  expect_identical(
    icd9ComorbidShort(pts, ahrqComorbid, threads = 3, chunkSize=upts, ompChunkSize = 1),
    icd9ComorbidShort(pts, ahrqComorbid, threads = 3, chunkSize=upts, ompChunkSize = 11)
  )
  expect_identical(
    icd9ComorbidShort(pts, ahrqComorbid),
    icd9ComorbidShort(pts, ahrqComorbid, threads = 3, chunkSize=3, ompChunkSize = 5) # primes < unique visits
  )
})
