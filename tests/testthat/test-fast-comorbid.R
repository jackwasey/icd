context("quick comorbidity test")
twoPts <- data.frame(visitId = c("v01", "v01", "v02", "v02"),
               icd9 = c("040", "000", "100", "000"), stringsAsFactors = FALSE)
twoMap <- data.frame("malady" = c("100", "2000"), "ailment" = c("003", "040"), stringsAsFactors = FALSE)
twoPtsFac <- data.frame(visitId = c("v01", "v01", "v02", "v02"), icd9 = c("040", "000", "100", "000"), stringsAsFactors = TRUE)
twoMapFac <- data.frame("malady" = c("100", "2000"), "ailment" = c("003", "040"), stringsAsFactors = TRUE)

test_that("comorbid quick test", {
  testres <- icd9Comorbid(twoPts, twoMap, return.df = TRUE)
  trueres <- data.frame("visitId" = c("v01", "v02"),
                        "malady" = c(FALSE, TRUE),
                        "ailment" = c(TRUE, FALSE),
                        stringsAsFactors = FALSE)
  expect_equal(testres, trueres)

  testmat <- icd9Comorbid(twoPts, twoMap, return.df = FALSE)
  truemat <- matrix(c(FALSE, TRUE, TRUE, FALSE), nrow=2,
                    dimnames = list(c("v01", "v02"), c("malady", "ailment")))
  expect_equal(testmat, truemat)

  testresfac <- icd9Comorbid(twoPtsFac, twoMapFac, return.df = TRUE)
  trueresfac <- data.frame("visitId" = c("v01", "v02"),
                        "malady" = c(FALSE, TRUE),
                        "ailment" = c(TRUE, FALSE),
                        stringsAsFactors = TRUE)
  expect_equal(testresfac, trueresfac)
  expect_equal(icd9Comorbid(twoPtsFac, twoMapFac), truemat)

})

test_that("control params don't affect result of comorbid calc", {
  pts <- randomPatients(101, 13)
  pts$visitId <- asCharacterNoWarn(pts$visitId)
  pts$icd9 <- as.factor(pts$icd9)
  upts <- length(unique(pts$visitId))
  ac <-  lapply(ahrqComorbid, function(x) {
    f <- factor(x, levels(pts[["icd9"]]))
    f[!is.na(f)]
  })
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, threads = 1, chunkSize=32),
    icd9ComorbidShortCpp(pts, ac, threads = 3, chunkSize=32)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, threads = 2, chunkSize=32),
    icd9ComorbidShortCpp(pts, ac, threads = 5, chunkSize=32)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, threads = 3, chunkSize=1),
    icd9ComorbidShortCpp(pts, ac, threads = 3, chunkSize=32)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, threads = 4, chunkSize=upts-1),
    icd9ComorbidShortCpp(pts, ac, threads = 4, chunkSize=upts)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, threads = 4, chunkSize=upts-1),
    icd9ComorbidShortCpp(pts, ac, threads = 4, chunkSize=upts+1)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, threads = 4, chunkSize=upts+1),
    icd9ComorbidShortCpp(pts, ac, threads = 4, chunkSize=upts)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, threads = 3, chunkSize=upts-2, ompChunkSize = 1),
    icd9ComorbidShortCpp(pts, ac, threads = 3, chunkSize=upts+2, ompChunkSize = 1)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, threads = 3, chunkSize=upts-2, ompChunkSize = 11),
    icd9ComorbidShortCpp(pts, ac, threads = 3, chunkSize=upts+2, ompChunkSize = 11)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, threads = 3, chunkSize=upts, ompChunkSize = 1),
    icd9ComorbidShortCpp(pts, ac, threads = 3, chunkSize=upts, ompChunkSize = 11)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac),
    icd9ComorbidShortCpp(pts, ac, threads = 3, chunkSize=3, ompChunkSize = 5) # primes < unique visits
  )
})
