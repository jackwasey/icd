context("quick comorbidity test")
twoPts <- list(visitId = c("v01", "v01", "v02", "v02"),
               icd9 = c("040", "000", "100", "000"))
twoMap <- list("malady" = c("100", "2000"), "ailment" = c("003", "040"))

test_that("comorbid quick test", {
  testres <- icd9Comorbid(twoPts, twoMap)
  trueres <- data.frame("visitId" = c("v01", "v02"),
                        "malady" = c(FALSE, TRUE),
                        "ailment" = c(TRUE, FALSE),
                        stringsAsFactors = FALSE)
  print(testres)
  print(trueres)
  expect_equal(testres, trueres)

})

test_that("control params don't affect result of comorbid calc", {
  pts <- randomPatients(101, 13)
  expect_identical(
    icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid, threads = 1, chunkSize=32),
    icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid, threads = 3, chunkSize=32)
  )
  expect_identical(
    icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid, threads = 3, chunkSize=1),
    icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid, threads = 3, chunkSize=32)
  )
  expect_identical(
    icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid, threads = 3, chunkSize=1, ompChunkSize = 1),
    icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid, threads = 3, chunkSize=33, ompChunkSize = 1)
  )
  expect_identical(
    icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid, threads = 3, chunkSize=1, ompChunkSize = 11),
    icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid, threads = 3, chunkSize=33, ompChunkSize = 11)
  )
  expect_identical(
    icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid),
    icd9ComorbidShortOpenMPVecInt(pts, ahrqComorbid, threads = 3, chunkSize=17, ompChunkSize = 7)
  )
})
