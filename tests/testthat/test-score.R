context("test Charlson and counting")

test_that("only matrix or data.frame accepted", {
  expect_error(icd9Charlson(c(1, 2)))
  expect_error(icd9Charlson(c(1, 2), visitId = "roam", return.df = TRUE, stringsAsFactors = TRUE))
})

test_that("Charlson score", {

  mydf <- data.frame(visitId = c("a", "b", "c"),
                     icd9 = c("441", "412.93", "044.9"),
                     stringsAsFactors = TRUE)
  expect_equal(
    icd9CharlsonComorbid(
      icd9ComorbidQuanDeyo(mydf, isShort = FALSE, applyHierarchy = TRUE, return.df = TRUE)
    ),
    icd9Charlson(mydf, isShort = FALSE, return.df = FALSE)
  )

  expect_equivalent(icd9Charlson(mydf,
                            return.df = TRUE,
                            stringsAsFactors = TRUE,
                            isShort = FALSE),
               structure(list(visitId = structure(1:3,
                                                  .Label = c("a", "b", "c"),
                                                  class = "factor"),
                              Charlson = c(1, 1, 6)),
                         .Names = c("visitId", "Charlson"),
                         row.names = c(NA, -3L),
                         class = "data.frame")
  )

  mydff <- data.frame(visitId = c("a", "b", "c"),
                      icd9 = c("441", "412.93", "044.9"),
                      stringsAsFactors = FALSE)

  expect_equivalent(icd9Charlson(mydff,
                            return.df = TRUE,
                            stringsAsFactors = TRUE,
                            isShort = FALSE),
               structure(list(visitId = c("a", "b", "c"),
                              Charlson = c(1, 1, 6)),
                         .Names = c("visitId", "Charlson"),
                         row.names = c(NA, -3L),
                         class = "data.frame")
  )

  mydfff <- mydff
  names(mydfff)[1] <- "v"
  expect_equivalent(icd9Charlson(mydfff,
                            return.df = TRUE,
                            stringsAsFactors = FALSE,
                            isShort = FALSE),
               structure(list(v = c("a", "b", "c"),
                              Charlson = c(1, 1, 6)),
                         .Names = c("v", "Charlson"),
                         row.names = c(NA, -3L),
                         class = "data.frame")
  )

  mydffff <- cbind(mydfff, data.frame(v2 = mydfff$v, stringsAsFactors = FALSE))
  mydffff$v <- NULL
  expect_equivalent(icd9Charlson(mydffff, visitId = "v2",
                            return.df = TRUE,
                            stringsAsFactors = FALSE,
                            isShort = FALSE),
               structure(list(v2 = c("a", "b", "c"),
                              Charlson = c(1, 1, 6)),
                         .Names = c("v2", "Charlson"),
                         row.names = c(NA, -3L),
                         class = "data.frame")
  )

  baddf <- data.frame(visitId = c("d", "d"),
                      icd9 = c("2500", "25042"),
                      stringsAsFactors = TRUE)
  cmb <- icd9ComorbidQuanDeyo(baddf, applyHierarchy = FALSE, isShort = TRUE)
  expect_error(icd9CharlsonComorbid(cmb, applyHierarchy = FALSE))

  baddf <- data.frame(visitId = c("d", "d"),
                      icd9 = c("2500", "25042"),
                      stringsAsFactors = TRUE)
  cmb <- icd9ComorbidQuanDeyo(baddf, applyHierarchy = FALSE, isShort = TRUE)
  expect_error(icd9CharlsonComorbid(cmb, applyHierarchy = FALSE))

  baddf <- data.frame(visitId = c("d", "d"),
                      icd9 = c("57224", "57345"),
                      stringsAsFactors = TRUE)
  cmb <- icd9ComorbidQuanDeyo(baddf, applyHierarchy = FALSE, isShort = TRUE)
  expect_error(icd9CharlsonComorbid(cmb, applyHierarchy = FALSE))
})

test_that("count icd9 codes", {
  mydf <- data.frame(visitId = c("r", "r", "s"),
                     icd9 = c("441", "412.93", "044.9"))
  expect_equal(icd9Count(mydf, return.df = TRUE),
               data.frame(visitId = c("r", "s"),
                          icd9Count = c(2, 1))
  )
  expect_equal(icd9Count(mydf), c(2, 1))

  cmb <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE, return.df = TRUE)
  expect_equivalent(icd9CountComorbidBin(cmb), icd9Count(mydf))

  wide <- data.frame(visitId = c("r", "s", "t"),
                     icd9_1 = c("0011", "441", "456"),
                     icd9_2 = c(NA, "442", NA),
                     icd9_3 = c(NA, NA, "510"))

  expect_equal(icd9CountWide(wide),
               c("r" = 1, "s" = 2, "t" = 2))

  widezero <- data.frame(visitId = c("j"),
                         icd9_a = NA,
                         icd9_b = NA)
  expect_equal(icd9CountWide(widezero),
               c("j" = 0))

  widezero2 <- data.frame(visitId = c("j"),
                          icd9_a = NA)
  expect_equal(icd9CountWide(widezero2),
               c("j" = 0))

  widezero3 <- data.frame(visitId = c("j", "j"),
                          icd9_a = c(NA, NA))
  expect_equal(icd9CountWide(widezero3, aggregate = TRUE),
               c("j" = 0))

  widezero4 <- data.frame(visitId = c("j", "j"),
                          icd9_a = c(NA, NA),
                          icd9_b = c(NA, NA))
  expect_equal(icd9CountWide(widezero4, aggregate = TRUE),
               c("j" = 0))

  widezero3b <- data.frame(visitId = c("j", "j"),
                           icd9_a = c(NA, NA))
  expect_equal(icd9CountWide(widezero3b, aggregate = FALSE),
               c("j" = 0, "j" = 0))

  widezero4b <- data.frame(visitId = c("j", "j"),
                           icd9_a = c(NA, NA),
                           icd9_b = c(NA, NA))
  expect_equal(icd9CountWide(widezero4b, aggregate = FALSE),
               c("j" = 0, "j" = 0))

  widezero5 <- data.frame(visitId = c("j", "k"),
                          icd9_a = c(NA, NA))
  expect_equal(icd9CountWide(widezero5),
               c("j" = 0, "k" = 0))

  widezero6 <- data.frame(visitId = c("j", "k"),
                          icd9_a = c(NA, NA),
                          icd9_b = c(NA, NA))
  expect_equal(icd9CountWide(widezero6),
               c("j" = 0, "k" = 0))

})

test_that("count wide directly (old func) same as reshape count", {

  widedf <- data.frame(visitId = c("a", "b", "c"),
                       icd9_01 = c("441", "4424", "441"),
                       icd9_02 = c(NA, "443", NA))

  # we don't get names back for the vector for 'long'
  expect_equivalent(icd9CountWide(widedf),
                    icd9Count(icd9WideToLong(widedf)))
})
