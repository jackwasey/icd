# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

context("test deprecated Charlson and counting")

test_that("github issue #44 from wmurphyrd", {
  mydf <- data.frame(visitId = c("a", "b", "c", "a", "b", "d"),
                     icd9 = c("441", "412.93", "044.9", "250.0", "250.0", "250.0"),
                     stringsAsFactors = TRUE)
  expect_that(icd9Charlson(mydf, return.df = TRUE), testthat::not(throws_error()))
})

test_that("github issue #46 from wmurphyd", {
  mydf <- data.frame(visitId = "a", icd9 = "250.0")
  comorbids <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE, return.df = TRUE)
  set.seed(123)
  # Fill a QuanDeyo comorbidity data frame with random data
  comorbids <- rbind(comorbids, data.frame(
    visitId = letters[2:10], matrix(runif((ncol(comorbids) - 1) * 9) > 0.7,
                                    ncol=17, dimnames = list(character(0), names(comorbids[2:18]))
    )
  )
  )
  c2.inv <- cbind(t(comorbids[2,2:18]),c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 6, 6))
  expect_equivalent(
    icd9CharlsonComorbid(comorbids, applyHierarchy = TRUE)[2],
    sum(apply(c2.inv,1,prod))
  )
})

test_that("only matrix or data.frame accepted", {
  expect_error(icd9Charlson(c(1, 2)))
  expect_error(icd9Charlson(c(1, 2), visitId = "roam", return.df = TRUE, stringsAsFactors = TRUE))
  expect_error(icd9Charlson(list(1, 2)))
  expect_error(icd9Charlson(list(1, 2), visitId = "roam", return.df = TRUE, stringsAsFactors = TRUE))
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

  expect_identical(icd9Charlson(mydff,
                                return.df = TRUE,
                                stringsAsFactors = FALSE,
                                isShort = FALSE),
                   structure(list(visitId = c("a", "b", "c"),
                                  Charlson = c(1, 1, 6)),
                             .Names = c("visitId", "Charlson"),
                             row.names = c(NA, -3L),
                             class = "data.frame")
  )

  expect_identical(icd9Charlson(mydff,
                                return.df = TRUE,
                                stringsAsFactors = TRUE,
                                isShort = FALSE),
                   structure(list(visitId = factor(c("a", "b", "c")),
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
})

test_that("Charlson - errors?", {
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

test_that("icd9VanWalravenComorbid score calculation", {

  comorbids <- icd9ComorbidQuanElix(
    mydf <- data.frame(visitId = "a", icd9 = "250.0"),
    return.df = TRUE)
  set.seed(123)
  # Fill a QuanElix comorbidity data frame with random data
  comorbids <- rbind(comorbids,
                     data.frame(visitId = letters[2:10],
                                matrix(stats::runif((ncol(comorbids) - 1) * 9) > 0.7,
                                       ncol = ncol(comorbids) - 1,
                                       dimnames = list(character(0), names(comorbids[2:31])))))
  c2.inv <- cbind(t(comorbids[2, -1]),
                  c(7, 5, -1, 4, 2, 0, 7, 6, 3, 0, 0, 0, 5, 11, 0, 0,
                    9, 12, 4, 0, 3, -4, 6, 5, -2, -2, 0, -7, 0, -3))
  expect_equivalent(
    icd9VanWalravenComorbid(comorbids, applyHierarchy = TRUE)[2],
    sum(apply(c2.inv, 1, prod))
  )
})

test_that("icd9VanWalraven comodbidity index and score", {
  mydf <- data.frame(id = factor(c(rep(1, 20), rep(2, 20), rep(3, 18))),
                     value = c("324.1", "285.9", "599.70", "038.9", "278.00", "38.97",
                             "V88.01", "112.0", "427.89", "790.4", "401.9", "53.51", "584.9",
                             "415.12", "995.91", "996.69", "83.39", "V46.2", "V58.61", "276.69",
                             "515", "V14.6", "784.0", "V85.1", "427.31", "V85.44", "300",
                             "86.28", "569.81", "041.49", "486", "45.62", "V15.82", "496",
                             "261", "280.9", "275.2", "96.59", "V49.86", "V10.42", "276.8",
                             "710.4", "311", "041.12", "276.0", "790.92", "518.84", "552.21",
                             "V85.41", "278.01", "V15.82", "96.72", "070.70", "285.29", "276.3",
                             "V66.7", "272.4", "790.92"))
  expect_equivalent(icd9VanWalraven(mydf, visitId = "id", icd9Field = "value"),
                    icd9VanWalravenComorbid(
                      icd9ComorbidQuanElix(mydf, visitId = "id", icd9Field = "value")))
  expect_equivalent(icd9VanWalraven(mydf, visitId = "id", icd9Field="value", return.df = TRUE),
                    data.frame(id = factor(c(1, 2, 3)),
                                   vanWalraven = c(10, 12, -2)))
  expect_equal(
    icd9VanWalraven(mydf, icd9Field = "value"),
               structure(c(10, 12, -2), names = c("1", "2", "3"))
    )
})

test_that("github issue #64 - quan revised charleson scores", {
  mydf <- data.frame(visitId = "a", icd9 = "250.0")
  comorbids <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE, return.df = TRUE)

  ## test against a known score for a single comorbidity

  # direct scoring
  expect_equivalent(icd9CharlsonComorbid(comorbids, scoringSystem = "original"), 1)
  expect_equivalent(icd9CharlsonComorbid(comorbids, scoringSystem = "charlson"), 1)
  expect_equivalent(icd9CharlsonComorbid(comorbids, scoringSystem = "quan"), 0)

  # pass through from top level function
  expect_equivalent(icd9Charlson(mydf, scoringSystem = "o"), 1)
  expect_equivalent(icd9Charlson(mydf, scoringSystem = "c"), 1)
  expect_equivalent(icd9Charlson(mydf, scoringSystem = "q"), 0)

  ## test against randomly generated comorbidities in various conditions
  set.seed(456)
  # Fill a QuanDeyo comorbidity data frame with random data
  comorbids <- rbind(comorbids, data.frame(
    visitId = letters[2:10], matrix(runif((ncol(comorbids) - 1) * 9) > 0.7,
                                    ncol=17, dimnames = list(character(0),
                                                             names(comorbids[2:18])))))
  comorbids[,"DM"] <- comorbids[, "DM"] & !comorbids[, "DMcx"]
  comorbids[, "LiverMild"] <- comorbids[, "LiverMild"] & !comorbids[, "LiverSevere"]
  comorbids[, "Cancer"] <- comorbids[, "Cancer"] & !comorbids[, "Mets"]

  original_weights <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 6, 6)
  quan_weights     <- c(0, 2, 0, 0, 2, 1, 1, 0, 2, 0, 1, 2, 1, 2, 4, 6, 4)

  #omitting scoringSystem argument should use original scores
  expect_equivalent(
    icd9CharlsonComorbid(comorbids, applyHierarchy = TRUE)[2],
    sum(apply(cbind(t(comorbids[2,2:18]),original_weights),1,prod))
  )

  #specify original scores
  expect_equivalent(
    icd9CharlsonComorbid(comorbids, applyHierarchy = TRUE, scoringSystem = "original")[3],
    sum(apply(cbind(t(comorbids[3,2:18]),original_weights), 1, prod))
  )
  expect_equivalent(
    icd9CharlsonComorbid(comorbids, applyHierarchy = TRUE, scoringSystem = "charlson")[3],
    sum(apply(cbind(t(comorbids[3,2:18]),original_weights), 1, prod))
  )

  #specify quan scores
  expect_equivalent(
    icd9CharlsonComorbid(comorbids, applyHierarchy = TRUE, scoringSystem = "quan")[4],
    sum(apply(cbind(t(comorbids[4,2:18]),quan_weights), 1, prod))
  )

  #partial matching of scoringSystem argument
  expect_equivalent(
    icd9CharlsonComorbid(comorbids, applyHierarchy = TRUE, scoringSystem = "o")[5],
    sum(apply(cbind(t(comorbids[5,2:18]),original_weights), 1, prod))
  )
  expect_equivalent(
    icd9CharlsonComorbid(comorbids, applyHierarchy = TRUE, scoringSystem = "q")[6],
    sum(apply(cbind(t(comorbids[6,2:18]),quan_weights), 1, prod))
  )

  #invalid scoringSystem argument
  expect_error(icd9CharlsonComorbid(comorbids,
                                    applyHierarchy = TRUE, scoringSystem = "z"))

})
