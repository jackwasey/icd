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

context("condense")

library(magrittr, quietly = TRUE, warn.conflicts = FALSE)

test_that("five digit to five digit code range condenses", {
  expect_equal(icd_condense.icd9("34500" %i9sa% "34509", real = FALSE), "3450")
})

test_that("condensing a single real codes gives themselves", {
  expect_that(res <- icd_condense.icd9("61172"), shows_message())
  expect_equal(res, "61172")
  expect_equal(icd_condense.icd9("61172", real = FALSE), "61172")
  expect_equal(icd_condense.icd9("143"), "143")
  expect_equal(icd_condense.icd9("143", real = FALSE), "143")
  expect_equal(icd_condense.icd9("V1221"), "V1221")
  expect_equal(icd_condense.icd9("V1221", real = FALSE), "V1221")
  expect_equal(icd_condense.icd9("V121"), "V121")
  expect_equal(icd_condense.icd9("V121", real = FALSE), "V121")
  expect_equal(icd_condense.icd9("E8280"), "E8280")
  expect_equal(icd_condense.icd9("E8280", real = FALSE), "E8280")
  expect_equal(icd_condense.icd9("E030"), "E030")
  expect_equal(icd_condense.icd9("E030", real = FALSE), "E030")
})

test_that("condense an ICD-9 code set to minimal group", {
  expect_equal(sort(icd_condense.icd9("98799" %i9sa% "98901", real = FALSE)),
               sort(c("98799", "988", "98900", "98901")))
  # non-real end of real range
  expect_that(res <- icd_condense.icd9("988" %i9sa% "98899", real = TRUE), gives_warning())
  expect_equal(res, "988")
  expect_that(res <- icd_condense.icd9("9879" %i9sa% "9891", real = TRUE), gives_warning())
  expect_equal(res, c("9879", "988", "9890", "9891"))
})


test_that("condense ranges which do consense", {
  expect_equal(
    icd_condense.icd9(icd_children.icd9("123", short_code = TRUE, real = TRUE), real = TRUE),
    "123")
  expect_equal(
    icd_condense.icd9(icd_children.icd9("1", short_code = TRUE, real = TRUE), real = TRUE),
    "001")
  expect_equal(icd_condense.icd9(icd_children.icd9("123", short_code = TRUE)), "123")
  expect_equal(icd_condense.icd9(icd_children.icd9("1", short_code = TRUE)), "001")
  for (or1 in c(TRUE, FALSE)) {
    for (or2 in c(TRUE, FALSE)) {
      expect_equal(
        icd_condense.icd9(icd_children.icd9("00321", short_code = TRUE, real = or1), real = or2),
        "00321", info = paste("or1,2 are: ", or1, ", ", or2))
      expect_equal(
        icd_condense.icd9(icd_children.icd9("V1221", short_code = TRUE, real = or1), real = or2),
        "V1221", info = paste("or1,2 are: ", or1, ", ", or2))
    }
  }
  expect_equal(icd_condense.icd9(icd_children.icd9("V12", short_code = TRUE, real = TRUE),
                            real = TRUE), "V12")
  expect_equal(icd_condense.icd9(icd_children.icd9("V12", short_code = TRUE, real = FALSE),
                            real = FALSE), "V12")
})

test_that("condense ranges that don't condense at all", {
  expect_equal(sort(icd_condense.icd9(c("1230", "1232", "1236"), real = FALSE)), c("1230", "1232", "1236"))
  expect_equal(sort(icd_condense.icd9(c("1230", "1232", "1236"), real = TRUE)), c("1230", "1232", "1236"))
  # missing 10009
  expect_equal(sort(icd_condense.icd9(c("1000", as.character(10000:10008)),
                                 real = FALSE)),
               c("1000", as.character(10000:10008)))
})

test_that("condense range invalid data", {
  expect_equal(icd_condense.icd9("turnpike", real = FALSE), character(0))
  expect_equal(icd_condense.icd9(c("turnpike", "road"), real = FALSE), character(0))
  expect_equal(icd_condense.icd9(c(""), real = FALSE), character(0))
  expect_equal(icd_condense.icd9(c("", ""), real = FALSE), character(0))
  expect_equal(icd_condense.icd9(c(NA_character_), real = FALSE), character(0))
  expect_equal(icd_condense.icd9(c(NA_character_, ""), real = FALSE), character(0))
  # one valid vode with invalids
  expect_equal(icd_condense.icd9(c("NA", "rotem", "123"), real = FALSE), "123")
})

test_that("mix of four and five digit billable codes", {
  # this is all of leptospirosis, but missing the "1008" non-billable sub-heading
  expect_equal(
    icd_condense.icd9(short_code = TRUE,c("1000", "10081", "10089", "1009")),
    "100")
})

test_that("mix of four and five digit billable codes over bigger range", {
  # this is all of leptospirosis, but missing the "1008" non-billable sub-heading
  expect_equal(
    icd_condense.icd9(short_code = TRUE,c("1000", "10081", "10089", "1009", "101")),
    c("100", "101"))
})


test_that("mix of four and five digit with non-billable mid-level four digit code", {
  expect_equal(
    icd_condense.icd9(short_code = TRUE,c("1000", "1008", "10081", "10089", "1009")),
    "100")
})


test_that("condense short range", {

  expect_equal(icd9ExplainShort(icd9short_code = othersalmonella),
               "Other salmonella infections")

  expect_equal(icd_condense.icd9(short_code = TRUE,othersalmonella, real = TRUE), "003")
  expect_that(res <- icd_condense.icd9(short_code = TRUE,othersalmonella, real = FALSE), testthat::not(gives_warning()))
  expect_equal(res, othersalmonella)
  # missing this leaf node, we can't condense at all
  expect_equal(icd_condense.icd9(short_code = TRUE,othersalmonella[-3], real = TRUE),
               othersalmonella[-3])
  # if we demand condensing to all possible values, we get the same back
  expect_equal(icd_condense.icd9(short_code = TRUE,othersalmonella[-3], real = FALSE),
               othersalmonella[-3])

  expect_equal(sort(icd9ChildrenShort(icd9short_code = "001", onlyBillable = TRUE)),
               c("0010", "0011", "0019"))

  expect_equal(sort(icd9ChildrenShort(icd9short_code = "001", real = TRUE)),
               c("001", "0010", "0011", "0019"))

  expect_equal(icd_condense.icd9(short_code = TRUE,icd9ChildrenShort("00320", real = TRUE), real = TRUE), "00320")
  # majors should be okay, even if not 'real'
  expect_that(dup_res <- icd_condense.icd9(short_code = TRUE,icd9ChildrenShort("003", real = TRUE)),
              testthat::not(gives_warning()))

  expect_equal(icd_condense.icd9(short_code = TRUE,c("003", "003"), real = TRUE), "003")
  expect_equal(icd_condense.icd9(short_code = TRUE,c("003", "003"), real = FALSE), "003")
})

test_that("condense full ranges", {
  # condensing to "real" means we don't get a lot of majors, which are often not
  # themselves defined.
  # majors:
  expect_equal(icd_condense.icd9(short_code = TRUE,icd9ChildrenShort("003", real = FALSE), real = FALSE), "003")
  expect_equal(icd_condense.icd9(short_code = TRUE,icd9ChildrenShort("3", real = FALSE), real = FALSE), "003")
  expect_equal(icd_condense.icd9(short_code = TRUE,icd9ChildrenShort("410", real = FALSE), real = FALSE), "410")
  expect_equal(icd_condense.icd9(short_code = TRUE,icd9ChildrenShort("V12", real = FALSE), real = FALSE), "V12")
  expect_equal(icd_condense.icd9(short_code = TRUE,icd9ChildrenShort("E800", real = FALSE), real = FALSE), "E800")
  # repeat some tests with decimals instead
  expect_equal(icd_condense.icd9Decimal(icd9Children("003", isshort_code = FALSE, real = FALSE), real = FALSE), "003")
  expect_equal(icd_condense.icd9(icd9ChildrenDecimal("3", real = FALSE), isshort_code = FALSE, real = FALSE), "003")
  expect_equal(icd_condense.icd9Decimal(icd9ChildrenDecimal("410", real = FALSE), real = FALSE), "410")
  expect_equal(icd_condense.icd9Decimal(icd9Children("V12", isshort_code = FALSE, real = FALSE), real = FALSE), "V12")
  expect_equal(icd_condense.icd9Decimal(icd9ChildrenDecimal("E800", real = FALSE), real = FALSE), "E800")
  # repeat some tests with decimals and smaller codes
  expect_equal(icd_condense.icd9Decimal(icd9Children("003.2", isshort_code = FALSE, real = FALSE), real = FALSE),
               "003.2")
  expect_equal(icd_condense.icd9(icd9ChildrenDecimal("3.2", real = FALSE), isshort_code = FALSE, real = FALSE),
               "003.2")
  expect_equal(icd_condense.icd9Decimal(icd9ChildrenDecimal("410.0", real = FALSE), real = FALSE), "410.0")
  expect_equal(icd_condense.icd9Decimal(icd9Children("V12", isshort_code = FALSE, real = FALSE), real = FALSE), "V12")
  expect_equal(icd_condense.icd9Decimal(icd9ChildrenDecimal("E800", real = FALSE), real = FALSE), "E800")

  expect_equal(icd_condense.icd9(short_code = TRUE, icd9ChildrenShort("0031", real = FALSE), real = FALSE), "0031")
  # major is alloect_equal(icd_condense.icd9(short_code = TRUE,c("003", othersalmonella), real = TRUE), "003")
  # major is retupect_equal(icd_condense.icd9(short_code = TRUE,othersalmonella, real = TRUE), "003")
  expect_equal(icd_condense.icd9(short_code = TRUE, othersalmonella, real = FALSE), othersalmonella)
  # now do we fining major if all chilren present?
  almostall003 <- icd9ChildrenShort("003", real = FALSE)
  almostall003 <- almostall003[almostall003 != "003"] # drop the major
  expect_equal(icd_condense.icd9(short_code = TRUE,almostall003, real = FALSE), "003")

  expect_equal(icd_condense.icd9(short_code = TRUE,icd9ChildrenShort("0031", real = FALSE),
                                 real = FALSE), "0031")
  # gives nothing back if a non-billable code provided, but billable requested

  expect_equal(icd_condense.icd9(short_code = TRUE,c("003", othersalmonella), real = TRUE),
               "003") # onlyBillable describes input, it doesn't make any sense to describe output when condensing.
  # major is returned
  expect_equal(icd_condense.icd9(short_code = TRUE, othersalmonella, real = TRUE), "003")
  expect_equal(icd_condense.icd9(short_code = TRUE, othersalmonella, real = FALSE), othersalmonella)
  # now do we find a missing major if all chilren present?
  almostall003 <- icd_children.icd9("003", short_code = TRUE, real = FALSE)
  almostall003 <- almostall003[almostall003 != "003"] # drop the major
  expect_equal(icd_condense.icd9(short_code = TRUE, almostall003, real = FALSE), "003")

})

test_that("condense single major and its children", {
  expect_equal(icd_condense.icd9(short_code = TRUE,"003"), "003")

  rheum_fever <- "Rheumatic fever with heart involvement"
  expect_equal(icd9ExplainShort("391"), rheum_fever)
  expect_equal(icd9ExplainShort(icd_children.icd9("391", short_code = TRUE)), rheum_fever)
  expect_equal(icd9ExplainShort(icd_children.icd9("391", short_code = TRUE, real = TRUE)), rheum_fever)
})

icd9::vermont_dx %>%
  icd_wide_to_long  %>%
  extract2("icdCode")  %>%
  icd9SortShort %>%
  unique %>%
  utils::head(10) -> vdat

test_that("condense a factor of codes instead of character vector", {
  # this is not a condensable list
  dat <- as.factor(vdat)
  expect_equal(dat, icd_condense.icd9(short_code = TRUE,dat))

})

test_that("levels are preserved from source factor", {
  dat <- factor(vdat, levels = c("plastic", vdat))
  expect_identical(dat, icd_condense.icd9(short_code = TRUE,dat, keepFactorLevels = TRUE))
  expect_equivalent(dat, icd_condense.icd9(short_code = TRUE,dat, keepFactorLevels = FALSE))
})
