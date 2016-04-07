# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

context("condense ranges of codes to parents and orphans")

test_that("five digit to five digit code range condenses", {
  expect_equal(icd_condense.icd9("34500" %i9sa% "34509", defined = FALSE), "3450")
})

test_that("condensing a single real codes gives themselves", {
  expect_message(res <- icd_condense.icd9("61172"))
  expect_equal(res, "61172")
  expect_equal(icd_condense.icd9("61172", defined = FALSE), "61172")
  expect_equal(icd_condense.icd9("143"), "143")
  expect_equal(icd_condense.icd9("143", defined = FALSE), "143")
  expect_equal(icd_condense.icd9("V1221"), "V1221")
  expect_equal(icd_condense.icd9("V1221", defined = FALSE), "V1221")
  expect_equal(icd_condense.icd9("V121"), "V121")
  expect_equal(icd_condense.icd9("V121", defined = FALSE), "V121")
  expect_equal(icd_condense.icd9("E8280"), "E8280")
  expect_equal(icd_condense.icd9("E8280", defined = FALSE), "E8280")
  expect_equal(icd_condense.icd9("E030"), "E030")
  expect_equal(icd_condense.icd9("E030", defined = FALSE), "E030")

})

test_that("dispatch character vector for condense", {
  expect_equal(icd_condense("E8280"), "E8280")
  expect_equal(sort(icd_condense(as.character("98799" %i9sa% "98901"), defined = FALSE)),
               sort(c("98799", "988", "98900", "98901")))
})

test_that("condense an ICD-9 code set to minimal group", {
  expect_equal(sort(icd_condense.icd9("98799" %i9sa% "98901", defined = FALSE)),
               sort(c("98799", "988", "98900", "98901")))
  # non-real end of real range
  expect_that(res <- icd_condense.icd9("988" %i9sa% "98899", defined = TRUE), gives_warning())
  expect_equal(res, "988")
  expect_that(res <- icd_condense.icd9("9879" %i9sa% "9891", defined = TRUE), gives_warning())
  expect_equal(res, c("9879", "988", "9890", "9891"))
})


test_that("condense ranges which do consense", {
  expect_equal(
    icd_condense.icd9(icd_children.icd9("123", short_code = TRUE, defined = TRUE), defined = TRUE),
    "123")
  expect_equal(
    icd_condense.icd9(icd_children.icd9("1", short_code = TRUE, defined = TRUE), defined = TRUE),
    "001")
  expect_equal(icd_condense.icd9(icd_children.icd9("123", short_code = TRUE)), "123")
  expect_equal(icd_condense.icd9(icd_children.icd9("1", short_code = TRUE)), "001")
  for (or1 in c(TRUE, FALSE)) {
    for (or2 in c(TRUE, FALSE)) {
      expect_equal(
        icd_condense.icd9(icd_children.icd9("00321", short_code = TRUE, defined = or1), defined = or2),
        "00321", info = paste("or1,2 are: ", or1, ", ", or2))
      expect_equal(
        icd_condense.icd9(icd_children.icd9("V1221", short_code = TRUE, defined = or1), defined = or2),
        "V1221", info = paste("or1,2 are: ", or1, ", ", or2))
    }
  }
  expect_equal(icd_condense.icd9(icd_children.icd9("V12", short_code = TRUE, defined = TRUE),
                                 defined = TRUE), "V12")
  expect_equal(icd_condense.icd9(icd_children.icd9("V12", short_code = TRUE, defined = FALSE),
                                 defined = FALSE), "V12")
})

test_that("condense ranges that don't condense at all", {
  expect_equal(sort(icd_condense.icd9(c("1230", "1232", "1236"), defined = FALSE)), c("1230", "1232", "1236"))
  expect_equal(sort(icd_condense.icd9(c("1230", "1232", "1236"), defined = TRUE)), c("1230", "1232", "1236"))
  # missing 10009
  expect_equal(sort(icd_condense.icd9(c("1000", as.character(10000:10008)),
                                      defined = FALSE)),
               c("1000", as.character(10000:10008)))
})

test_that("condense range invalid data", {
  expect_equal(icd_condense.icd9("turnpike", defined = FALSE), character(0))
  expect_equal(icd_condense.icd9(c("turnpike", "road"), defined = FALSE), character(0))
  expect_equal(icd_condense.icd9(c(""), defined = FALSE), character(0))
  expect_equal(icd_condense.icd9(c("", ""), defined = FALSE), character(0))
  expect_equal(icd_condense.icd9(c(NA_character_), defined = FALSE), character(0))
  expect_equal(icd_condense.icd9(c(NA_character_, ""), defined = FALSE), character(0))
  # one valid vode with invalids
  expect_equal(icd_condense.icd9(c("NA", "rotem", "123"), defined = FALSE), "123")
})

test_that("mix of four and five digit billable codes", {
  # this is all of leptospirosis, but missing the "1008" non-billable sub-heading
  expect_equal(
    icd_condense.icd9(short_code = TRUE, c("1000", "10081", "10089", "1009")),
    "100")
})

test_that("mix of four and five digit billable codes over bigger range", {
  # this is all of leptospirosis, but missing the "1008" non-billable sub-heading
  expect_equal(
    icd_condense.icd9(short_code = TRUE, c("1000", "10081", "10089", "1009", "101")),
    c("100", "101"))
})


test_that("mix of four and five digit with non-billable mid-level four digit code", {
  expect_equal(
    icd_condense.icd9(short_code = TRUE, c("1000", "1008", "10081", "10089", "1009")),
    "100")
})


test_that("condense short range", {

  expect_equal(icd_explain.icd9(othersalmonella), "Other salmonella infections")

  expect_equal(icd_condense.icd9(short_code = TRUE, othersalmonella, defined = TRUE), "003")
  expect_warning(res <- icd_condense.icd9(short_code = TRUE, othersalmonella, defined = FALSE), regexp = NA)
  expect_equal(res, othersalmonella)
  # missing this leaf node, we can't condense at all
  expect_equal(icd_condense.icd9(short_code = TRUE, othersalmonella[-3], defined = TRUE),
               othersalmonella[-3])
  # if we demand condensing to all possible values, we get the same back
  expect_equal(icd_condense.icd9(short_code = TRUE, othersalmonella[-3], defined = FALSE),
               othersalmonella[-3])

  expect_equal_no_icd(sort(icd_children.icd9(short_code = TRUE, "001", billable = TRUE)),
                      c("0010", "0011", "0019"))

  expect_equal_no_icd(sort(icd_children.icd9(short_code = TRUE, x = "001", defined = TRUE)),
                      c("001", "0010", "0011", "0019"))

  expect_equal(icd_condense.icd9(short_code = TRUE,
                                 icd_children.icd9(short_code = TRUE, "00320", defined = TRUE), defined = TRUE), "00320")
  # majors should be okay, even if not 'real'
  expect_warning(dup_res <- icd_condense.icd9(short_code = TRUE,
                                              icd_children.icd9(short_code = TRUE, "003", defined = TRUE)), regexp = NA)

  expect_equal(icd_condense.icd9(short_code = TRUE, c("003", "003"), defined = TRUE), "003")
  expect_equal(icd_condense.icd9(short_code = TRUE, c("003", "003"), defined = FALSE), "003")
})

test_that("condense full ranges", {
  # condensing to "real" means we don't get a lot of majors, which are often not
  # themselves defined.
  # majors:
  expect_equal(icd_condense.icd9(short_code = TRUE,
                                 icd_children.icd9(short_code = TRUE, "003", defined = FALSE), defined = FALSE), "003")
  expect_equal(icd_condense.icd9(short_code = TRUE,
                                 icd_children.icd9(short_code = TRUE, "3", defined = FALSE), defined = FALSE), "003")
  expect_equal(icd_condense.icd9(short_code = TRUE,
                                 icd_children.icd9(short_code = TRUE, "410", defined = FALSE), defined = FALSE), "410")
  expect_equal(icd_condense.icd9(short_code = TRUE,
                                 icd_children.icd9(short_code = TRUE, "V12", defined = FALSE), defined = FALSE), "V12")
  expect_equal(icd_condense.icd9(short_code = TRUE,
                                 icd_children.icd9(short_code = TRUE, "E800", defined = FALSE), defined = FALSE), "E800")
  # repeat some tests with decimals instead
  expect_equal_no_icd(icd_condense.icd9(short_code = FALSE,
                                        icd_children.icd9("003", short_code = FALSE, defined = FALSE), defined = FALSE), "003")
  expect_equal_no_icd(icd_condense.icd9(
    icd_children.icd9(short_code = FALSE, "3", defined = FALSE), short_code = FALSE, defined = FALSE), "003")
  expect_equal_no_icd(icd_condense.icd9(short_code = FALSE,
                                        icd_children.icd9(short_code = FALSE, "410", defined = FALSE), defined = FALSE), "410")
  expect_equal_no_icd(icd_condense.icd9(short_code = FALSE,
                                        icd_children.icd9("V12", short_code = FALSE, defined = FALSE), defined = FALSE), "V12")
  expect_equal_no_icd(icd_condense.icd9(short_code = FALSE,
                                        icd_children.icd9(short_code = FALSE, "E800", defined = FALSE), defined = FALSE), "E800")
  # repeat some tests with decimals and smaller codes
  expect_equal_no_icd(icd_condense.icd9(short_code = FALSE,
                                        icd_children.icd9("003.2", short_code = FALSE, defined = FALSE), defined = FALSE),
                      "003.2")
  expect_equal_no_icd(icd_condense.icd9(
    icd_children.icd9(short_code = FALSE, "3.2", defined = FALSE), short_code = FALSE, defined = FALSE),
    "003.2")
  expect_equal_no_icd(icd_condense.icd9(short_code = FALSE,
                                        icd_children.icd9(short_code = FALSE, "410.0", defined = FALSE), defined = FALSE), "410.0")
  expect_equal_no_icd(icd_condense.icd9(short_code = FALSE,
                                        icd_children.icd9("V12", short_code = FALSE, defined = FALSE), defined = FALSE), "V12")
  expect_equal_no_icd(icd_condense.icd9(short_code = FALSE,
                                        icd_children.icd9(short_code = FALSE, "E800", defined = FALSE), defined = FALSE), "E800")

  expect_equal(icd_condense.icd9(short_code = TRUE,
                                 icd_children.icd9(short_code = TRUE, "0031", defined = FALSE), defined = FALSE), "0031")
  # major is alloect_equal(icd_condense.icd9(short_code = TRUE,c("003", othersalmonella), defined = TRUE), "003")
  # major is retupect_equal(icd_condense.icd9(short_code = TRUE,othersalmonella, defined = TRUE), "003")
  expect_equal(icd_condense.icd9(short_code = TRUE, othersalmonella, defined = FALSE), othersalmonella)
  # now do we fining major if all chilren present?
  almostall003 <- icd_children.icd9(short_code = TRUE, "003", defined = FALSE)
  almostall003 <- almostall003[almostall003 != "003"] # drop the major
  expect_equal(icd_condense.icd9(short_code = TRUE, almostall003, defined = FALSE), "003")

  expect_equal(icd_condense.icd9(short_code = TRUE, icd_children.icd9(short_code = TRUE, "0031", defined = FALSE),
                                 defined = FALSE), "0031")
  # gives nothing back if a non-billable code provided, but billable requested

  expect_equal(icd_condense.icd9(short_code = TRUE, c("003", othersalmonella), defined = TRUE),
               "003") # billable describes input, it doesn't make any sense to describe output when condensing.
  # major is returned
  expect_equal(icd_condense.icd9(short_code = TRUE, othersalmonella, defined = TRUE), "003")
  expect_equal(icd_condense.icd9(short_code = TRUE, othersalmonella, defined = FALSE), othersalmonella)
  # now do we find a missing major if all chilren present?
  almostall003 <- icd_children.icd9("003", short_code = TRUE, defined = FALSE)
  almostall003 <- almostall003[almostall003 != "003"] # drop the major
  expect_equal(icd_condense.icd9(short_code = TRUE, almostall003, defined = FALSE), "003")

})

test_that("condense single major and its children", {
  expect_equal(icd_condense.icd9(short_code = TRUE, "003"), "003")

  rheum_fever <- "Rheumatic fever with heart involvement"
  expect_equal(icd_explain.icd9("391"), rheum_fever)
  expect_equal(icd_explain.icd9(icd_children.icd9("391", short_code = TRUE)), rheum_fever)
  expect_equal(icd_explain.icd9(icd_children.icd9("391", short_code = TRUE, defined = TRUE)), rheum_fever)
})

icd::vermont_dx %>%
  icd_wide_to_long  %>%
  extract2("icd_code")  %>%
  icd_sort.icd9(short_code = TRUE) %>%
  unique %>%
  utils::head(10) -> vdat

test_that("condense a factor of codes instead of character vector", {
  # this is not a condensable list
  dat <- as.factor(vdat)
  expect_equal(dat, icd_condense.icd9(short_code = TRUE, dat))

})

test_that("levels are preserved from source factor", {
  dat <- factor(vdat, levels = c("plastic", vdat))
  expect_identical(dat, icd_condense.icd9(short_code = TRUE, dat, keep_factor_levels = TRUE))
  expect_equivalent(dat, icd_condense.icd9(short_code = TRUE, dat, keep_factor_levels = FALSE))
})
