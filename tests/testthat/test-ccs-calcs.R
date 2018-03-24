# Copyright (C) 2014 - 2018  Jack O. Wasey
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

context("ahrq ccs calculations")

test_that("ahrq css icd 9 is performing correctly", {
  test_df <-
    data.frame(
      visit_id = c("a", "b", "b", "c"),
      icd9 = c("01012", "32341", "83314", "7721"),
      single = c("1", "77", "225", "224"),
      lvl1 = c("1", "6", "16", "15"),
      lvl2 = c("1.1", "6.1", "16.1", "15.7"),
      lvl3 = c("1.1.1", "6.1.2", " ", "15.7.4"),
      lvl4 = c(" ", " ", " ", " "),
      stringsAsFactors = FALSE
    )

  res <- icd9_comorbid_ccs(test_df,  visit_name = "visit_id", icd_name = "icd9")
  expect_true(all(mapply(function(x, y) res[x, y], test_df$visit_id, test_df$single)))
  expect_equal(dim(res), c(3, 284))

  expect_error(icd9_comorbid_ccs(test_df, visit_name = "visit_id", icd_name = "icd9", single = FALSE))
  expect_error(icd9_comorbid_ccs(test_df, visit_name = "visit_id", icd_name = "icd9", single = FALSE, lvl = "a"))

  res <- icd9_comorbid_ccs(test_df, visit_name = "visit_id", icd_name = "icd9", single = FALSE, lvl = 1)
  expect_true(all(mapply(function(x, y) res[x, y], test_df$visit_id, test_df$lvl1)))
  expect_equal(dim(res), c(3, 18))

  res <- icd9_comorbid_ccs(test_df, visit_name = "visit_id", icd_name = "icd9", single = FALSE, lvl = 2)
  expect_true(all(mapply(function(x, y) res[x, y], test_df$visit_id, test_df$lvl2)))
  expect_equal(dim(res), c(3, 136))

  res <- icd9_comorbid_ccs(test_df, visit_name = "visit_id", icd_name = "icd9",  single = FALSE,  lvl = 3)
  expect_true(all(mapply(function(x, y) res[x, y], test_df$visit_id, test_df$lvl3)))
  expect_equal(dim(res), c(3, 367))

  res <- icd9_comorbid_ccs(test_df, visit_name = "visit_id", icd_name = "icd9", single = FALSE, lvl = 4)
  expect_true(all(mapply(function(x, y) res[x, y], test_df$visit_id, test_df$lvl4)))
  expect_equal(dim(res), c(3, 209))
})


test_that("ahrq css icd 10 is performing correctly", {
  test_df <-
    data.frame(
      visit_id = c("a", "b", "b", "c"),
      icd10 = c("M2578", "Z5681", "W290XXD", "L408"),
      single = c("204", "255", "2601", "198"),
      lvl1 = c("13", "17", "18", "12"),
      lvl2 = c("13.2", "17.2", " ", "12.2"),
      stringsAsFactors = FALSE
    )


  res <-
    icd10_comorbid_ccs(test_df,  visit_name = "visit_id", icd_name = "icd10")
  expect_true(all(mapply(
    function(x, y)
      res[x, y], test_df$visit_id, test_df$single
  )))
  expect_equal(dim(res), c(3, 283)) #there is no more cat 0 in icd10

  expect_error(
    icd10_comorbid_ccs(
      test_df,
      visit_name = "visit_id",
      icd_name = "icd10",
      single = FALSE
    )
  )
  expect_error(
    icd10_comorbid_ccs(
      test_df,
      visit_name = "visit_id",
      icd_name = "icd10",
      single = FALSE,
      lvl = "a"
    )
  )

  res <-
    icd10_comorbid_ccs(
      test_df,
      visit_name = "visit_id",
      icd_name = "icd10",
      single = FALSE,
      lvl = 1
    )
  expect_true(all(mapply(
    function(x, y)
      res[x, y], test_df$visit_id, test_df$lvl1
  )))
  expect_equal(dim(res), c(3, 18))

  res <-
    icd10_comorbid_ccs(
      test_df,
      visit_name = "visit_id",
      icd_name = "icd10",
      single = FALSE,
      lvl = 2
    )
  expect_true(all(mapply(
    function(x, y)
      res[x, y], test_df$visit_id, test_df$lvl2
  )))
  expect_equal(dim(res), c(3, 136))

})
