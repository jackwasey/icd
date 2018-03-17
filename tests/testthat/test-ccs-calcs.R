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

test_that("ahrq css icd 9 is performing correctly",{
  test_df <-
    data.frame(
      visit_id = c("a", "b", "b", "c"),
      icd9 = c('01012','32341','83314','7721'),
      single = c("1", "77", "225", "224"),
      lvl1 = c("1", "6", "16", "15"),
      lvl2 = c("1.1", "6.1", "16.1", "15.7"),
      lvl3 = c("1.1.1", "6.1.2", " ", "15.7.4"),
      lvl4 = c(" ", " ", " "," "),
      stringsAsFactors = FALSE
    )

  res <- icd9_comorbid_css(test_df,  visit_name = "visit_id", icd_name = "icd9")

  expect_true(all(mapply(function(x,y){res[x,y]}, test_df$visit_id, test_df$single)))
  expect_equal(dim(res), c(3,284))

  expect_error(icd9_comorbid_css(test_df, visit_name = "visit_id",
                                 icd_name = "icd9", type = "Multi"))
  expect_error(icd9_comorbid_css(test_df, visit_name = "visit_id",
                                 icd_name = "icd9", type = "Multi", lvl = "a"))

  res <- icd9_comorbid_css(test_df, visit_name = "visit_id",
                           icd_name = "icd9", type = "Multi", lvl = 1)
  expect_true(all(mapply(function(x,y){res[x,y]}, test_df$visit_id, test_df$lvl1)))
  expect_equal(dim(res), c(3,18))
  res <- icd9_comorbid_css(test_df, visit_name = "visit_id",
                           icd_name = "icd9", type = "Multi", lvl = 2)
  expect_true(all(mapply(function(x,y){res[x,y]}, test_df$visit_id, test_df$lvl2)))
  expect_equal(dim(res), c(3,136))

  res <- icd9_comorbid_css(test_df, visit_name = "visit_id",
                           icd_name = "icd9",  type = "Multi",  lvl = 3)
  expect_true(all(mapply(function(x,y){res[x,y]}, test_df$visit_id, test_df$lvl3)))
  expect_equal(dim(res), c(3,367))

  res <- icd9_comorbid_css(test_df, visit_name = "visit_id", icd_name = "icd9",
                           type = "Multi", lvl = 4)
  expect_true(all(mapply(function(x,y){res[x,y]}, test_df$visit_id, test_df$lvl4)))
  expect_equal(dim(res), c(3,209))
})

