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

test_that("icd9 CCS map is valid", {
  skip("on hold until it is clarified whether the empty first element is somehow needed")
  expect_true(is_valid(icd9_map_single_ccs, short_code = TRUE))
})

test_that("one code from each single level", {
  first_from_each <-
    vapply(icd9_map_single_ccs, function(y) as.character(y[[1]]), character(1), USE.NAMES = FALSE)
  # drop the (maybe unnecessary empty first group)
  first_from_each <- first_from_each[first_from_each != ""]
  test_all_ccs_df <- data.frame(
    visit_id = rep("z", length(first_from_each)),
    icd9 = first_from_each
  )
  res <- icd9_comorbid_ccs(test_all_ccs_df,  visit_name = "visit_id", icd_name = "icd9")
  res2 <- icd9_comorbid_ccs(test_all_ccs_df,  visit_name = "visit_id", icd_name = "icd9", comorbid_fun = icd:::icd9ComorbidShortCpp)
  expect_identical(res, res2)
  # should be one for each (do this way to ignore the empty first group)
  expect_equal(sum(res), length(first_from_each))

  # same but with strings instead of factors
  test_all_ccs_df <- data.frame(
    visit_id = rep("z", length(first_from_each)),
    icd9 = first_from_each,
    stringsAsFactors = FALSE
  )
  res <- icd9_comorbid_ccs(test_all_ccs_df,  visit_name = "visit_id", icd_name = "icd9")
  res2 <- icd9_comorbid_ccs(test_all_ccs_df,  visit_name = "visit_id", icd_name = "icd9", comorbid_fun = icd:::icd9ComorbidShortCpp)
  expect_identical(res, res2)
  # should be one for each (do this way to ignore the empty first group)
  expect_equal(sum(res), length(first_from_each))
})

test_that("one code from each single level backwards", {
  first_from_each <- rev(
    vapply(icd9_map_single_ccs, function(y) as.character(y[[1]]), character(1), USE.NAMES = FALSE))
  # drop the (maybe unnecessary empty first group)
  first_from_each <- first_from_each[first_from_each != ""]
  test_all_ccs_df <- data.frame(
    visit_id = rep("z", length(first_from_each)),
    icd9 = first_from_each
  )
  res <- icd9_comorbid_ccs(test_all_ccs_df,  visit_name = "visit_id", icd_name = "icd9")
  res2 <- icd9_comorbid_ccs(test_all_ccs_df,  visit_name = "visit_id", icd_name = "icd9", comorbid_fun = icd:::icd9ComorbidShortCpp)
  expect_identical(res, res2)
  # should be one for each (do this way to ignore the empty first group)
  expect_equal(sum(res), length(first_from_each))
})

test_that("one code from each single level backwards with disordered visits", {
  first_from_each <- rev(
    vapply(icd9_map_single_ccs, function(y) as.character(y[[1]]), character(1), USE.NAMES = FALSE))
  # drop the (maybe unnecessary empty first group)
  first_from_each <- first_from_each[first_from_each != ""]
  set.seed(1441)
  rnd_ccs_df <- data.frame(
    visit_id = sample(c("j", "b", "k"), size = length(first_from_each), replace = TRUE),
    icd9 = first_from_each
  )
  res <- icd9_comorbid_ccs(rnd_ccs_df,  visit_name = "visit_id", icd_name = "icd9")
  res2 <- icd9_comorbid_ccs(rnd_ccs_df,  visit_name = "visit_id", icd_name = "icd9", comorbid_fun = icd:::icd9ComorbidShortCpp)
  expect_identical(res, res2)
  # should be one for each (do this way to ignore the empty first group)
  expect_equal(sum(res), length(first_from_each))
})

test_that("ahrq ccs icd 9 is performing correctly", {
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
  if (exists("icd9Comorbid_alt_MatMul")) {
    expect_identical(icd9_comorbid_ccs(test_df, comorbid_fun = icd:::icd9Comorbid_alt_MatMul),
                     icd9_comorbid_ccs(test_df, comorbid_fun = icd:::icd9ComorbidShortCpp))
  }

  a_res <- which(sapply(icd9_map_single_ccs, function(y) "01012" %in% y))
  b_res1 <- which(sapply(icd9_map_single_ccs, function(y) "32341" %in% y))
  b_res2 <- which(sapply(icd9_map_single_ccs, function(y) "83314" %in% y))
  c_res <- which(sapply(icd9_map_single_ccs, function(y) "7721" %in% y))

  # build an unnamed matrix with the right flags set
  manual_res <- matrix(FALSE, nrow = 3, ncol = 284)
  manual_res[1, a_res] <- TRUE
  manual_res[2, b_res1] <- TRUE
  manual_res[2, b_res2] <- TRUE
  manual_res[3, c_res] <- TRUE
  expect_equivalent(manual_res, res)
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
