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

test_that("icd_explain_table, with and without condense returns correct structure", {
  codes <- c("362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414")
  expect_equal(dim(icd_explain_table(codes, condense = FALSE)), c(7, 10))
  expect_equal(dim(icd_explain_table(codes, condense = TRUE)), c(6, 12))
  expect_equal(names(icd_explain_table(codes, condense = TRUE))[c(11, 12)],
               c("condensed_codes", "condensed_num"))
})

test_that("icd_explain_table reproduces icd_explain.list (element-by-element) with mixed major and minor codes", {
  codes <- c("362.5", "413.9", "010.02", "584.9", "357.2", "588.81", "010")
  method1 <- unlist(icd:::icd_explain.list(codes))
  method2 <- icd_explain_table(codes, condense = FALSE)[["long_desc"]]
  expect_identical(method1, method2)
})

test_that("icd_explain_table can handle invalid icd by filling with NAs.", {

  res <- icd_explain_table("Rick Shaw", condense = TRUE)
  expect_true(is.na(res[["short_desc"]]))
  expect_true(is.na(res[["is_major"]])) # could be false?
  expect_equal(res[["condensed_codes"]], res[["code"]])
  expect_equal(res[["condensed_num"]], 1L)

  res <- icd_explain_table(c("bogus code", "Percy Veer"), condense = TRUE)
  expect_true(all(is.na(res[["short_desc"]])))
  expect_true(all(is.na(res[["is_major"]])))
  expect_equal(res[["condensed_codes"]], res[["code"]])
  expect_equal(res[["condensed_num"]], c(1L, 1L))
})

test_that("condensing icd_explain_table generates correct columns", {
  dat <- data.frame(code = c("123", "123.4"),
                    three_digit = factor(c("123", "123")),
                    stringsAsFactors = FALSE)

  expect_identical(
    condense_explain_table_to_majors_worker(dat),
    structure(
      list(code = structure(1L, .Label = "123", class = "factor"), condensed_codes = "123, 123.4", condensed_num = 2L),
      .Names = c("three_digit", "condensed_codes", "condensed_num"),
      row.names = c(NA, -1L), class = "data.frame")
  )
  expect_identical(condense_explain_table_to_majors_worker(dat)$condensed_num, 2L)

  dat <- data.frame(code = c("123", "123.4"),
                    three_digit = factor(c("123", "123")),
                    stringsAsFactors = FALSE)
  expect_identical(condense_explain_table_to_majors_worker(dat)$condensed_num, 2L)

  dat <- data.frame(code = c("123.3", "123.4"),
                    three_digit = factor(c("123", "123")),
                    stringsAsFactors = FALSE)
  expect_identical(condense_explain_table_to_majors_worker(dat)$condensed_num, 2L)

  dat <- data.frame(code = c("789.0", "123.4"),
                    three_digit = factor(c("789", "123")),
                    stringsAsFactors = FALSE)
  res <- condense_explain_table_to_majors_worker(dat)
  expect_is(res$condensed_num, "integer")
  expect_identical(dim(res), c(0L, 3L))

  dat <- data.frame(code = c("radish", "feral"),
                    three_digit = factor(c("goniatite", "slalom")),
                    stringsAsFactors = FALSE)
  res <- condense_explain_table_to_majors_worker(dat)
  expect_is(res$condensed_num, "integer")
  expect_identical(dim(res), c(0L, 3L))

  # NA codes are dropped
  dat <- data.frame(code = c("radish", NA),
                    three_digit = factor(c("goniatite", NA)),
                    stringsAsFactors = FALSE)
  expect_identical(dim(condense_explain_table_to_majors_worker(dat)), c(0L, 3L))

  dat <- data.frame(code = c("carvedilol", NA),
                    three_digit = factor(c(NA, NA)),
                    stringsAsFactors = FALSE)
  expect_identical(nrow(condense_explain_table_to_majors_worker(dat)), 0L)
})

test_that("icd_explain_table num_condense sum after condense equals input length", {

  variations <- list(
    c("362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414"),
    c("362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414", "bogus code"),
    "another bogus",
    c("one bogus", "two bogus"))
  for (codes in variations) {
    res <- icd_explain_table(codes, condense = TRUE)
    expect_is(res[["condensed_num"]], "integer")
    expect_equal(
      sum(res$condensed_num, na.rm = TRUE),
      length(codes),
      info = paste("codes", codes, collapse = " ")
    )
  }
})

test_that("icd_explain_table, appropriately convert mixed code character vector,
          casted icd9, and casted icd10 vectors: ", {

            codes <- c("N18.3", "414", "362.5")
            res <- icd_explain_table(icd9(codes))
            expect_equal(is.na(res$short_desc), c(TRUE, FALSE, FALSE))

            res <- icd_explain_table(icd10(codes))
            expect_equal(is.na(res$short_desc), c(FALSE, TRUE, TRUE))
          })
