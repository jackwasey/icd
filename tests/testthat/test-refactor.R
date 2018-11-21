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

context("refactor")

test_that("simple cases", {
  expect_identical(refactor(factor("a", levels = "a"), "a"),
                   factor("a", levels = "a"))
  expect_identical(refactor(factor(NA, levels = "a"), "a"),
                   factor(NA, levels = "a"))
  expect_identical(refactor(factor(NA, levels = "a"), "b"),
                   factor(NA, levels = "b"))
  expect_identical(refactor(factor(NA, levels = "a"), "a"),
                   factor(NA, levels = "a"))
  expect_identical(refactor(factor(c("a", "b")), "a"),
                   factor(c("a", NA), levels = "a"))
  expect_identical(refactor(factor(c("a", "b")), "b"),
                   factor(c(NA, "b"), levels = "b"))
  expect_identical(refactor(factor(c("a", "b")), "c"),
                   factor(c("a", "b"), levels = "c"))
})

test_that("simple na.rm cases", {
  expect_identical(refactor(factor("a", levels = "a"), "a", na.rm = TRUE),
                   factor("a", levels = "a"))
  expect_identical(refactor(factor(NA, levels = "a"), "a", na.rm = TRUE),
                   factor(c(), levels = "a"))
  expect_identical(refactor(factor(NA, levels = "a"), "b", na.rm = TRUE),
                   factor(c(), levels = "b"))
  expect_identical(refactor(factor(NA, levels = "a"), "a", na.rm = TRUE),
                   factor(c(), levels = "a"))
  expect_identical(refactor(factor(c("a", "b")), "a", na.rm = TRUE),
                   factor(c("a"), levels = "a"))
  expect_identical(refactor(factor(c("a", "b")), "b", na.rm = TRUE),
                   factor(c("b"), levels = "b"))
  expect_identical(refactor(factor(c("a", "b")), "c", na.rm = TRUE),
                   factor(c(), levels = "c"))
})

test_that("basic refactoring", {
  u <- c("a", "b", "c")
  v <- c("c", "d")
  w <- c(NA_character_, "a")
  x <- c("X", NA_character_)
  test_cases <- expand.grid(
    list(u, v, w, x),
    list(u, v, w, x),
    list(u, v, w, x))
  for (tc in seq_along(test_cases[[1]])) {
    m <- test_cases[tc, 1][[1]]
    n <- test_cases[tc, 2][[1]]
    p <- unique(test_cases[tc, 3][[1]])
    f <- factor(m, levels = p)
    expect_identical(
      refactor(f, n),
      factor(f, levels = n), # exclude NA by default, as factor does
      info = paste("m = c('", paste(unlist(m), collapse = "', '"), "')\n",
                   "n = c('", paste(unlist(n), collapse = "', '"), "')",
                   "p: ", paste(unlist(p), collapse = " "), sep = "")
    )
    expect_identical(
      refactor(f, n, na.rm = FALSE, exclude_na = FALSE),
      factor(f, levels = n, exclude = NULL),
      info = paste("m = c('", paste(unlist(m), collapse = "', '"), "')\n",
                   "n = c('", paste(unlist(n), collapse = "', '"), "')",
                   "p: ", paste(unlist(p), collapse = " "), sep = "")
    )
    if (!anyNA(f) && !anyNA(levels(f)))
      expect_identical(
        refactor(f, n, na.rm = FALSE),
        factor(f, levels = n),
        info = paste("m = c('", paste(unlist(m), collapse = "', '"), "')\n",
                     "n = c('", paste(unlist(n), collapse = "', '"), "')",
                     "p: ", paste(unlist(p), collapse = " "), sep = "")

      )
  }
})

test_that("new factor has empty levels when necessary", {
  f <- factor("a")
  expect_equal(
    refactor(f, levels = NA, na.rm = TRUE),
    factor())
  expect_equal(
    refactor(f, levels = NA, na.rm = FALSE, exclude_na = TRUE),
    factor(NA, levels = NULL))
  expect_equal(
    refactor(f, levels = NA, na.rm = FALSE, exclude_na = FALSE),
    factor(NA, exclude = NULL))
  for (narm in c(TRUE, FALSE)) {
    refactor(NA, NA, na.rm = narm)
    refactor(factor(NA), NA, na.rm = narm)
    refactor(factor(NA), "a", na.rm = narm)
    refactor(factor(NA, levels = NA), c("a", NA), na.rm = narm)
    refactor(factor(NA), c("a", NA), na.rm = narm)
    refactor(factor("a"), c("a", NA), na.rm = narm)
    refactor(factor("a"), NA, na.rm = narm)
  }
})

test_that("no crash with NA levels", {
  for (ena in c(TRUE, FALSE)) {
    for (narm in c(TRUE, FALSE)) {
      message("ena = ", ena, ", narm = ", narm)
      expect_error(
        refactor(factor(NA, NA, exclude = NULL),
                 NA, na.rm = narm, exclude_na = ena), #ok
        NA, info = paste(ifelse(narm, "na.rm", "!na.rm"),
                         ifelse(ena, "exclude_na", "!exclude_na")))
      expect_error(
        refactor(factor(NA, c("a", NA), exclude = NULL),
                 NA, na.rm = narm, exclude_na = ena), #ok
        NA, info = paste(ifelse(narm, "na.rm", "!na.rm"),
                         ifelse(ena, "exclude_na", "!exclude_na")))
      expect_error(
        refactor(factor(NA, NA, exclude = NULL),
                 "a", na.rm = narm, exclude_na = ena),
        NA, info = paste(ifelse(narm, "na.rm", "!na.rm"),
                         ifelse(ena, "exclude_na", "!exclude_na")))
      expect_error(
        refactor(factor(NA, c("a", NA), exclude = NULL),
                 "a", na.rm = narm, exclude_na = ena),
        NA, info = paste(ifelse(narm, "na.rm", "!na.rm"),
                         ifelse(ena, "exclude_na", "!exclude_na")))
      expect_error(
        refactor(factor(c("a", NA), c("a", NA), exclude = NULL),
                 "a", na.rm = narm, exclude_na = ena),
        NA, info = paste(ifelse(narm, "na.rm", "!na.rm"),
                         ifelse(ena, "exclude_na", "!exclude_na")))
    }
  }
})
