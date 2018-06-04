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

test_that("basic refactoring", {
  u = c("a", "b", "c")
  v = c("c", "d")
  w = c(NA_character_, "a")
  x = c("X", NA_character_)
  test_cases <- expand.grid(
    list(u, v, w, x),
    list(u, v, w, x),
    list(u, v, w, x))
  for (tc in seq_along(test_cases[[1]])) {
    m <- test_cases[tc, 1][[1]]
    n <- test_cases[tc, 2][[1]]
    p <- unique(test_cases[tc, 3][[1]])
    if (FALSE) print(paste("x: ", paste(unlist(m), collapse = " "),
                           "old levels: ", paste(unlist(p), collapse = " "),
                           "new levels: ", paste(unlist(n), collapse = " ")))
    expect_identical(
      refactor(factor(m, levels = p), n),
      factor(factor(m, levels = p), levels = n), # exclude NA by default, as factor does
      info = paste("m = c('", paste(unlist(test_cases[tc, 1]), collapse = "', '"), "')\n",
                   "n = c('", paste(unlist(test_cases[tc, 2]), collapse = "', '"), "')", sep = "")
    )
    skip("for as yet unimplemented code path")
    expect_identical(
      refactor(factor(m, levels = p), n, na.rm = FALSE, exclude_na = FALSE),
      factor(factor(m, levels = p), levels = n, exclude = NULL),
      info = paste("m = c('", paste(unlist(test_cases[tc, 1]), collapse = "', '"), "')\n",
                   "n = c('", paste(unlist(test_cases[tc, 2]), collapse = "', '"), "')", sep = "")
    )
  }
})
