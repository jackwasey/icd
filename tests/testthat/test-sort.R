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

context("sorting")

test_that("sorting preserves order of names", {
  a <- c("Bad disease" = "100", "Another bad disease" = "200")
  b <- sort_icd.icd9(a)
  expect_identical(a[["Bad disease"]], b[["Bad disease"]])
  expect_identical(a[["Another bad disease"]], b[["Another bad disease"]])
})

test_that("sorting of icd9 object", {
  j <- icd9(c("Bad disease" = "500", "Another bad disease" = "400"))
  k <- sort_icd(j)
  expect_identical(j[2], k[1])
  expect_identical(j[1], k[2])
})

test_that("sorting of icd10 object", {
  a <- c("Bad disease" = "I119", "Another bad disease" = "I110")
  j <- icd10cm(a)
  k <- sort_icd(j)
  expect_identical(j[2], k[1])
  expect_identical(j[1], k[2])
  expect_identical(sort_icd(a), unclass(k))
})

test_that("warn if NA when ordering ICD-9 codes", {
  expect_warning(icd9_order_short(c("a", NA)))
  expect_identical(
    expect_warning(icd9_order_short(NA)), character())
})
