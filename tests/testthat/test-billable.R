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

context("billable code lists")

test_that("specific known parsing errors", {
  # problems with whitespace stripping or sorting around: 12167 13276 14567
  b32 <- icd9cm_billable[["32"]]
  nines <- b32[b32$code == "9999", ]
  expect_equal(nrow(nines), 1)
  expect_equal(nines$long_desc, "Other and unspecified complications of medical care, not elsewhere classified") # nolint
})

test_that("billable codes for expected versions exist", {
  expect_true(all(as.character(23:32) %in% names(icd9cm_billable)))
  expect_true(all(vapply(icd9cm_billable, is.data.frame, logical(1))))
})

test_that("billable codes are all in order", {
  testthat::skip_on_cran()
  for (v in names(icd9cm_billable)) {
    i <- icd9cm_billable[[v]][["code"]]
    expect_identical(i, sort_icd.icd9(i, short_code = TRUE),
                     info = paste("version = ", v))
  }
})

test_that("parsing 27 gives zero-padded digit icd9 codes", {
  expect_equal(icd9cm_billable[["27"]][1, "code"], "0010")
})
