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

context("billable code lists")

test_that("specific known parsing errors", {
  # problems with whitespace stripping or sorting around: 12167 13276 14567
  b32 <- icd::icd9cm_billable[["32"]]
  nines <- b32[b32$code == "9999", ]
  expect_equal(nrow(nines), 1)
  expect_equal(nines$long_desc, "Other and unspecified complications of medical care, not elsewhere classified")
})

test_that("ICD-9-CM billable codes package data is recreated", {

  skip_on_os(c("windows", "mac", "solaris"))
  # Do encoding problems on Linux. It is unpredictable at the best of times.

  # we can do this offline if we have all (currently available) versions of the
  # ICD-9-CM code, otherwise we may have to skip
  skip_flat_icd9_all_avail()

  check_billable <- parse_leaf_descriptions_all(save_data = FALSE, offline = TRUE)
  # check this one thing known to be dodgy
  expect_identical(check_billable[["28"]][["long_desc"]], icd::icd9cm_billable[["28"]][["long_desc"]])

  # make specific quick tests for previously known problems:
  b32 <- check_billable[["32"]]
  expect_identical(b32[b32$code == "9999", "short_desc"], "Complic med care NEC/NOS")
  expect_identical(b32[b32$code == "E0000", "short_desc"], "Civilian activity-income")
  expect_identical(b32[b32$code == "E9991", "short_desc"], "Late effect, terrorism")
  expect_identical(b32[b32$code == "V9199", "short_desc"], "Mult gest-plac/sac undet")

  for (ver in c("27", "28", "29", "30", "31", "32")) {
    v <- icd::icd9cm_billable[[ver]][["long_desc"]]
    cb <- check_billable[[ver]][["long_desc"]]
    diff <- v != cb
    expect_identical(check_billable[[ver]], icd::icd9cm_billable[[ver]],
                     info = paste("long_desc differences for version", ver,
                                  "\noriginal: ", paste(head(v[diff]), collapse = ", "),
                                  "\nprocess:", paste(head(cb[diff]), collapse = ", ")
                     ))
  }
})

test_that("billable codes for expected versions exist", {
  expect_true(all(as.character(23:32) %in% names(icd9cm_billable)))
  expect_true(all(sapply(icd9cm_billable, is.data.frame)))
})

test_that("billable codes are all in order", {
  skip_slow_tests()
  for (v in names(icd9cm_billable)) {
    i <- icd::icd9cm_billable[[v]][["code"]]
    expect_identical(i, icd_sort.icd9(i, short_code = TRUE),
                     info = paste("version = ", v))
  }
})

test_that("parsing 27 gives zero-padded digit icd9 codes", {
  expect_equal(icd9cm_billable[["27"]][1, "code"], "0010")
})
