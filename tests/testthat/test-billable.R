# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

context("billable code lists")

test_that("ICD-9-CM billable codes package data is recreated", {

  if (Sys.info()[["sysname"]] != "Linux")
    skip("Only do encoding problems on Linux.")

  skip_slow_tests() # > 30 seconds

  # we can do this offline if we have all (currently available) versions of the
  # ICD-9-CM code, otherwise we may have to skip
  lapply(icd9_sources$version, skip_flat_icd9_avail)

  check_billable <- parse_leaf_descriptions_all(save_data = FALSE)

  for (ver in c("27", "28", "29", "30", "31", "32")) {
    v <- icd::icd9cm_billable[[ver]][["descLong"]]
    cb <- check_billable[[ver]][["descLong"]]
    diff <- v != cb
    expect_identical(check_billable[[ver]], icd::icd9cm_billable[[ver]],
                     info = paste("descLong differences for version", ver,
                                  "\noriginal: ", paste(v[diff], collapse = ", "),
                                  "\nprocess:", paste(cb[diff], collapse = ", ")
                     ))
  }
})

test_that("billable codes for expected versions exist", {
  expect_true(all(as.character(23:32) %in% names(icd9cm_billable)))
  expect_true(all(sapply(icd9cm_billable, is.data.frame)))
})

test_that("billable codes are all in order", {
  for (v in names(icd9cm_billable)) {
    i <- icd::icd9cm_billable[[v]][["icd9"]]
    expect_identical(i, icd_sort.icd9(i, short_code = TRUE),
                     info = paste("version = ", v))
  }
})

test_that("parsing 27 gives zero-padded digit icd9 codes", {
  expect_equal(icd9cm_billable[["27"]][1, "icd9"], "0010")
})
