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

context("deprecated billable code lists")

test_that("billable codes are recreated", {
  # can only do the test if we have all the flat files downloaded. TODO: on at
  # least one of travis and wercker, downloading on the fly didn't seem to work
  # from R. Someday may write function to download all online dependencies, and
  # then travis or wercker could use this each time (whereas the function would
  # only be needed occasionally for local testing)
  skip_flat_icd9_all_avail()
  # TODO: run test if file is available, even if running in offline
  check_billable <- parse_leaf_descriptions_all(save_data = FALSE, offline = TRUE)
  skip_on_os(c("windows", "mac", "solaris"))
  for (ver in c("27", "28", "29", "30", "31", "32")) {
    names(check_billable[[ver]]) <- c("icd9", "descShort", "descLong")
    # v <- icd::icd9Billable[[ver]][["descLong"]]
    # cb <- check_billable[[ver]][["descLong"]]
    expect_identical(check_billable[[ver]],
                     icd::icd9Billable[[ver]],
                     info = paste("descLong version:", ver))
  }
})

test_that("billable codes for expected versions exist", {
  expect_true(all(as.character(23:32) %in% names(icd9Billable)))
  expect_true(all(sapply(icd9Billable, is.data.frame)))
})

test_that("billable codes are all in order", {
  skip_on_cran()
  skip_slow_tests()
  for (v in names(icd9Billable)) {
    icd9 <- icd::icd9Billable[[v]][["icd9"]]
    expect_identical(icd9, icd9SortShort(icd9),
                     info = paste("version = ", v))
  }
})

test_that("parsing 27 gives zero-padded digit icd9 codes", {
  expect_equal(icd9Billable[["27"]][1, "icd9"], "0010")
})
