# Copyright (C) 2014 - 2015  Jack O. Wasey
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

test_that("billable codes are recreated", {
  # this costs about 30 seconds
  skip_online_tests()
  check_billable <- parseLeafDescriptionsAll(save = FALSE, fromWeb = TRUE)
  if (Sys.info()[["sysname"]] != "Linux")
    skip("Only do encoding problems on Linux.")
  for (ver in c("27", "28", "29", "30", "31", "32")) {
    v <- icd9::icd9Billable[[ver]][["descLong"]]
    cb <- check_billable[[ver]][["descLong"]]
    diff <- v != cb
    expect_identical(check_billable[[ver]], icd9::icd9Billable[[ver]],
                     info = paste("descLong differences for version", ver,
                                  "\noriginal: ", paste(v[diff], collapse = ", "),
                                  "\nprocess:", paste(cb[diff], collapse = ", ")
                     ))
  }
})

test_that("billable codes for expected versions exist", {
  expect_true(all(as.character(23:32) %in% names(icd9Billable)))
  expect_true(all(sapply(icd9Billable, is.data.frame)))
})

test_that("billable codes are all in order", {
  skip_on_cran()
  for (v in names(icd9Billable)) {
    icd9 <- icd9::icd9Billable[[v]][["icd9"]]
    expect_identical(icd9, icd9SortShort(icd9),
                     info = paste("version = ", v))
  }
})

test_that("parsing 27 gives zero-padded digit icd9 codes", {
  expect_equal(icd9Billable[["27"]][1, "icd9"], "0010")
})
