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

test_that("generating uranium data is identical to saved", {
  if (is.null(fetch_uranium_pathology(offline = TRUE)$file_path))
    skip_online_tests("uranium pathology data only available online")

  suppressWarnings(
    if (!require("RODBC") || !existsFunction("odbcConnectAccess2007"))
      skip("only test if RODBC::odbcConnectAccess2007 is available. (Probably just windows.)")
  )
  expect_identical(generate_uranium_pathology(save_data = FALSE), uranium_pathology)
})

test_that("generating vermont data is identical to saved", {
  if (is.null(fetch_vermont_dx(offline = TRUE)$file_path))
    skip_online_tests("vermont data only available online")
  expect_identical(generate_vermont_dx(save_data = FALSE), vermont_dx)
})

test_that("uranium data looks okay", {

  expect_is(uranium_pathology, c("icd_long_data", "icd10", "icd_decimal_code"))
  expect_equal(dim(uranium_pathology), c(2376, 2))
})

test_that("vermont data looks okay", {
  expect_is(vermont_dx, c("icd9cm", "icd9", "icd_wide_data", "icd_short_code", "data.frame"))
  expect_true(is.icd9cm(vermont_dx))
  expect_true(is.icd9(vermont_dx))
  expect_true(is.icd_wide_data(vermont_dx))
  expect_true(is.icd_short_code(vermont_dx))
  expect_equal(dim(vermont_dx), c(1000, 25))
})
