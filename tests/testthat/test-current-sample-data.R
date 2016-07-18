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
    skip("uranium pathology data must be downloaded with fetch_uranium_pathology")

  suppressWarnings(
    if (!require("RODBC") || !existsFunction("odbcConnectAccess2007"))
      skip("only test if RODBC::odbcConnectAccess2007 is available. (Probably just windows.)")
  )
  expect_identical(generate_uranium_pathology(save_data = FALSE), uranium_pathology)
})

test_that("ICD-10 codes in uranium data are okay", {
  skip("reinstate this test once ICD-10 WHO codes are available for comparison.
       Uranium Pathology data is not ICD-10-CM, but ICD-10 WHO.")
  expect_true(
    all(strip(uranium_pathology$icd10, ".") %in% icd10cm2016$code)
  )
  # codes missing from RHS:
  setdiff(uranium_pathology$icd10  %>%  strip("."), icd10cm2016$code)

  # http://apps.who.int/classifications/icd10/browse/2015/en#!/Y86

})

test_that("generating vermont data is identical to saved", {
  if (is.null(fetch_vermont_dx(offline = TRUE)$file_path))
    skip("vermont data must be downloaded with fetch_vermont_dx")
  expect_identical(generate_vermont_dx(save_data = FALSE), vermont_dx)
})

test_that("uranium data looks okay", {

  expect_is(uranium_pathology, c("icd_long_data", "icd10", "icd_decimal_diag"))
  expect_equal(dim(uranium_pathology), c(2376, 2))
})

test_that("vermont data looks okay", {
  expect_true(is.icd_wide_data(vermont_dx))
  expect_equal(dim(vermont_dx), c(1000, 25))
})
