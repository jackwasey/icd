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

test_that("uranium data looks okay", {
  expect_is(uranium_pathology, c("icd_long_data", "icd10", "icd_decimal_diag"))
  expect_equal(dim(uranium_pathology), c(2376, 2))
})

test_that("vermont data looks okay", {
  expect_true(is.icd_wide_data(vermont_dx))
  expect_equal(dim(vermont_dx), c(1000, 25))
})
