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
  expect_true(all(is_valid(uranium_pathology$icd10, short_code = FALSE)))
  skip("reinstate this test once ICD-10 WHO codes are available for comparison.
       Uranium Pathology data is not ICD-10-CM, but ICD-10 WHO.")
  expect_true(
    all(strip(uranium_pathology$icd10, ".") %in% icd10cm2016$code)
  )
  # See codes missing from RHS. e.g. https://www.sdrugs.com/?c=icd&s=x67 exists
  # in ICD-10 international, but not in ICD-10-CM
  setdiff(uranium_pathology$icd10  %>%  strip("."), icd10cm2016$code)

  # http://apps.who.int/classifications/icd10/browse/2015/en#!/Y86

})

test_that("sample data frames have correct class", {
  expect_true(is.icd_long_data(uranium_pathology))
  expect_true(is.icd_wide_data(vermont_dx))

  # should not have vector classes on the data frames themselves
  expect_false(any(c("icd9", "icd9cm", "icd9who") %in% class(vermont_dx)))
  expect_false(any(c("icd10", "icd10cm", "icd10who") %in% class(uranium_pathology)))
})

test_that("uranium data looks okay", {
  # generating uranium data depends on RODBC and an access database source file,
  # so don't do this.
  expect_equal(dim(uranium_pathology), c(2376, 2))
  expect_true(is.decimal_diag(uranium_pathology$icd10))
  expect_true(is.icd10(uranium_pathology$icd10))
})

test_that("vermont data looks okay", {
  expect_true(is.icd_wide_data(vermont_dx))
  expect_equal(dim(vermont_dx), c(1000, 25))

  expect_true(
    all(
      is_valid.icd9(
        wide_to_long(vermont_dx)$icd_code)))

  expect_true(
    all(
      wide_to_long(vermont_dx)$icd_code %in% icd9cm_hierarchy$code))

})
