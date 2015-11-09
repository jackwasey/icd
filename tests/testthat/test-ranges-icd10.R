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

context("icd10 ranges")

test_that("top level ICD-10 ranges between different alphabetic groups are not valid", {
  # is this valid? injury codes, for example, go through S and T.
  expect_error(icd10ExpandRange("A10", "B20"))
  expect_error(icd10ExpandRange("C99", "B00"))
  expect_error(icd10ExpandRange("G99.x", "H01"))
  expect_error(icd10ExpandRange("G99", "H01.x"))
  expect_error(icd10ExpandRange("G99.x", "H01.x"))
})

test_that("top level ICD-10 ranges of single code are expanded to real codes", {
  expect_equals(icd10ExpandRange("J11", "J11", onlyReal = TRUE), )
})
