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

context("test hcc")

# Sample datasets for HCC tests
# 4 patients, some with ICDs that do not exist in CC crosswalk
# One of the patients with multiple visit dates, all valid ICDs
hcc_test_simple <- icd_long_data(
  visit_id = c("1", "2", "3", "4", "4"),
  icd_code = c("20084", "1742", "30410", "41514", "95893"),
  date = as.Date(c("2011-01-01", "2011-01-02", "2011-01-03",
                   "2011-01-04", "2011-01-04")))

# Only one record
hcc_test_single <- icd_long_data(
  visit = c("1"),
  icd_code = c("20084"),
  date = as.Date(c("2011-01-01")))

# Mix of valid and invalid ICD Codes
hcc_test_invalid <- icd_long_data(
  patient = c("1", "2", "3", "4", "4"),
  icd_code = c("20084", "174242", "aB30410", "41514", "95893"),
  date = as.Date(c("2011-01-01", "2011-01-02", "2011-01-03",
                   "2011-01-04", "2011-01-04")))

# Typical use case
# Data as expected. Multiple visits (different patients encounters)
# One of the patients with multiple visit dates, all valid ICDs
# Returns a unique HCC for each visit/date combination
# Only returns matches for valid ICDs in CC crosswalk
test_that("hcc mapping is applied correctly,
          4 patients on different dates, each should have a single
          HCC assigned", {
            res <- icd_comorbid_hcc(hcc_test_simple)
            expect_equal(dim(res), c(4, 3))
            expect_true(setequal(c("visit_id", "date", "hcc"), names(res)))
})

# Data as expected but only a single record
test_that("hcc mapping is applied correctly, one patient, single visit
          should have a single HCC assigned", {
            res <- icd_comorbid_hcc(hcc_test_single)
            expect_equal(dim(res), c(1, 3))
            expect_true(setequal(c("visit", "date", "hcc"), names(res)))
})

# Mix of valid and invalid ICDs, some patients dont have any valid ICDs
# Only returns matches for valid ICDs in CC crosswalk
# should return 2 rows, 2 different patients
test_that("hcc mapping is applied correctly, results in 2 pt/visit combos
          each should have a single HCC assigned", {
            res <- icd_comorbid_hcc(hcc_test_invalid)
            expect_equal(dim(res), c(2, 3))
            expect_true(setequal(c("patient", "date", "hcc"), names(res)))
})
