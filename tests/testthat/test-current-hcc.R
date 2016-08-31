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

context("test hcc")

# Typical use case
# Data as expected. Multiple visit_names (different patients)
# One of the patients with multiple visit dates, all valid ICDs
# Returns a unique HCC for each visit/date combination
# Only returns matches for valid ICDs in CC crosswalk
test_that("hcc mapping is applied correctly,
          4 patients on different dates, each should have a single
          HCC assigned", {
            res <- icd_comorbid_hcc(hcc_test_simple)
            expect_equal(dim(res), c(4, 3))
            expect_true(setequal(c("visit_name", "date", "hcc"), names(res)))
})

# Data as expected but only a single record
test_that("hcc mapping is applied correctly, one patient, single visit
          should have a single HCC assigned", {
            res <- icd_comorbid_hcc(hcc_test_single)
            expect_equal(dim(res), c(1, 3))
            expect_true(setequal(c("visit_name", "date", "hcc"), names(res)))
})

# Mix of valid and invalid ICDs, some patients dont have any valid ICDs
# Only returns matches for valid ICDs in CC crosswalk
# should return 2 rows, 2 different patients
test_that("hcc mapping is applied correctly, results in 2 pt/visit combos
          each should have a single HCC assigned", {
            res <- icd_comorbid_hcc(hcc_test_invalid)
            expect_equal(dim(res), c(2, 3))
            expect_true(setequal(c("visit_name", "date", "hcc"), names(res)))
})
