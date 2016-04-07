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

context("filtering on ICD-10 validity")

pts <- icd10_all_ahrq
pts[1, "icd10_code"] <- "invalid"

test_that("filter an invalid row", {
  expect_warning(res <- icd10_filter_invalid(pts), regexp = NA)
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 1)
})
