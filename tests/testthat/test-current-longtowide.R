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

context("compare ordered long to wide methods")

pts <- generate_random_ordered_pts(5000, 13)

test_that("ordered and unordered methods on ordered data are identical", {
  agg <- icd_long_to_wide(pts, aggr = TRUE)
  ord <- icd_long_to_wide(pts, aggr = FALSE)
  expect_identical(ord, agg)
  expect_true(all(rownames(ord) %in% pts$visit_id))
  expect_true(all(rownames(agg) %in% pts$visit_id))
  expect_true(all(pts$visit_id %in% rownames(ord)))
  expect_true(all(pts$visit_id %in% rownames(agg)))
})

test_that("cpp: failure with non character visitId", {
  expect_error(icd_long_to_wide_cpp(pts, visitId = "visit_id", icd9Field = "code"), "converted to")
  # and make sure it works otherwise
  pts$visit_id <- as_char_no_warn(pts$visit_id)
  res <- icd_long_to_wide_cpp(pts, visitId = "visit_id", icd9Field = "code")
})
