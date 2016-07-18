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

context("deprecated compare ordered long to wide methods")

pts <- generate_random_ordered_pts(5000, 13)
pts$visitId <- pts$visit_id

test_that("deprecated - ordered and unordered methods on ordered data are identical", {
  agg <- icd9LongToWide(pts, aggregate = TRUE)
  ord <- icd9LongToWide(pts, aggregate = FALSE)
  expect_identical(ord, agg)
  expect_true(all(rownames(ord) %in% pts$visitId))
  expect_true(all(rownames(agg) %in% pts$visitId))
  expect_true(all(pts$visitId %in% rownames(ord)))
  expect_true(all(pts$visitId %in% rownames(agg)))
})
