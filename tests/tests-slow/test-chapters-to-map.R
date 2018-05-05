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

context("slow tests")

test_that("convert icd-9 ranges", {
  ooe <- icd_long_data(visit_id = sprintf("pt%02d", seq_along(one_of_each)),
                       code = one_of_each,
                       stringsAsFactors = TRUE)

  class(ooe[["code"]]) <- c("icd9", "icd_decimal_diag", "factor")

  expect_warning(
    test_map <- icd9_chapters_to_map(icd9_chapters), regexp = NA)
  expect_warning(
    cmb <- icd9_comorbid(x = ooe, short_code = FALSE, map = test_map,
                         short_map = TRUE, return_df = TRUE), regexp = NA)
  cmbcmp <- unname(as.matrix(logical_to_binary(cmb)[-1]))
  expmat <- diag(nrow = length(ooe$code))
  expect_equivalent(cmbcmp, expmat)
})
