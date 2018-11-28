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

context("reshaping wide to long")

longdf <- data.frame(visit_id = c("a", "b", "b", "c"),
                     icd9 = c("441", "4424", "443", "441"))

widedf <- data.frame(visit_id = c("a", "b", "c"),
                     icd9_001 = c("441", "4424", "441"),
                     icd9_002 = c(NA, "443", NA))

test_that("wide data to long data", {
  expect_equivalent(icd_wide_to_long(widedf),
                    longdf)

  widedfempty <- data.frame(visit_id = c("a", "b", "c"),
                            icd9_001 = c("441", "4424", "441"),
                            icd9_002 = c("", "443", ""))

  expect_equivalent(icd_wide_to_long(widedfempty),
                    longdf)
  expect_equal(icd_wide_to_long(widedfempty),
               icd_wide_to_long(widedfempty))

})

test_that("matrix to data frame and back", {
  for (pts in list(random_test_patients, test_twenty, multi_comorbid)) {
    mat <- icd_comorbid_ahrq(pts)
    df <- icd_comorbid_mat_to_df(mat)
    expect_identical(icd_comorbid_df_to_mat(df), mat)
  }
})

test_that("dataframe to matrix and back", {
  for (pts in list(random_test_patients, test_twenty, multi_comorbid)) {
    df2 <- icd_comorbid_ahrq(pts, return_df = TRUE)
    mat2 <- icd_comorbid_df_to_mat(df2)
    df3 <- icd_comorbid_mat_to_df(mat2, visit_name = "visit_id",
                                  stringsAsFactors = FALSE)
    expect_identical(df2, df3)
  }
})
