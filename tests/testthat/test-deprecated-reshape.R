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

context("deprecated reshaping wide to long")

longdf <- data.frame(visitId = c("a", "b", "b", "c"),
                     icd9 = c("441", "4424", "443", "441"))

widedf <- data.frame(visitId = c("a", "b", "c"),
                     icd9_001 = c("441", "4424", "441"),
                     icd9_002 = c(NA, "443", NA))

test_that("deprecated - long data to wide data", {
  longcmp <- data.frame(visitId = c("a", "b", "c"),
                        icd_001 = c("441", "4424", "441"),
                        icd_002 = c(NA, "443", NA))
  expect_equal(icd9LongToWide(longdf, return.df = TRUE), longcmp)

  longcmp2 <- data.frame(visitId = c("a", "b", "c"),
                         icd_001 = c("441", "4424", "441"),
                         icd_002 = c(NA, "443", NA),
                         icd_003 = c(NA, NA, NA))
  expect_equal(icd9LongToWide(longdf, min.width = 3, return.df = TRUE), longcmp2)


  longdf2 <- data.frame(i = c("441", "4424", "443", "441"),
                        v = c("a", "b", "b", "c"))
  expect_equal(names(icd9LongToWide(longdf2,
                                    visitId = "v",
                                    icd9Field = "i",
                                    prefix = "ICD10_", return.df = TRUE)),
               c("v", "ICD10_001", "ICD10_002"))
})

test_that("deprecated - wide data to long data", {
  expect_equivalent(icd9WideToLong(widedf), longdf)

  widedfempty <- data.frame(visitId = c("a", "b", "c"),
                            icd9_001 = c("441", "4424", "441"),
                            icd9_002 = c("", "443", ""))

  expect_equivalent(icd9WideToLong(widedfempty), longdf)
  expect_equal(icd9WideToLong(widedfempty), icd9WideToLong(widedfempty))

})

test_that("matrix to data frame and back", {
  for (pts in list(random_test_patients, test_twenty, multi_comorbid)) {
    mat <- icd_comorbid_ahrq(pts)
    df <- icd9ComorbidMatToDf(mat)
    expect_identical(icd9ComorbidDfToMat(df), mat)
  }
})

test_that("dataframe to matrix and back", {
  for (pts in list(random_test_patients, test_twenty, multi_comorbid)) {
    df2 <- icd_comorbid_ahrq(pts, return_df = TRUE)
    mat2 <- icd9ComorbidDfToMat(df2)
    df3 <- icd9ComorbidMatToDf(mat2, visitId = "visit_id", stringsAsFactors = FALSE)
    expect_identical(df2, df3)
  }
})
