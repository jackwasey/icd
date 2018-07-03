1# Copyright (C) 2014 - 2018  Jack O. Wasey
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

context("PCCC")

pccc_col_names <- c("neuromusc", "cvd", "respiratory", "renal", "gi",
                    "hemato_immu", "metabolic", "congeni_genetic",
                    "malignancy", "neonatal", "tech_dep", "transplant")

# Six random codes from each PCCC procedure code map. 'icd' will use
# an heuristic to guess whether ICD-9 or ICD-10:
pccc_pts <- data.frame(encounters = c(10, 11, 12),
                       icd9_dx = c("31800", "41610", "27701"),
                       icd10_dx = c("K50", "B20", "E702"),
                       icd9_pcs = c("0152", "304", "0050"),
                       icd10_pcs = c("0B110Z4", "02YA0Z2", "031209D"))

test_that("procedure codes work", {
  res9 <- comorbid_pccc_pcs(pccc_pts, icd_name = "icd9_pcs", return_binary = FALSE)
  expect_true(res9[1, "neuromusc"])
  expect_true(res9[3, "cvd"])
  expect_true(res9["11", "respiratory"])
  expect_true(res9[3, "tech_dep"])
  expect_equal(sum(res9), 4)
  res0 <- comorbid_pccc_pcs(pccc_pts, icd_name = "icd10_pcs", return_binary = FALSE)
  expect_true(res0["11", "cvd"])
  expect_true(res0["10", "respiratory"])
  expect_true(res0[3, 11])
  expect_true(res0[3, 4])
  expect_true(res0[2, "transplant"])
  expect_equal(sum(res0), 5)

  # All ICD-9 procedure codes are numeric, some ICD-10 procedure codes
  # are numeric, so best to call functions directly:
  pts <- data.frame(encounters = c(100), icd10_pcs = c("0016070"))
  icd10_comorbid_pccc_pcs(pts, icd_name = "icd10_pcs")
})

test_that("PCCC dx works", {
  res <- icd9_comorbid_pccc_dx(wide_to_long(vermont_dx), return_binary = TRUE)
  expect_equivalent(
    colSums(res), c(82, 270, 50, 119, 55, 39, 313, 30, 128, 7, 129, 21))
  expect_equal(colnames(res), pccc_col_names)
})

test_that("colnames same for both dx and procedure codes", {
  res9d <- comorbid_pccc_dx(pccc_pts, icd_name = "icd9_dx")
  res0d <- comorbid_pccc_dx(pccc_pts, icd_name = "icd10_dx")
  res9p <- comorbid_pccc_pcs(pccc_pts, icd_name = "icd9_pcs")
  res0p <- comorbid_pccc_pcs(pccc_pts, icd_name = "icd10_pcs")
  expect_identical(names(res9d), names(res9p))
  expect_identical(names(res9d), names(res0d))
  expect_identical(names(res9d), names(res0p))

  res9dd <- icd9_comorbid_pccc_dx(pccc_pts, icd_name = "icd9_dx")
  res0dd <- icd10_comorbid_pccc_dx(pccc_pts, icd_name = "icd10_dx")
  res9pp <- icd9_comorbid_pccc_pcs(pccc_pts, icd_name = "icd9_pcs")
  res0pp <- icd10_comorbid_pccc_pcs(pccc_pts, icd_name = "icd10_pcs")
  expect_identical(names(res9d), names(res9dd))
  expect_identical(names(res9d), names(res9pp))
  expect_identical(names(res9d), names(res0dd))
  expect_identical(names(res9d), names(res0pp))
})
