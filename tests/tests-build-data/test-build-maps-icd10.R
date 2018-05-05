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

context("build icd10 maps")

test_that("the icd-10 quan elix comorbidity map is reproduced", {
  expect_equivalent(icd10_map_quan_elix, icd10_generate_map_quan_elix(save_data = FALSE))
})

test_that("the icd-10 quan deyo comorbidity map is reproduced", {
  expect_equivalent(icd10_map_quan_deyo, icd10_generate_map_quan_deyo(save_data = FALSE))
})

test_that("the icd-10 elix comorbidity map is reproduced", {
  expect_equivalent(icd10_map_elix, icd10_generate_map_elix(save_data = FALSE))
})

test_that("icd-10 ahrq map is reproduced", {
  if (is.null(icd10_fetch_ahrq_sas(offline = TRUE)))
    skip("AHRQ ICD-10 SAS must be downloaded with icd10_fetch_ahrq_sas")
  expect_equivalent(icd10_map_ahrq, icd10_parse_ahrq_sas(save_data = FALSE))
})

