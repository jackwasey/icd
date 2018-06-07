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

context("build icd9 maps")

test_that("AHRQ children same as saved", {
  for (i in icd9_map_ahrq)
    expect_equal(children.icd9(i, defined = FALSE, short_code = TRUE), sort_icd.icd9(i))
})

test_that("Elixhauser children same as saved", {
  for (i in icd9_map_quan_elix)
    expect_equal(children.icd9(i, defined = FALSE, short_code = TRUE), sort_icd.icd9(i))
})

test_that("Quan Charlson children same as saved", {
  for (i in icd9_map_quan_deyo)
    expect_equal_no_class_order(children.icd9(i, defined = FALSE, short_code = TRUE), sort_icd.icd9(i))
})

test_that("Quan Elixhauser children same as saved", {
  for (i in icd9_map_quan_elix)
    expect_equal_no_class_order(children.icd9(i, defined = FALSE, short_code = TRUE), sort_icd.icd9(i))
})


test_that("ahrq icd9 map recreated", {
  # skip this test if the file is not already in data-raw
  if (is.null(icd9_fetch_ahrq_sas(offline = TRUE)))
    skip("data-raw/comformat2012-2013.txt must be downloaded with icd9_fetch_ahrq_sas")
  # same but from source data. Should be absolutely identical.
  expect_identical(result <- icd9_parse_ahrq_sas(save_data = FALSE), icd9_map_ahrq)
  checkmate::expect_list(result, len = 30)
  expect_equivalent(get_invalid.comorbidity_map(icd9_map_ahrq), list())
})

test_that("Quan Charlson icd9 map generated = saved", {
  if (is.null(icd9_fetch_quan_deyo_sas(offline = TRUE)))
    skip("data-raw/ICD9_E_Charlson.sas must be downloaded with icd9_fetch_quan_deyo_sas")
  expect_equivalent(icd9_map_quan_deyo, icd9_parse_quan_deyo_sas(save_data = FALSE))
  expect_equivalent(
    get_invalid.comorbidity_map(icd9_map_quan_deyo, short_code = TRUE),
    list())
})

test_that("Quan Elix icd9 map generated = saved", {
  expect_equivalent(icd9_map_quan_elix, icd9_generate_map_quan_elix(save_data = FALSE))
  expect_equivalent(
    get_invalid.comorbidity_map(icd9_map_quan_elix, short_code = TRUE),
    list())
})

test_that("Elixhauser icd9 map generated = saved", {
  expect_equivalent(icd9_map_elix, icd9_generate_map_elix(save_data = FALSE))
  expect_equivalent(get_invalid.comorbidity_map(icd9_map_elix, short_code = TRUE), list())
})

test_that("Elixhauser icd10 map generated = saved", {
  expect_equivalent(icd10_map_elix, icd10_generate_map_elix(save_data = FALSE))
  expect_equivalent(get_invalid.comorbidity_map(icd10_map_elix, short_code = TRUE), list())
})


test_that("icd9cm_hierarchy as saved in data can be recreated as expected", {
  # avoid encoding problems by just doing this on Linux.
  skip_on_os(c("windows", "mac", "solaris"))
  skip_flat_icd9_avail_all()
  skip_on_no_rtf("2011")
  cmh_headings <- c("code", "short_desc", "long_desc", "three_digit",
                    "major", "sub_chapter", "chapter")
  cmh <- icd9cm_generate_chapters_hierarchy(save_data = FALSE, verbose = FALSE, offline = TRUE)
  for (h in cmh_headings)
    expect_equal(cmh[[h]], icd9cm_hierarchy[[h]], info = paste("working on :", h))
})
