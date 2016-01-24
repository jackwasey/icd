# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

context("RTF ICD-9")

test_that("subchapter parsing went okay", {

  # some things shouldn't have been called sub-chapters
  expect_icd9_subchap_missing("Supplementary Classification Of External Causes Of Injury And Poisoning")

  expect_icd9_subchap_equal("Accidental Poisoning By Drugs, Medicinal Substances, And Biologicals", "E850", "E858")
  expect_icd9_subchap_equal("Accidental Poisoning By Other Solid And Liquid Substances, Gases, And Vapors", "E860", "E869")
  expect_icd9_subchap_equal("External Cause Status", "E000", "E000")
  expect_icd9_subchap_equal("Injury Resulting From Operations Of War", "E990", "E999")
  #expect_icd9_subchap_equal("", "", "")
})

test_that("majors okay", {
  # pick out some troublemakers found in testing, and some edge cases.
  expect_equal(icd9_majors[["Other poxvirus infections"]], "059")
  expect_equal(icd9_majors[["Other disorders of stomach and duodenum"]], "537")
  expect_equal(icd9_majors[["Gastrointestinal mucositis (ulcerative)"]], "538")
  expect_equal(icd9_majors[["Perinatal disorders of digestive system"]], "777")
})

test_that("some have no subchapter, just major:", {
  expect_false("280" %in% icd9_sub_chapters)
  expect_false("289" %in% icd9_sub_chapters)
  expect_false("740" %in% icd9_sub_chapters)
  expect_false("759" %in% icd9_sub_chapters)
  expect_equal(icd9_majors[["Iron deficiency anemias"]], "280")
  expect_equal(icd9_majors[["Other diseases of blood and blood-forming organs"]], "289")
  expect_equal(icd9_majors[["Anencephalus and similar anomalies"]], "740")
  expect_equal(icd9_majors[["Other and unspecified congenital anomalies"]], "759")
})
