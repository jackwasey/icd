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

context("RTF ICD-9")

test_that("some known sub vs chap confusion", {
  # some things shouldn't have been called sub-chapters, just chapters. Known troublemakers:
  expect_icd9_only_chap("Supplementary Classification Of External Causes Of Injury And Poisoning")
  expect_icd9_only_chap("Supplementary Classification Of Factors Influencing Health Status And Contact With Health Services") # nolint

  # and scan all, noting each is tested twice and half the test is circular,
  # since it looks up with the looped group.
  for (i in names(icd9_chapters))
    expect_icd9_only_chap(i)

  for (i in names(icd9_sub_chapters))
    expect_icd9_only_sub_chap(i, info = i)

})

test_that("sub_chapter parsing went okay, tricky cases", {

  # "TUBERCULOSIS(010-018)" # nolint
  expect_icd9_sub_chap_equal("Tuberculosis", "010", "018")

  # or with comma: "Vehicle Accidents, Not Elsewhere Classifiable"
  expect_icd9_sub_chap_equal("Vehicle Accidents Not Elsewhere Classifiable", "E846", "E848")

  expect_icd9_sub_chap_equal("Accidental Poisoning By Drugs, Medicinal Substances, And Biologicals", "E850", "E858") # nolint
  expect_icd9_sub_chap_equal("Accidental Poisoning By Other Solid And Liquid Substances, Gases, And Vapors", "E860", "E869") # nolint
  expect_icd9_sub_chap_equal("External Cause Status", "E000", "E000")
  expect_icd9_sub_chap_equal("Injury Resulting From Operations Of War", "E990", "E999")
})

test_that("majors okay", {
  # pick out some troublemakers found in testing, and some edge cases.
  expect_icd9_major_equals("Other respiratory tuberculosis", "012")
  expect_icd9_major_equals("Other poxvirus infections", "059")
  expect_icd9_major_equals("Other disorders of stomach and duodenum", "537")
  expect_icd9_major_equals("Gastrointestinal mucositis (ulcerative)", "538")
  expect_icd9_major_equals("Perinatal disorders of digestive system", "777")
  expect_icd9_major_equals("Iron deficiency anemias", "280")
  expect_icd9_major_equals("Other diseases of blood and blood-forming organs", "289")
  expect_icd9_major_equals("Anencephalus and similar anomalies", "740")
  expect_icd9_major_equals("Other and unspecified congenital anomalies", "759")

  # the following is incorrectly specified under vehicle injury in
  # http://www.icd9data.com/2015/Volume1/E000-E999/E846-E849/E849/default.htm
  expect_icd9_major_equals("Place of occurrence", "E849")
})

test_that("some majors are the same as sub-chapters", {
  # majors and sub-chapters cannot overlap, and be the same thing? I think this
  # is right. When we consider 'major' codes, we expect all three-digit (four
  # for E) codes to be present, even if this is also a sub-chapter. And it would
  # be more consistent to have every code being in a chapter, sub-chapter and
  # major, than some being exceptional.


  # TODO: V85 to V91

  expect_icd9_major_is_sub_chap("External cause status", "E000")

})
