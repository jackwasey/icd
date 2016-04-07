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

context("ICD-10 comorbidity calculations")
# since most of the code is common to ICD-9 and ICD-10, this doesn't need to be
# very extensive.

test_that("ICD-10 comorbidities from uranium are calculated without warnings or errors", {
  skip_slow_tests("icd10 comorbidity calcs are unfortunatley slow")
  expect_warning(icd_comorbid(uranium_pathology, icd10_map_quan_elix), regexp = NA)
  expect_warning(icd_comorbid(uranium_pathology, icd10_map_quan_deyo), regexp = NA)
  expect_warning(icd_comorbid(uranium_pathology, icd10_map_elix), regexp = NA)
  expect_warning(icd_comorbid(uranium_pathology, icd10_map_ahrq), regexp = NA)
  # TODO much more here
})

context("icd10 comorbidity lookups")

test_that("ahrq comorbidities found for test data", {
  test_two <- icd10_all_ahrq_one_pt
  test_two$icd10_code <- paste0(icd10_all_ahrq_one_pt$icd10_code, "J2")
  test_three <- icd10_all_ahrq_one_pt
  test_three$icd10_code <- paste0(icd10_all_ahrq_one_pt$icd10_code,
                                  random_string(nrow(test_three), max_chars = 5))
  td <- named_list(icd10_all_ahrq_one_pt, test_two = test_two, test_three)

  for (test_name in names(td)) {

    expect_error(res <- icd_comorbid(td[[test_name]], map = icd::icd10_map_ahrq), regexp = NA)
    for (n in colnames(res))
      expect_true(res[, n], info = paste("method one comorbidity:", n, ", test: ", test_name))

    expect_error(res <- icd10_comorbid(td[[test_name]], map = icd::icd10_map_ahrq), regexp = NA)
    for (n in colnames(res))
      expect_true(res[, n], info = paste("method two comorbidity:", n, ", test: ", test_name))

    expect_error(res <- icd10_comorbid_ahrq(td[[test_name]], hierarchy = FALSE), regexp = NA)
    for (n in colnames(res))
      expect_true(res[, n], info = paste("method three comorbidity:", n, ", test: ", test_name))

  }
})

test_that("ahrq comorbidities found for test data for multiple patients each with a different comorbidity", {
  test_three <- test_two <- icd10_all_ahrq
  test_two$icd10_code <- paste0(icd10_all_ahrq$icd10_code, "J2")
  test_three$icd10_code <- paste0(icd10_all_ahrq$icd10_code,
                                  random_string(nrow(test_three), max_chars = 5))
  td <- list(
    icd10_all_ahrq = icd10_all_ahrq,
    test_two = test_two,
    test_three = test_three
  )

  # just checking for a trace matrix of TRUE, but this way lets me find error much more quickly:
  for (test_name in names(td)) {

    res <- icd_comorbid(td[[test_name]], icd::icd10_map_ahrq)
    for (n in colnames(res))
      expect_equal(sum(res[, n]), 1, info = paste("col method one comorbidity:", n, ", test: ", test_name))
    for (n in rownames(res))
      expect_equal(sum(res[n, ]), 1, info = paste("row method one comorbidity:", n, ", test: ", test_name))

    res <- icd10_comorbid(td[[test_name]], icd::icd10_map_ahrq)
    for (n in colnames(res))
      expect_equal(sum(res[, n]), 1, info = paste("method two comorbidity:", n, ", test: ", test_name))
    for (n in rownames(res))
      expect_equal(sum(res[n, ]), 1, info = paste("row method two comorbidity:", n, ", test: ", test_name))
  }
})

test_that("comorbidity from single ICD-10 leaf or non-leaf code doesn't cause infinite recursion with Elix", {
  # this code I5020 is in all the maps for ICD-10
  for (code in c("I5020")) {
    for (class_fun in c("as.character", "as.icd10", "as.icd10cm")) {
      for (map_fun in c("icd10_comorbid_elix", "icd10_comorbid_quan_elix",
                        "icd10_comorbid_quan_deyo", "icd10_comorbid_ahrq",
                        "icd_comorbid_elix", "icd_comorbid_quan_elix",
                        "icd_comorbid_quan_deyo", "icd_comorbid_ahrq")) {
        code_with_class <- do.call(class_fun, list(code))
        df <- data.frame(visit = 1, code = code_with_class,
                         stringsAsFactors = FALSE) # TODO: allow factors
        res <- do.call(map_fun, list(x = df))
        expect_true(res[, "CHF"])
        expect_false("HTNcx" %in% names(res))

        df2 <- data.frame(visit = c(1, 2), code = c(code_with_class, code_with_class),
                          stringsAsFactors = FALSE) # TODO: allow factors
        res <- do.call(map_fun, list(df2))
        expect_true(all(res[, "CHF"]))
        expect_false("HTNcx" %in% names(res))
      }
    }
  }
})
