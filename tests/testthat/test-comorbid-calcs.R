# Copyright (C) 2014 - 2015  Jack O. Wasey
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

context("comorbidity calculations")

test_that("ahrq comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, hier", {
            res <- icd_comorbid_ahrq.icd9(ahrqTestDat, short_code = TRUE,
                                    abbrev_names = FALSE,
                                    apply_hierarchy = TRUE, return_df = TRUE)
            expect_equal(dim(res), c(1, 30))
            expect_true(setequal(c("visitId", ahrqComorbidNames), names(res)))
            # should not have dm and dmcx, etc
            expect_false(all(as.logical(res[1, unlist(ahrqComorbidNames)])))
            expect_false(res[1, "Diabetes, uncomplicated"])
            expect_false(res[1, "Solid tumor without metastasis"])

            #matrix
            res <- icd_comorbid_ahrq.icd9(ahrqTestDat, short_code = TRUE,
                                    abbrev_names = FALSE,
                                    hierarchy = TRUE, return_df = FALSE)
            expect_equal(dim(res), c(1, 29))
            expect_true(setequal(ahrqComorbidNames, colnames(res)))
            # should not have dm and dmcx, etc
            expect_false(all(as.logical(res[1, unlist(ahrqComorbidNames)])))
            expect_false(res[1, "Diabetes, uncomplicated"])
            expect_false(res[1, "Solid tumor without metastasis"])
          })

test_that("elix comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, hier", {
            res <- icd_comorbid_elix.icd9(elixTestDat, short_code = TRUE,
                                    abbrev_names = FALSE,
                                    hierarchy = TRUE, return_df = TRUE)
            expect_equal(dim(res), c(1,31))
            expect_true(setequal(c("visitId", elixComorbidNames), names(res)))
            # should not have dm and dmcx, etc
            expect_false(all(as.logical(res[1, unlist(elixComorbidNames)])))
            expect_false(res[1, "Diabetes, uncomplicated"])
            expect_false(res[1, "Solid tumor without metastasis"])

            #matrix
            res <- icd_comorbid_elix.icd9(elixTestDat, short_code = TRUE,
                                    abbrev_names = FALSE,
                                    hierarchy = TRUE, return_df = FALSE)
            expect_equal(dim(res), c(1,30))
            expect_true(setequal(elixComorbidNames, colnames(res)))
            # should not have dm and dmcx, etc
            expect_false(all(as.logical(res[1, unlist(elixComorbidNames)])))
            expect_false(res[1, "Diabetes, uncomplicated"])
            expect_false(res[1, "Solid tumor without metastasis"])
          })

test_that("elix comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, hier", {
            res <- icd_comorbid_elix.icd9(elixTestDat,
                                    short_code = TRUE,
                                    abbrev_names = TRUE,
                                    hierarchy = TRUE, return_df = TRUE)
            expect_equal(dim(res), c(1,31))
            expect_true(setequal(c("visitId", elixComorbidNamesAbbrev), names(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(elixComorbidNamesAbbrev)])))
            expect_false(res[1, "DM"])
            expect_false(res[1, "Tumor"])

            #matrix
            res <- icd_comorbid_elix.icd9(elixTestDat,
                                    short_code = TRUE,
                                    abbrev_names = TRUE,
                                    hierarchy = TRUE, return_df = FALSE)
            expect_equal(dim(res), c(1,30))
            expect_true(setequal(elixComorbidNamesAbbrev, colnames(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(elixComorbidNamesAbbrev)])))
            expect_false(res[1, "DM"])
            expect_false(res[1, "Tumor"])
          })

test_that("elix comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, no hier", {
            res <- icd_comorbid_elix.icd9(elixTestDat, short_code = TRUE,
                                    abbrev_names = FALSE,
                                    hierarchy = FALSE, return_df = TRUE)
            expect_equal(dim(res), c(1,32)) #longer because 2x htn
            expect_true(setequal(c("visitId", elixComorbidNamesHtn), names(res)))
            # not applying hierarchy, so dm and dmcx can both be true
            expect_true(all(as.logical(res[1, unlist(elixComorbidNamesHtn)])))

            # same for matrix result
            res <- icd_comorbid_elix.icd9(elixTestDat, short_code = TRUE,
                                    abbrev_names = FALSE,
                                    hierarchy = FALSE, return_df = FALSE)
            expect_equal(dim(res), c(1, 31)) #longer because 2x htn
            expect_true(setequal(elixComorbidNamesHtn, colnames(res)))
            # not applying hierarchy, so dm and dmcx can both be true
            expect_true(all(as.logical(res[1, unlist(elixComorbidNamesHtn)])))
          })

test_that("elix comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, no hier", {
            res <- icd_comorbid_elix.icd9(elixTestDat, short_code = TRUE,
                                    abbrev_names = TRUE,
                                    hierarchy = FALSE, return_df = TRUE)
            expect_equal(dim(res), c(1,32))
            expect_true(setequal(c("visitId", elixComorbidNamesHtnAbbrev), names(res)))
            expect_true(
              all(as.logical(res[1, unlist(elixComorbidNamesHtnAbbrev)])))

            #matrix
            res <- icd_comorbid_elix.icd9(elixTestDat, short_code = TRUE,
                                    abbrev_names = TRUE,
                                    hierarchy = FALSE, return_df = FALSE)
            expect_equal(dim(res), c(1,31))
            expect_true(setequal(elixComorbidNamesHtnAbbrev, colnames(res)))
            expect_true(
              all(as.logical(res[1, unlist(elixComorbidNamesHtnAbbrev)])))

          })

test_that("qelix comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, hier", {
            res <- icd_comorbid_quan_elix.icd9(quanElixTestDat,
                                        short_code = TRUE, abbrev_names = FALSE,
                                        hierarchy = TRUE, return_df = TRUE)
            expect_equal(dim(res), c(1,31))
            expect_true(setequal(c("visitId", quanElixComorbidNames), names(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(quanElixComorbidNames)])))
            expect_false(res[1, "Diabetes, uncomplicated"])
            expect_false(res[1, "Solid tumor without metastasis"])

            #matrix
            res <- icd_comorbid_quan_elix.icd9(quanElixTestDat,
                                        short_code = TRUE,
                                        abbrev_names = FALSE,
                                        hierarchy = TRUE, return_df = FALSE)
            expect_equal(dim(res), c(1,30))
            expect_true(setequal(quanElixComorbidNames, colnames(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(quanElixComorbidNames)])))
            expect_false(res[1, "Diabetes, uncomplicated"])
            expect_false(res[1, "Solid tumor without metastasis"])
          })

test_that("qelix comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, hier", {
            res <- icd_comorbid_quan_elix.icd9(quanElixTestDat,
                                        short_code = TRUE,
                                        abbrev_names = TRUE,
                                        hierarchy = TRUE, return_df = TRUE)
            expect_equal(dim(res), c(1,31))
            expect_true(setequal(c("visitId", quanElixComorbidNamesAbbrev), colnames(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(quanElixComorbidNamesAbbrev)])))
            expect_false(res[1, "DM"])
            expect_false(res[1, "Tumor"])

            #same for matrix
            res <- icd_comorbid_quan_elix.icd9(quanElixTestDat,
                                        short_code = TRUE,
                                        abbrev_names = TRUE,
                                        hierarchy = TRUE, return_df = FALSE)
            expect_equal(dim(res), c(1, 30))
            expect_equal(rownames(res)[1], quanElixTestDat[1, "visitId"])
            expect_true(setequal(quanElixComorbidNamesAbbrev, colnames(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(quanElixComorbidNamesAbbrev)])))
            expect_false(res[1, "DM"])
            expect_false(res[1, "Tumor"])
          })

test_that("qelix comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, no hier", {
            res <- icd_comorbid_quan_elix.icd9(quanElixTestDat,
                                        short_code = TRUE,
                                        abbrev_names = FALSE,
                                        hierarchy = FALSE, return_df = TRUE)
            #longer because 2x htn
            expect_equal(dim(res), c(1,32))
            # not applying hierarchy, so dm and dmcx can both be true
            expect_true(setequal(c("visitId", quanElixComorbidNamesHtn), names(res)))
            expect_true(
              all(as.logical(res[1, unlist(quanElixComorbidNamesHtn)])))

            #matrix
            res <- icd_comorbid_quan_elix.icd9(quanElixTestDat,
                                        short_code = TRUE,
                                        abbrev_names = FALSE,
                                        hierarchy = FALSE, return_df = FALSE)
            expect_equal(dim(res), c(1,31))
            # not applying hierarchy, so dm and dmcx can both be true
            expect_true(setequal(quanElixComorbidNamesHtn, colnames(res)))
            expect_true(
              all(as.logical(res[1, unlist(quanElixComorbidNamesHtn)])))
          })

test_that("qelix comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, no hier", {
            res <- icd_comorbid_quan_elix.icd9(quanElixTestDat,
                                        short_code = TRUE,
                                        abbrev_names = TRUE,
                                        hierarchy = FALSE, return_df = TRUE)
            expect_equal(dim(res), c(1,32))
            expect_true(setequal(c("visitId", quanElixComorbidNamesHtnAbbrev), names(res)))
            expect_true(
              all(as.logical(res[1, unlist(quanElixComorbidNamesHtnAbbrev)])))
            # same for matrix
            res <- icd_comorbid_quan_elix.icd9(quanElixTestDat,
                                        short_code = TRUE,
                                        abbrev_names = TRUE,
                                        hierarchy = FALSE, return_df = FALSE)
            expect_equal(dim(res), c(1,31))
            expect_true(setequal(quanElixComorbidNamesHtnAbbrev, colnames(res)))
            expect_true(
              all(as.logical(res[1, unlist(quanElixComorbidNamesHtnAbbrev)])))
          })

test_that("ahrq comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, hier", {
            res <- icd_comorbid_ahrq.icd9(ahrqTestDat, short_code = TRUE,
                                    abbrev_names = TRUE,
                                    hierarchy = TRUE, return_df = TRUE)
            expect_equal(dim(res), c(1, 30))
            expect_true(setequal(c("visitId", ahrqComorbidNamesAbbrev), names(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(ahrqComorbidNamesAbbrev)])))
            expect_false(res[1, "DM"])
            expect_false(res[1, "Tumor"])

            #same for matrix
            res <- icd_comorbid_ahrq.icd9(ahrqTestDat, short_code = TRUE,
                                    abbrev_names = TRUE,
                                    hierarchy = TRUE, return_df = FALSE)
            expect_equal(dim(res), c(1,29))
            expect_true(setequal(ahrqComorbidNamesAbbrev, colnames(res)))
            expect_false(
              all(as.logical(res[1, unlist(ahrqComorbidNamesAbbrev)])))
            expect_false(res[1, "DM"])
            expect_false(res[1, "Tumor"])
          })

test_that("ahrq comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, no hier", {
            res <- icd_comorbid_ahrq.icd9(ahrqTestDat, short_code = TRUE,
                                    abbrev_names = FALSE,
                                    hierarchy = FALSE, return_df = TRUE)
            #longer because 2x htn
            expect_equal(dim(res), c(1, 31))
            # not applying hierarchy, so dm and dmcx can both be true
            expect_true(setequal(c("visitId", ahrqComorbidNamesHtn), names(res)))
            expect_true(all(as.logical(res[1, unlist(ahrqComorbidNamesHtn)])))

            #same for matrix:
            res <- icd_comorbid_ahrq.icd9(ahrqTestDat, short_code = TRUE,
                                    abbrev_names = FALSE,
                                    hierarchy = FALSE, return_df = FALSE)
            expect_equal(dim(res), c(1, 30))
            expect_true(setequal(ahrqComorbidNamesHtn, colnames(res)))
            expect_true(all(as.logical(res[1, unlist(ahrqComorbidNamesHtn)])))
          })

test_that("ahrq comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, no hier", {
            res <- icd_comorbid_ahrq.icd9(ahrqTestDat, short_code = TRUE,
                                    abbrev_names = TRUE,
                                    hierarchy = FALSE, return_df = TRUE)
            expect_equal(dim(res), c(1,31))
            expect_true(setequal(c("visitId", ahrqComorbidNamesHtnAbbrev), names(res)))
            expect_true(
              all(as.logical(res[1, unlist(ahrqComorbidNamesHtnAbbrev)])))

            #matrix
            res <- icd_comorbid_ahrq.icd9(ahrqTestDat, short_code = TRUE,
                                    abbrev_names = TRUE,
                                    hierarchy = FALSE, return_df = FALSE)
            expect_equal(dim(res), c(1, 30))
            expect_true(setequal(ahrqComorbidNamesHtnAbbrev, colnames(res)))
            expect_true(
              all(as.logical(res[1, unlist(ahrqComorbidNamesHtnAbbrev)])))
          })

test_that("get Charlson/Deyo comorbidities for a single patient", {
  mydf <- data.frame(visitId = c("a"),
                     icd9 = c("044.9"),
                     stringsAsFactors = FALSE)
  expect_equal(
    icd_comorbid_quan_deyo.icd9(mydf, short_code = FALSE, return_df = TRUE),
    structure(
      list(
        visitId = "a",
        MI = FALSE, CHF = FALSE, PVD = FALSE, Stroke = FALSE, Dementia = FALSE,
        Pulmonary = FALSE, Rheumatic = FALSE, PUD = FALSE, LiverMild = FALSE,
        DM = FALSE, DMcx = FALSE, Paralysis = FALSE, Renal = FALSE,
        Cancer = FALSE, LiverSevere = FALSE, Mets = FALSE, HIV = TRUE),
      .Names = c("visitId",
                 "MI", "CHF", "PVD", "Stroke", "Dementia", "Pulmonary",
                 "Rheumatic", "PUD", "LiverMild", "DM", "DMcx", "Paralysis",
                 "Renal", "Cancer", "LiverSevere", "Mets", "HIV"),
      row.names = 1L,
      class = "data.frame")
  )

  mydf <- data.frame(visitId = c("a", "a"),
                     icd9 = c("044.9", "044.9"),
                     stringsAsFactors = FALSE)
  expect_error(icd_comorbid_quan_deyo.icd9(mydf, short_code = FALSE, return_df = TRUE), NA)

  mydf <- data.frame(visitId = c("a", "a"), icd9 = c("441", "412.93"))
  expect_error(icd_comorbid_quan_deyo.icd9(mydf, short_code = FALSE, return_df = TRUE), NA)

})

test_that("if an ICD class is not specified for a data set, but is for a column therein, the correct method is dispatched", {
  mydf <- data.frame(visitId = c("a", "b", "c"), icd9 = c("412.93", "441", "044.9"))
  expect_error(icd_comorbid_quan_elix(mydf, short_code = FALSE, hierarchy = TRUE, return_df = TRUE), NA)
  # TODO, expand this section
})
