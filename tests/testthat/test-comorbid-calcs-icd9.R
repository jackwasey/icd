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

context("comorbidity calculations")

test_that("ahrq comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, hier", {
            res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                                      abbrev_names = FALSE,
                                      apply_hierarchy = TRUE, return_df = TRUE)
            expect_equal(dim(res), c(1, 30))
            expect_true(setequal(c("visit_id", icd_names_ahrq), names(res)))
            # should not have dm and dmcx, etc
            expect_false(all(as.logical(res[1, unlist(icd_names_ahrq)])))
            expect_false(res[1, "Diabetes, uncomplicated"])
            expect_false(res[1, "Solid tumor without metastasis"])

            #matrix
            res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                                      abbrev_names = FALSE,
                                      hierarchy = TRUE, return_df = FALSE)
            expect_equal(dim(res), c(1, 29))
            expect_true(setequal(icd_names_ahrq, colnames(res)))
            # should not have dm and dmcx, etc
            expect_false(all(as.logical(res[1, unlist(icd_names_ahrq)])))
            expect_false(res[1, "Diabetes, uncomplicated"])
            expect_false(res[1, "Solid tumor without metastasis"])
          })

test_that("elix comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, hier", {
            res <- icd9_comorbid_elix(elix_test_dat, short_code = TRUE,
                                      abbrev_names = FALSE,
                                      hierarchy = TRUE, return_df = TRUE)
            expect_equal(dim(res), c(1, 31))
            expect_true(setequal(c("visit_id", icd_names_elix), names(res)))
            # should not have dm and dmcx, etc
            expect_false(all(as.logical(res[1, unlist(icd_names_elix)])))
            expect_false(res[1, "Diabetes, uncomplicated"])
            expect_false(res[1, "Solid tumor without metastasis"])

            #matrix
            res <- icd9_comorbid_elix(elix_test_dat, short_code = TRUE,
                                      abbrev_names = FALSE,
                                      hierarchy = TRUE, return_df = FALSE)
            expect_equal(dim(res), c(1, 30))
            expect_true(setequal(icd_names_elix, colnames(res)))
            # should not have dm and dmcx, etc
            expect_false(all(as.logical(res[1, unlist(icd_names_elix)])))
            expect_false(res[1, "Diabetes, uncomplicated"])
            expect_false(res[1, "Solid tumor without metastasis"])
          })

test_that("elix comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, hier", {
            res <- icd9_comorbid_elix(elix_test_dat,
                                      short_code = TRUE,
                                      abbrev_names = TRUE,
                                      hierarchy = TRUE, return_df = TRUE)
            expect_equal(dim(res), c(1, 31))
            expect_true(setequal(c("visit_id", icd_names_elix_abbrev), names(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(icd_names_elix_abbrev)])))
            expect_false(res[1, "DM"])
            expect_false(res[1, "Tumor"])

            #matrix
            res <- icd9_comorbid_elix(elix_test_dat,
                                      short_code = TRUE,
                                      abbrev_names = TRUE,
                                      hierarchy = TRUE, return_df = FALSE)
            expect_equal(dim(res), c(1, 30))
            expect_true(setequal(icd_names_elix_abbrev, colnames(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(icd_names_elix_abbrev)])))
            expect_false(res[1, "DM"])
            expect_false(res[1, "Tumor"])
          })

test_that("elix comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, no hier", {
            res <- icd9_comorbid_elix(elix_test_dat, short_code = TRUE,
                                      abbrev_names = FALSE,
                                      hierarchy = FALSE, return_df = TRUE)
            expect_equal(dim(res), c(1, 32)) #longer because 2x htn
            expect_true(setequal(c("visit_id", icd_names_elix_htn), names(res)))
            # not applying hierarchy, so dm and dmcx can both be true
            expect_true(all(as.logical(res[1, unlist(icd_names_elix_htn)])))

            # same for matrix result
            res <- icd9_comorbid_elix(elix_test_dat, short_code = TRUE,
                                      abbrev_names = FALSE,
                                      hierarchy = FALSE, return_df = FALSE)
            expect_equal(dim(res), c(1, 31)) #longer because 2x htn
            expect_true(setequal(icd_names_elix_htn, colnames(res)))
            # not applying hierarchy, so dm and dmcx can both be true
            expect_true(all(as.logical(res[1, unlist(icd_names_elix_htn)])))
          })

test_that("elix comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, no hier", {
            res <- icd9_comorbid_elix(elix_test_dat, short_code = TRUE,
                                      abbrev_names = TRUE,
                                      hierarchy = FALSE, return_df = TRUE)
            expect_equal(dim(res), c(1, 32))
            expect_true(setequal(c("visit_id", icd_names_elix_htn_abbrev), names(res)))
            expect_true(
              all(as.logical(res[1, unlist(icd_names_elix_htn_abbrev)])))

            #matrix
            res <- icd9_comorbid_elix(elix_test_dat, short_code = TRUE,
                                      abbrev_names = TRUE,
                                      hierarchy = FALSE, return_df = FALSE)
            expect_equal(dim(res), c(1, 31))
            expect_true(setequal(icd_names_elix_htn_abbrev, colnames(res)))
            expect_true(
              all(as.logical(res[1, unlist(icd_names_elix_htn_abbrev)])))

          })

test_that("qelix comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, hier", {
            res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                           short_code = TRUE, abbrev_names = FALSE,
                                           hierarchy = TRUE, return_df = TRUE)
            expect_equal(dim(res), c(1, 31))
            expect_true(setequal(c("visit_id", icd_names_quan_elix), names(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(icd_names_quan_elix)])))
            expect_false(res[1, "Diabetes, uncomplicated"])
            expect_false(res[1, "Solid tumor without metastasis"])

            #matrix
            res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                           short_code = TRUE,
                                           abbrev_names = FALSE,
                                           hierarchy = TRUE, return_df = FALSE)
            expect_equal(dim(res), c(1, 30))
            expect_true(setequal(icd_names_quan_elix, colnames(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(icd_names_quan_elix)])))
            expect_false(res[1, "Diabetes, uncomplicated"])
            expect_false(res[1, "Solid tumor without metastasis"])
          })

test_that("qelix comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, hier", {
            res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                           short_code = TRUE,
                                           abbrev_names = TRUE,
                                           hierarchy = TRUE, return_df = TRUE)
            expect_equal(dim(res), c(1, 31))
            expect_true(setequal(c("visit_id", icd_names_quan_elix_abbrev), colnames(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(icd_names_quan_elix_abbrev)])))
            expect_false(res[1, "DM"])
            expect_false(res[1, "Tumor"])

            #same for matrix
            res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                           short_code = TRUE,
                                           abbrev_names = TRUE,
                                           hierarchy = TRUE, return_df = FALSE)
            expect_equal(dim(res), c(1, 30))
            expect_equal(rownames(res)[1], quan_elix_test_dat[1, "visit_id"])
            expect_true(setequal(icd_names_quan_elix_abbrev, colnames(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(icd_names_quan_elix_abbrev)])))
            expect_false(res[1, "DM"])
            expect_false(res[1, "Tumor"])
          })

test_that("qelix comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, no hier", {
            res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                           short_code = TRUE,
                                           abbrev_names = FALSE,
                                           hierarchy = FALSE, return_df = TRUE)
            #longer because 2x htn
            expect_equal(dim(res), c(1, 32))
            # not applying hierarchy, so dm and dmcx can both be true
            expect_true(setequal(c("visit_id", icd_names_quan_elix_htn), names(res)))
            expect_true(
              all(as.logical(res[1, unlist(icd_names_quan_elix_htn)])))

            #matrix
            res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                           short_code = TRUE,
                                           abbrev_names = FALSE,
                                           hierarchy = FALSE, return_df = FALSE)
            expect_equal(dim(res), c(1, 31))
            # not applying hierarchy, so dm and dmcx can both be true
            expect_true(setequal(icd_names_quan_elix_htn, colnames(res)))
            expect_true(
              all(as.logical(res[1, unlist(icd_names_quan_elix_htn)])))
          })

test_that("qelix comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, no hier", {
            res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                           short_code = TRUE,
                                           abbrev_names = TRUE,
                                           hierarchy = FALSE, return_df = TRUE)
            expect_equal(dim(res), c(1, 32))
            expect_true(setequal(c("visit_id", icd_names_quan_elix_htn_abbrev), names(res)))
            expect_true(
              all(as.logical(res[1, unlist(icd_names_quan_elix_htn_abbrev)])))
            # same for matrix
            res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                           short_code = TRUE,
                                           abbrev_names = TRUE,
                                           hierarchy = FALSE, return_df = FALSE)
            expect_equal(dim(res), c(1, 31))
            expect_true(setequal(icd_names_quan_elix_htn_abbrev, colnames(res)))
            expect_true(
              all(as.logical(res[1, unlist(icd_names_quan_elix_htn_abbrev)])))
          })

test_that("ahrq comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, hier", {
            res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                                      abbrev_names = TRUE,
                                      hierarchy = TRUE, return_df = TRUE)
            expect_equal(dim(res), c(1, 30))
            expect_true(setequal(c("visit_id", icd_names_ahrq_abbrev), names(res)))
            # should not have dm and dmcx, etc
            expect_false(
              all(as.logical(res[1, unlist(icd_names_ahrq_abbrev)])))
            expect_false(res[1, "DM"])
            expect_false(res[1, "Tumor"])

            #same for matrix
            res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                                      abbrev_names = TRUE,
                                      hierarchy = TRUE, return_df = FALSE)
            expect_equal(dim(res), c(1, 29))
            expect_true(setequal(icd_names_ahrq_abbrev, colnames(res)))
            expect_false(
              all(as.logical(res[1, unlist(icd_names_ahrq_abbrev)])))
            expect_false(res[1, "DM"])
            expect_false(res[1, "Tumor"])
          })

test_that("ahrq comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, no hier", {
            res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                                      abbrev_names = FALSE,
                                      hierarchy = FALSE, return_df = TRUE)
            #longer because 2x htn
            expect_equal(dim(res), c(1, 31))
            # not applying hierarchy, so dm and dmcx can both be true
            expect_true(setequal(c("visit_id", icd_names_ahrq_htn), names(res)))
            expect_true(all(as.logical(res[1, unlist(icd_names_ahrq_htn)])))

            #same for matrix:
            res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                                      abbrev_names = FALSE,
                                      hierarchy = FALSE, return_df = FALSE)
            expect_equal(dim(res), c(1, 30))
            expect_true(setequal(icd_names_ahrq_htn, colnames(res)))
            expect_true(all(as.logical(res[1, unlist(icd_names_ahrq_htn)])))
          })

test_that("ahrq comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, no hier", {
            res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                                      abbrev_names = TRUE,
                                      hierarchy = FALSE, return_df = TRUE)
            expect_equal(dim(res), c(1, 31))
            expect_true(setequal(c("visit_id", icd_names_ahrq_htn_abbrev), names(res)))
            expect_true(
              all(as.logical(res[1, unlist(icd_names_ahrq_htn_abbrev)])))

            #matrix
            res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                                      abbrev_names = TRUE,
                                      hierarchy = FALSE, return_df = FALSE)
            expect_equal(dim(res), c(1, 30))
            expect_true(setequal(icd_names_ahrq_htn_abbrev, colnames(res)))
            expect_true(
              all(as.logical(res[1, unlist(icd_names_ahrq_htn_abbrev)])))
          })

test_that("get Charlson/Deyo comorbidities for a single patient", {
  mydf <- data.frame(visit_id = c("a"),
                     icd9 = c("044.9"),
                     stringsAsFactors = FALSE)
  expect_equal(
    icd9_comorbid_quan_deyo(mydf, short_code = FALSE, return_df = TRUE),
    structure(
      list(
        visit_id = "a",
        MI = FALSE, CHF = FALSE, PVD = FALSE, Stroke = FALSE, Dementia = FALSE,
        Pulmonary = FALSE, Rheumatic = FALSE, PUD = FALSE, LiverMild = FALSE,
        DM = FALSE, DMcx = FALSE, Paralysis = FALSE, Renal = FALSE,
        Cancer = FALSE, LiverSevere = FALSE, Mets = FALSE, HIV = TRUE),
      .Names = c("visit_id",
                 "MI", "CHF", "PVD", "Stroke", "Dementia", "Pulmonary",
                 "Rheumatic", "PUD", "LiverMild", "DM", "DMcx", "Paralysis",
                 "Renal", "Cancer", "LiverSevere", "Mets", "HIV"),
      row.names = 1L,
      class = "data.frame")
  )

  mydf <- data.frame(visit_id = c("a", "a"),
                     icd9 = c("044.9", "044.9"),
                     stringsAsFactors = FALSE)
  expect_error(
    icd9_comorbid_quan_deyo(mydf, short_code = FALSE, return_df = TRUE), NA)

  mydf <- data.frame(visit_id = c("a", "a"), icd9 = c("441", "412.93"))
  expect_error(
    icd9_comorbid_quan_deyo(mydf, short_code = FALSE, return_df = TRUE), NA)

})

test_that("correct comorbidities when the whole data frame has a class for ICD type", {
  mydf <- data.frame(visit_id = c("a", "b", "c"),
                     icd9 = c("412.93", "441", "044.9"))
  icd9_comorbid_quan_elix(mydf)
  icd9_comorbid_quan_deyo(mydf)
  icd9_comorbid_elix(mydf)
  icd9_comorbid_ahrq(mydf)
})

test_that("if an ICD class is not specified for a data set, but is for a column therein,
          the correct method is dispatched", {
            mydf <- data.frame(visit_id = c("a", "b", "c"),
                               icd9 = icd9cm(c("412.93", "441", "044.9")))
            expect_warning(icd9_comorbid_quan_elix(mydf), regexp = NA)
            expect_warning(icd9_comorbid_quan_deyo(mydf), regexp = NA)
            expect_warning(icd9_comorbid_elix(mydf), regexp = NA)
            expect_warning(icd9_comorbid_ahrq(mydf), regexp = NA)
          })

test_that("if an ICD class is not specified for a data set, and is not set for a column therein,
          the correct method is dispatched", {
            mydf <- data.frame(visit_id = c("a", "b", "c"),
                               icd9 = c("412.93", "441", "044.9"))
            expect_warning(icd9_comorbid_quan_elix(mydf), regexp = NA)
            expect_warning(icd9_comorbid_quan_deyo(mydf), regexp = NA)
            expect_warning(icd9_comorbid_elix(mydf), regexp = NA)
            expect_warning(icd9_comorbid_ahrq(mydf), regexp = NA)
          })
