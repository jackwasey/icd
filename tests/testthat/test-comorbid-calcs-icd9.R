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

context("Generic comorbidity calculation")

test_that("comorbid quick test", {
  testres <- icd9_comorbid(two_pts, two_map, return_df = TRUE)
  testres_cat_simple <- categorize_simple(two_pts, two_map, return_df = TRUE,
                                          id_name = "visit_id", code_name = "icd9")
  trueres <- data.frame("visit_id" = c("v01", "v02"),
                        "malady" = c(FALSE, TRUE),
                        "ailment" = c(TRUE, FALSE),
                        stringsAsFactors = FALSE)
  expect_equal(testres, trueres)
  expect_equal(testres_cat_simple, trueres)
  testmat <- icd9_comorbid(two_pts, two_map, return_df = FALSE)
  truemat <- matrix(c(FALSE, TRUE, TRUE, FALSE), nrow = 2,
                    dimnames = list(c("v01", "v02"), c("malady", "ailment")))
  expect_equal(testmat, truemat)
  testresfac <- icd9_comorbid(two_pts_fac, two_map_fac, return_df = TRUE)
  trueresfac <- data.frame("visit_id" = c("v01", "v02"),
                           "malady" = c(FALSE, TRUE),
                           "ailment" = c(TRUE, FALSE),
                           stringsAsFactors = TRUE)
  expect_equal(testresfac, trueresfac)
  expect_equal(icd9_comorbid(two_pts, two_map_fac, return_df = FALSE), truemat)
  expect_equal(icd9_comorbid(two_pts_fac, two_map, return_df = FALSE), truemat)
  expect_equal(icd9_comorbid(two_pts_fac, two_map_fac, return_df = FALSE), truemat)
})

test_that("failing example", {
  mydf <- data.frame(visit_id = c("a", "b", "c"),
                     icd9 = c("441", "412.93", "042"))
  cmb <- icd9_comorbid_quan_deyo(mydf, short_code = FALSE, hierarchy = TRUE)
  expect_false("names" %in% names(attributes(cmb)))
  charlson(mydf, short_code = FALSE)
  expect_is(charlson(mydf, short_code = FALSE, return_df = TRUE), "data.frame")
  charlson_from_comorbid(cmb)
})

test_that("disordered visit_ids works by default", {
  set.seed(1441)
  rnd_ord <- sample(seq_along(test_twenty$visit_id))
  dat <- test_twenty[rnd_ord, ]
  tres <- icd9_comorbid(dat, icd9_map_ahrq)
  cres <- icd9_comorbid(test_twenty, icd9_map_ahrq)
  expect_equal(dim(tres), dim(cres))
  expect_equal(sum(tres), sum(cres))
  expect_true(setequal(rownames(tres), rownames(cres)))
  expect_equal(colnames(tres), colnames(cres))
})

context("ICD-9 comorbidity calculations")

test_that("ahrq all comorbidities in one patient, no abbrev, hier", {
  res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                            abbrev_names = FALSE,
                            hierarchy = TRUE, return_df = TRUE)
  expect_equal(dim(res), c(1, 30))
  expect_true(setequal(c("visit_id", names_ahrq), names(res)))
  # should not have dm and dmcx, etc
  expect_false(all(as.logical(res[1, unlist(names_ahrq)])))
  expect_false(res[1, "Diabetes, uncomplicated"])
  expect_false(res[1, "Solid tumor without metastasis"])

  #matrix
  res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                            abbrev_names = FALSE,
                            hierarchy = TRUE, return_df = FALSE)
  expect_equal(dim(res), c(1, 29))
  expect_true(setequal(names_ahrq, colnames(res)))
  # should not have dm and dmcx, etc
  expect_false(all(as.logical(res[1, unlist(names_ahrq)])))
  expect_false(res[1, "Diabetes, uncomplicated"])
  expect_false(res[1, "Solid tumor without metastasis"])
})

test_that("empty data returns empty data with or without hierarchy", {
  res <- icd9_comorbid_ahrq(empty_pts, hierarchy = FALSE)
  res2 <- dim(icd9_comorbid_ahrq(empty_pts, hierarchy = TRUE))
  res3 <- icd9_comorbid_ahrq(empty_pts, hierarchy = TRUE)
  expect_identical(res, empty_ahrq_mat)
  expect_identical(res2, dim(empty_ahrq_mat_heir))
  expect_identical(res3, empty_ahrq_mat_heir)
  # TODO: same for other comorbidity mappings
})

test_that("elix, all cmb in one patient, no abbrev, hier", {
  res <- icd9_comorbid_elix(elix_test_dat, short_code = TRUE,
                            abbrev_names = FALSE,
                            hierarchy = TRUE, return_df = TRUE)
  expect_equal(dim(res), c(1, 31))
  expect_true(setequal(c("visit_id", names_elix), names(res)))
  # should not have dm and dmcx, etc
  expect_false(all(as.logical(res[1, unlist(names_elix)])))
  expect_false(res[1, "Diabetes, uncomplicated"])
  expect_false(res[1, "Solid tumor without metastasis"])

  #matrix
  res <- icd9_comorbid_elix(elix_test_dat, short_code = TRUE,
                            abbrev_names = FALSE,
                            hierarchy = TRUE, return_df = FALSE)
  expect_equal(dim(res), c(1, 30))
  expect_true(setequal(names_elix, colnames(res)))
  # should not have dm and dmcx, etc
  expect_false(all(as.logical(res[1, unlist(names_elix)])))
  expect_false(res[1, "Diabetes, uncomplicated"])
  expect_false(res[1, "Solid tumor without metastasis"])
})

test_that("elix, all cmb in one patient, abbrev, hier", {
  res <- icd9_comorbid_elix(elix_test_dat,
                            short_code = TRUE,
                            abbrev_names = TRUE,
                            hierarchy = TRUE, return_df = TRUE)
  expect_equal(dim(res), c(1, 31))
  expect_true(setequal(c("visit_id", names_elix_abbrev), names(res)))
  # should not have dm and dmcx, etc
  expect_false(
    all(as.logical(res[1, unlist(names_elix_abbrev)])))
  expect_false(res[1, "DM"])
  expect_false(res[1, "Tumor"])

  #matrix
  res <- icd9_comorbid_elix(elix_test_dat,
                            short_code = TRUE,
                            abbrev_names = TRUE,
                            hierarchy = TRUE, return_df = FALSE)
  expect_equal(dim(res), c(1, 30))
  expect_true(setequal(names_elix_abbrev, colnames(res)))
  # should not have dm and dmcx, etc
  expect_false(
    all(as.logical(res[1, unlist(names_elix_abbrev)])))
  expect_false(res[1, "DM"])
  expect_false(res[1, "Tumor"])
})

test_that("elix, all cmb in one patient, no abbrev, no hier", {
  res <- icd9_comorbid_elix(elix_test_dat, short_code = TRUE,
                            abbrev_names = FALSE,
                            hierarchy = FALSE, return_df = TRUE)
  expect_equal(dim(res), c(1, 32)) #longer because 2x htn
  expect_true(setequal(c("visit_id", names_elix_htn), names(res)))
  # not applying hierarchy, so dm and dmcx can both be true
  expect_true(all(as.logical(res[1, unlist(names_elix_htn)])))

  # same for matrix result
  res <- icd9_comorbid_elix(elix_test_dat, short_code = TRUE,
                            abbrev_names = FALSE,
                            hierarchy = FALSE, return_df = FALSE)
  expect_equal(dim(res), c(1, 31)) #longer because 2x htn
  expect_true(setequal(names_elix_htn, colnames(res)))
  # not applying hierarchy, so dm and dmcx can both be true
  expect_true(all(as.logical(res[1, unlist(names_elix_htn)])))
})

test_that("elix, all cmb in one patient, abbrev, no hier", {
  res <- icd9_comorbid_elix(elix_test_dat, short_code = TRUE,
                            abbrev_names = TRUE,
                            hierarchy = FALSE, return_df = TRUE)
  expect_equal(dim(res), c(1, 32))
  expect_true(setequal(c("visit_id", names_elix_htn_abbrev), names(res)))
  expect_true(
    all(as.logical(res[1, unlist(names_elix_htn_abbrev)])))

  #matrix
  res <- icd9_comorbid_elix(elix_test_dat, short_code = TRUE,
                            abbrev_names = TRUE,
                            hierarchy = FALSE, return_df = FALSE)
  expect_equal(dim(res), c(1, 31))
  expect_true(setequal(names_elix_htn_abbrev, colnames(res)))
  expect_true(
    all(as.logical(res[1, unlist(names_elix_htn_abbrev)])))

})

test_that("qelix, all cmb in one patient, no abbrev, hier", {
  res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                 short_code = TRUE, abbrev_names = FALSE,
                                 hierarchy = TRUE, return_df = TRUE)
  expect_equal(dim(res), c(1, 31))
  expect_true(setequal(c("visit_id", names_quan_elix), names(res)))
  # should not have dm and dmcx, etc
  expect_false(
    all(as.logical(res[1, unlist(names_quan_elix)])))
  expect_false(res[1, "Diabetes, uncomplicated"])
  expect_false(res[1, "Solid tumor without metastasis"])

  #matrix
  res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                 short_code = TRUE,
                                 abbrev_names = FALSE,
                                 hierarchy = TRUE, return_df = FALSE)
  expect_equal(dim(res), c(1, 30))
  expect_true(setequal(names_quan_elix, colnames(res)))
  # should not have dm and dmcx, etc
  expect_false(
    all(as.logical(res[1, unlist(names_quan_elix)])))
  expect_false(res[1, "Diabetes, uncomplicated"])
  expect_false(res[1, "Solid tumor without metastasis"])
})

test_that("qelix, cmb in one patient, abbrev, hier", {
  res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                 short_code = TRUE,
                                 abbrev_names = TRUE,
                                 hierarchy = TRUE, return_df = TRUE)
  expect_equal(dim(res), c(1, 31))
  expect_true(setequal(c("visit_id", names_quan_elix_abbrev), colnames(res)))
  # should not have dm and dmcx, etc
  expect_false(
    all(as.logical(res[1, unlist(names_quan_elix_abbrev)])))
  expect_false(res[1, "DM"])
  expect_false(res[1, "Tumor"])

  #same for matrix
  res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                 short_code = TRUE,
                                 abbrev_names = TRUE,
                                 hierarchy = TRUE, return_df = FALSE)
  expect_equal(dim(res), c(1, 30))
  expect_equal(rownames(res)[1], quan_elix_test_dat[1, "visit_id"])
  expect_true(setequal(names_quan_elix_abbrev, colnames(res)))
  # should not have dm and dmcx, etc
  expect_false(
    all(as.logical(res[1, unlist(names_quan_elix_abbrev)])))
  expect_false(res[1, "DM"])
  expect_false(res[1, "Tumor"])
})

test_that("qelix, all cmb in one patient, no abbrev, no hier", {
  res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                 short_code = TRUE,
                                 abbrev_names = FALSE,
                                 hierarchy = FALSE, return_df = TRUE)
  #longer because 2x htn
  expect_equal(dim(res), c(1, 32))
  # not applying hierarchy, so dm and dmcx can both be true
  expect_true(setequal(c("visit_id", names_quan_elix_htn), names(res)))
  expect_true(
    all(as.logical(res[1, unlist(names_quan_elix_htn)])))

  #matrix
  res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                 short_code = TRUE,
                                 abbrev_names = FALSE,
                                 hierarchy = FALSE, return_df = FALSE)
  expect_equal(dim(res), c(1, 31))
  # not applying hierarchy, so dm and dmcx can both be true
  expect_true(setequal(names_quan_elix_htn, colnames(res)))
  expect_true(
    all(as.logical(res[1, unlist(names_quan_elix_htn)])))
})

test_that("qelix, all cmb in one patient, abbrev, no hier", {
  res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                 short_code = TRUE,
                                 abbrev_names = TRUE,
                                 hierarchy = FALSE, return_df = TRUE)
  expect_equal(dim(res), c(1, 32))
  expect_true(setequal(c("visit_id", names_quan_elix_htn_abbrev), names(res)))
  expect_true(
    all(as.logical(res[1, unlist(names_quan_elix_htn_abbrev)])))
  # same for matrix
  res <- icd9_comorbid_quan_elix(quan_elix_test_dat,
                                 short_code = TRUE,
                                 abbrev_names = TRUE,
                                 hierarchy = FALSE, return_df = FALSE)
  expect_equal(dim(res), c(1, 31))
  expect_true(setequal(names_quan_elix_htn_abbrev, colnames(res)))
  expect_true(
    all(as.logical(res[1, unlist(names_quan_elix_htn_abbrev)])))
})

test_that("ahrq, all cmb in one patient, abbrev, hier", {
  res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                            abbrev_names = TRUE,
                            hierarchy = TRUE, return_df = TRUE)
  expect_equal(dim(res), c(1, 30))
  expect_true(setequal(c("visit_id", names_ahrq_abbrev), names(res)))
  # should not have dm and dmcx, etc
  expect_false(
    all(as.logical(res[1, unlist(names_ahrq_abbrev)])))
  expect_false(res[1, "DM"])
  expect_false(res[1, "Tumor"])

  #same for matrix
  res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                            abbrev_names = TRUE,
                            hierarchy = TRUE, return_df = FALSE)
  expect_equal(dim(res), c(1, 29))
  expect_true(setequal(names_ahrq_abbrev, colnames(res)))
  expect_false(
    all(as.logical(res[1, unlist(names_ahrq_abbrev)])))
  expect_false(res[1, "DM"])
  expect_false(res[1, "Tumor"])
})

test_that("ahrq, all cmb in one patient, no abbrev, no hier", {
  res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                            abbrev_names = FALSE,
                            hierarchy = FALSE, return_df = TRUE)
  #longer because 2x htn
  expect_equal(dim(res), c(1, 31))
  # not applying hierarchy, so dm and dmcx can both be true
  expect_true(setequal(c("visit_id", names_ahrq_htn), names(res)))
  expect_true(all(as.logical(res[1, unlist(names_ahrq_htn)])))

  #same for matrix:
  res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                            abbrev_names = FALSE,
                            hierarchy = FALSE, return_df = FALSE)
  expect_equal(dim(res), c(1, 30))
  expect_true(setequal(names_ahrq_htn, colnames(res)))
  expect_true(all(as.logical(res[1, unlist(names_ahrq_htn)])))
})

test_that("ahrq, all cmb in one patient, abbrev, no hier", {
  res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                            abbrev_names = TRUE,
                            hierarchy = FALSE, return_df = TRUE)
  expect_equal(dim(res), c(1, 31))
  expect_true(setequal(c("visit_id", names_ahrq_htn_abbrev), names(res)))
  expect_true(
    all(as.logical(res[1, unlist(names_ahrq_htn_abbrev)])))

  #matrix
  res <- icd9_comorbid_ahrq(ahrq_test_dat, short_code = TRUE,
                            abbrev_names = TRUE,
                            hierarchy = FALSE, return_df = FALSE)
  expect_equal(dim(res), c(1, 30))
  expect_true(setequal(names_ahrq_htn_abbrev, colnames(res)))
  expect_true(
    all(as.logical(res[1, unlist(names_ahrq_htn_abbrev)])))
})

test_that("Charlson/Deyo comorbidities for a single patient, one icd9", {
  expect_equal(
    icd9_comorbid_quan_deyo(one_pt_one_icd9, short_code = FALSE, return_df = TRUE),
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
      class = "data.frame"))
})

test_that("no error for deyo single pt w two identical (major) icd9 codes", {
  expect_error(icd9_comorbid_quan_deyo(one_pt_two_icd9, short_code = FALSE, return_df = TRUE), NA)
})

test_that("no error for deyo single pt w two different decimal icd9 codes", {
  mydf <- data.frame(visit_id = c("a", "a"), icd9 = c("441", "412.93"))
  expect_error(icd9_comorbid_quan_deyo(mydf), NA)
  expect_error(icd9_comorbid_quan_deyo(mydf, short_code = FALSE, return_df = TRUE), NA)
})

test_that("dispatch from column class when specified", {
  mydf <- data.frame(visit_id = c("a", "b", "c"),
                     icd9 = icd:::icd9cm(c("412.93", "441", "042")))
  expect_warning(icd9_comorbid_quan_elix(mydf), regexp = NA)
  expect_warning(icd9_comorbid_quan_deyo(mydf), regexp = NA)
  expect_warning(icd9_comorbid_elix(mydf), regexp = NA)
  expect_warning(icd9_comorbid_ahrq(mydf), regexp = NA)
})

# if an ICD class is not set for a column, the correct method is dispatched
test_that("dispatch from column class when not specified", {
  mydf <- data.frame(visit_id = c("a", "b", "c"),
                     icd9 = c("412.93", "441", "042"))
  expect_warning(icd9_comorbid_quan_elix(mydf), regexp = NA)
  expect_warning(icd9_comorbid_quan_deyo(mydf), regexp = NA)
  expect_warning(icd9_comorbid_elix(mydf), regexp = NA)
  expect_warning(icd9_comorbid_ahrq(mydf), regexp = NA)
})

test_that("if we try to do comorbidity calc on wide data, it gives error", {
  expect_error(comorbid_elix(vermont_dx), regexp = NA)
  expect_error(comorbid_charlson(long_to_wide(uranium_pathology, return_df = TRUE)), regexp = NA)
})

test_that("code appearing in two icd9 comorbidities", {
  dat <- data.frame(id = 1, icd9 = c("123"))
  map <- list(a = "123", b = "123")
  expect_identical(res <- icd9_comorbid(dat, map),
                   matrix(c(TRUE, TRUE), nrow = 1, dimnames = list("1", c("a", "b")))
  )
  dat_clean <- data.frame(id = "1", icd9 = factor("123"), stringsAsFactors = FALSE)
})

test_that("comorbid for icd9 gives binary values if asked for matrices", {
  res_bin <- comorbid(random_test_patients, map = icd9_map_charlson,
                      return_binary = TRUE, return_df = FALSE)
  res_log <- comorbid(random_test_patients, map = icd9_map_charlson,
                      return_binary = FALSE, return_df = FALSE)
  expect_true(is.integer(res_bin))
  expect_true(is.logical(res_log))
  expect_equivalent(apply(res_log, 2, as.integer), res_bin)
  expect_identical(res_bin, logical_to_binary(res_log))
  expect_identical(res_log, binary_to_logical(res_bin))
})

test_that("comorbid for icd9 gives binary values if asked for data.frames", {
  res_bin <- comorbid(random_test_patients, map = icd9_map_charlson,
                      return_binary = TRUE, return_df = TRUE)
  res_log <- comorbid(random_test_patients, map = icd9_map_charlson,
                      return_binary = FALSE, return_df = TRUE)
  expect_true(all(vapply(res_bin[-1], is.integer, logical(1))))
  expect_true(all(vapply(res_log[-1], is.logical, logical(1))))
  expect_identical(res_bin, logical_to_binary(res_log))
  expect_identical(res_log, binary_to_logical(res_bin))
})

test_that("binary output for CCS", {
  res_bin <- comorbid_ccs(random_test_patients,
                          return_binary = TRUE, return_df = TRUE)
  res_log <- comorbid_ccs(random_test_patients,
                          return_binary = FALSE, return_df = TRUE)
  expect_true(all(vapply(res_bin[-1], is.integer, logical(1))))
  expect_true(all(vapply(res_log[-1], is.logical, logical(1))))
  expect_identical(res_bin, logical_to_binary(res_log))
  expect_identical(res_log, binary_to_logical(res_bin))
})

test_that("integer visit IDs", {
  d <- ahrq_test_dat
  d$visit_id <- -1L
  mat_res <- comorbid_ahrq(d)
  expect_identical(rownames(mat_res), as.character(unique(d$visit_id)))
  df_res <- comorbid_ahrq(d, return_df = TRUE, preserve_id_type = TRUE)
  expect_identical(df_res$visit_id, d[1, "visit_id"])
})

test_that("float visit IDs", {
  d <- ahrq_test_dat
  d$visit_id <- -1.7
  mat_res <- comorbid_ahrq(d)
  expect_identical(rownames(mat_res), as.character(unique(d$visit_id)))
  df_res <- comorbid_ahrq(d, return_df = TRUE, preserve_id_type = TRUE)
  expect_identical(df_res$visit_id, d[1, "visit_id"])
})
