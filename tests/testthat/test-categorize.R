context("categorize")

test_that("categorize result is empty", {
  expect_equal(0,
               nrow(res <- categorize(one_icd10_pt[0,],
                                      map = icd10_map_ahrq,
                                      id_name = "visit",
                                      code_name = "icd"))
  )
  expect_identical(colnames(res), names(icd10_map_ahrq))
  expect_equal(0,
               nrow(rdf <- categorize(one_icd10_pt[0,],
                                      map = icd10_map_ahrq,
                                      id_name = "visit",
                                      code_name = "icd"))
  )
  expect_identical(colnames(rdf), names(icd10_map_ahrq))
})

test_that("binary instead of logical can be returned", {
  expect_identical(
    categorize(one_icd10_pt,
               map = icd10_map_ahrq,
               id_name = "visit",
               code_name = "icd",
               return_binary = TRUE),
    logical_to_binary(
      categorize(one_icd10_pt,
                 map = icd10_map_ahrq,
                 id_name = "visit",
                 code_name = "icd",
                 return_binary = FALSE))
  )
  expect_identical(
    categorize(one_icd10_pt,
               map = icd10_map_ahrq,
               id_name = "visit",
               code_name = "icd",
               return_binary = TRUE,
               return_df = TRUE),
    logical_to_binary(
      categorize(one_icd10_pt,
                 map = icd10_map_ahrq,
                 id_name = "visit",
                 code_name = "icd",
                 return_binary = FALSE,
                 return_df = TRUE))
  )
})

test_that("different ID types are preserved", {
  pts_int <- pts_num <- pts_chr <- random_icd10_pts
  pts_int$visit_id <- as.integer(pts_int$visit_id)
  pts_num$visit_id <- as.numeric(pts_num$visit_id)
  pts_chr$visit_id <- as_char_no_warn(pts_chr$visit_id)
  ress <- lapply(
    list(pts_int, pts_num, pts_chr),
    function(x) categorize(x,
                           map = icd10_map_elix,
                           id_name = "visit_id",
                           code_name = "icd10",
                           return_df = TRUE,
                           preserve_visit_id_type = TRUE))
  expect_equal(class(ress[[1]][[1]]), "integer")
  expect_equal(class(ress[[2]][[1]]), "numeric")
  expect_equal(class(ress[[3]][[1]]), "character")
  ress_noid <- lapply(ress, `[`, -1L)
  expect_identical(ress_noid[[1]], ress_noid[[2]])
  expect_identical(ress_noid[[1]], ress_noid[[3]])
})

test_that("different ID types are preserved (simple)", {
  pts_int <- pts_num <- pts_chr <- random_icd10_pts
  pts_int$visit_id <- as.integer(pts_int$visit_id)
  pts_num$visit_id <- as.numeric(pts_num$visit_id)
  pts_chr$visit_id <- as_char_no_warn(pts_chr$visit_id)
  ress <- lapply(
    list(pts_int, pts_num, pts_chr),
    function(x) categorize_simple(x,
                                  map = icd10_map_elix,
                                  id_name = "visit_id",
                                  code_name = "icd10",
                                  return_df = TRUE,
                                  preserve_visit_id_type = TRUE))
  expect_equal(class(ress[[1]][[1]]), "integer")
  expect_equal(class(ress[[2]][[1]]), "numeric")
  expect_equal(class(ress[[3]][[1]]), "character")
  ress_noid <- lapply(ress, `[`, -1L)
  expect_identical(ress_noid[[1]], ress_noid[[2]])
  expect_identical(ress_noid[[1]], ress_noid[[3]])
})
