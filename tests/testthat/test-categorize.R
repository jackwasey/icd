context("categorize")

test_that("categorize_simple result is empty", {
  expect_equal(0,
               nrow(res <- categorize_simple(one_icd10_pt[0, ],
                                             map = icd10_map_ahrq,
                                             id_name = "visit",
                                             code_name = "icd"))
  )
  expect_identical(colnames(res), names(icd10_map_ahrq))
  expect_equal(0,
               nrow(rdf <- categorize_simple(one_icd10_pt[0, ],
                                             map = icd10_map_ahrq,
                                             id_name = "visit",
                                             code_name = "icd",
                                             return_df = TRUE))
  )
  expect_identical(c("visit", names(icd10_map_ahrq)), colnames(rdf))
})
test_that("binary instead of logical can be returned", {
  expect_identical(
    categorize_simple(one_icd10_pt,
                      map = icd10_map_ahrq,
                      id_name = "visit",
                      code_name = "icd",
                      return_binary = TRUE),
    logical_to_binary(
      categorize_simple(one_icd10_pt,
                        map = icd10_map_ahrq,
                        id_name = "visit",
                        code_name = "icd",
                        return_binary = FALSE))
  )
  expect_identical(
    categorize_simple(one_icd10_pt,
                      map = icd10_map_ahrq,
                      id_name = "visit",
                      code_name = "icd",
                      return_binary = TRUE,
                      return_df = TRUE),
    logical_to_binary(
      categorize_simple(one_icd10_pt,
                        map = icd10_map_ahrq,
                        id_name = "visit",
                        code_name = "icd",
                        return_binary = FALSE,
                        return_df = TRUE))
  )
})

test_that("different ID types are preserved w data frame return", {
  pts_int <- pts_num <- pts_chr <- pts_fac <- random_icd10_pts
  empty_i <- empty_n <- empty_c <- empty_f <- random_icd10_pts[NULL, ]
  pts_int$visit_id <- as.integer(pts_int$visit_id)
  pts_num$visit_id <- as.numeric(pts_num$visit_id)
  pts_chr$visit_id <- as_char_no_warn(pts_chr$visit_id)
  pts_fac$visit_id <- factor(pts_chr$visit_id)
  empty_n$visit_id <- numeric()
  empty_c$visit_id <- character()
  empty_f$visit_id <- factor()
  for (cat_fun in c("categorize_simple")) {
    ress <- lapply(
      stats::setNames(list(pts_int, pts_num, pts_chr, pts_fac,
                           empty_i, empty_n, empty_c, empty_f),
                      c("i", "n", "c", "f", "ei", "en", "ec", "ef")),
      function(x) eval(call(cat_fun, x,
                            map = icd10_map_elix,
                            id_name = "visit_id",
                            code_name = "icd10",
                            return_df = TRUE,
                            preserve_id_type = TRUE)))
    expect_equal(class(ress[["i"]][[1]]), "integer")
    expect_equal(class(ress[["n"]][[1]]), "numeric")
    expect_equal(class(ress[["c"]][[1]]), "character")
    expect_equal(class(ress[["f"]][[1]]), "factor")
    expect_equal(class(ress[["ei"]][[1]]), "integer",
                 info = paste("fun = ", cat_fun))
    expect_equal(class(ress[["en"]][[1]]), "numeric")
    expect_equal(class(ress[["ec"]][[1]]), "character")
    expect_equal(class(ress[["ef"]][[1]]), "factor")
    ress_noid <- lapply(ress, `[`, -1L)
    expect_identical(ress_noid[["i"]], ress_noid[["n"]])
    expect_identical(ress_noid[["i"]], ress_noid[["c"]])
    expect_identical(ress_noid[["i"]], ress_noid[["f"]])
    expect_identical(ress_noid[["ei"]], ress_noid[["en"]])
    expect_identical(ress_noid[["ei"]], ress_noid[["ec"]])
    expect_identical(ress_noid[["ei"]], ress_noid[["ef"]])
  }
})
