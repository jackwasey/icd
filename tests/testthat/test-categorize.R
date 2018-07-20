context("categorize")

test_that("categorize result is empty", {
  expect_equal(0,
               nrow(res <- categorize(one_icd10_pt[0, ],
                                      map = icd10_map_ahrq,
                                      id_name = "visit",
                                      code_name = "icd"))
  )
  expect_identical(colnames(res), names(icd10_map_ahrq))
  expect_equal(0,
               nrow(rdf <- categorize(one_icd10_pt[0, ],
                                      map = icd10_map_ahrq,
                                      id_name = "visit",
                                      code_name = "icd",
                                      return_df = TRUE))
  )
  expect_identical(c("visit", names(icd10_map_ahrq)), colnames(rdf))
})

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
  for (cat_fun in c("categorize", "categorize_simple")) {
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
    expect_equal(class(ress[["ei"]][[1]]), "integer", info = paste("fun = ", cat_fun))
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

test_that("factor split basics", {
  df <- data.frame(visit_id = factor(c("visit1", "visit2", "visit1")),
                   icd_code = factor(c("410", "0010", "E999")))
  expect_equal(
    length(
      res <- factor_split_rcpp(df, "410", "visit_id", "icd_code")),
    2)
  expect_equal(
    length(
      res_empty <- factor_split_rcpp(df, "999", "visit_id", "icd_code")),
    2)
  expect_equal(dim(res$comorbid_df), c(1, 2))
  expect_equal(dim(res_empty$comorbid_df), c(0, 2))
  expect_equal(res$unique_no_comorbid, "visit2")
  expect_setequal(res_empty$unique_no_comorbid, c("visit1", "visit2"))
})

test_that("factor split with no relevant codes", {
  res <- factor_split_rcpp(random_icd10_pts, character(0), "visit_id", "icd10")
  expect_equivalent(
    res[["comorbid_df"]],
    data.frame(visit_id = integer(0),
               icd10 = factor(integer(0), levels = character(0)),
               stringsAsFactors = FALSE))
})
