context("github #133")
pts10 <- icd_long_data(
  visit = c("a"),
  icd = as.icd10(NA_character_),
  date = as.Date(c("2011-01-01")))
test_that("github #133 is NA also okay?", {
  # just run to check it doesn't segfault
  res <- icd::icd10_comorbid(pts10, map = icd::icd10_map_ahrq)
  expect_true(all(!res))
})
test_that("github #133 with NA", {
  pts10[["icd"]] <- NA
  res <- icd::icd10_comorbid(pts10, map = icd::icd10_map_ahrq)
  expect_true(all(!res))
})
test_that("github #133 minimal example of bug", {
  pts10 <- icd_long_data(
    visit = c("a"),
    icd = as.icd10(""),
    date = as.Date(c("2011-01-01")),
    stringsAsFactors = FALSE)
  res <- icd::icd10_comorbid(pts10, map = icd10_map_ahrq)
  expect_equal(dim(res), c(1L, 30L))
  expect_equal(sum(res), 0)
})
test_that("github #133 doesn't crash R", {
  f <- system.file("tests", "testthat", "github133-b.rds", package = "icd", mustWork = FALSE)
  if (f == "")
    skip("cannot load github133-b.rds from tests/testthat")

  x <- readRDS(f)
  # first NA in codes at row 15
  devnull <- icd10_comorbid(x[12:13, ], icd10_map_ahrq)
  devnull <- icd10_comorbid(x[13, ], icd10_map_ahrq)
  devnull <- icd10_comorbid(x[13:14, ], icd10_map_ahrq)
  devnull <- icd10_comorbid(x[14:15, ], icd10_map_ahrq)
  devnull <- icd10_comorbid(x[13:15, ], icd10_map_ahrq)
  devnull <- icd10_comorbid(x[15:16, ], icd10_map_ahrq)
  devnull <- icd10_comorbid(x[1:13, ], icd10_map_ahrq)
  devnull <- icd10_comorbid(x[1:14, ], icd10_map_ahrq)
  devnull <- icd10_comorbid(x[1:15, ], icd10_map_ahrq)
  # smaller problem to highlight a development issue:
  y <- x[x$CLAIMNO %in% c("8534028", "8534030") & !is.na(x$icd10), ]
  res1 <- icd10_comorbid_ahrq(y, visit_name = "CLAIMNO", icd_name = "icd10")
  expect_identical(rownames(res1), c("8534028", "8534030"))
  res <- icd10_comorbid(x, icd10_map_ahrq, visit_name = "CLAIMNO")
  expect_equal(dim(res), c(20, 30))
  expect_equal(
    rownames(res),
    c("8534023", "8534024", "8534025", "8534026", "8534027", "8534028",
      "8534029", "8534030", "8534031", "8534032", "8534033", "8534034",
      "8534035", "8534036", "8534037", "8534038", "8534039", "8534040",
      "8534041", "8534042"))
})
