context("github #133")

test_that("github #133 doesn't crash R", {
  f <- system.file("tests", "testthat", "github133-b.rds", package = "icd", mustWork = FALSE)
  if (f == "")
    skip("cannot load github133-b.rds from tests/testthat")

  x <- readRDS(f)
  res <- icd10_comorbid(x,
                        icd10_map_ahrq,
                        visit_name = "CLAIMNO",
                        icd_name = "icd10",
                        aggregate = FALSE)
  expect_equal(dim(res), c(20, 30))
  expect_equal(
    rownames(res),
    c("8534023", "8534024", "8534025", "8534026", "8534027", "8534028",
      "8534029", "8534030", "8534031", "8534032", "8534033", "8534034",
      "8534035", "8534036", "8534037", "8534038", "8534039", "8534040",
      "8534041", "8534042"))
})

test_that("github #133 minimal example of bug", {
  pts10 <- icd_long_data(
    visit = c("a"),
    icd = as.icd10(""),
    date = as.Date(c("2011-01-01")),
    stringsAsFactors = FALSE);
  res <- icd::icd10_comorbid(pts10, map = icd10_map_ahrq)
  expect_equal(dim(res), c(1L, 30L))
  expect_equal(sum(res), 0)
})

{# nolint
  pts10 <- icd_long_data(
    visit = c("a"),
    icd = as.icd10(NA_character_),
    date = as.Date(c("2011-01-01")));

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
}
