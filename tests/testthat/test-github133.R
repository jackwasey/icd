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
})

test_that("github #133 possibly related bug", {
  # just run to check it doesn't segfault
  pts10 <- icd::icd_long_data(
    visit = c("a"),
    icd = c(""),
    date = as.Date(c("2011-01-01")));
  icd::icd10_comorbid(pts10, map = icd::icd10_map_ahrq)

})
