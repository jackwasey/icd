context("github #138")

test_that("github #138 doesn't crash R", {
  # # don't do this test on CRAN, because I don't want (yet) to pull in dplyr
  # skip_on_cran()
  # pkgs <- c("dplyr", "tidyr")
  # for (p in pkgs)
  #   library(p, character.only = TRUE)
  #
  # data_2015_10_test_long <- read.csv("../../github138_tiny_slice.txt")
  # #saveRDS(data_2015_10_test_long, "github138.rds")
  # #data_2015_10_test_long <- readRDS(system.file("tests", "github138.rds",
  # #                                              package = "icd", mustWork = TRUE))
  #
  # # code from Olga's bug report follows:
  # data_diag_elix_test_m <- data_2015_10_test_long %>%
  #   gather(diag_field, icd10, -CLAIMNO) %>%
  #   arrange(CLAIMNO) %>% icd_long_data
  #
  # saveRDS(data_diag_elix_test_m, file.path(system.file("tests", "testthat", package = "icd"), "github138-b.rds"))

  x <- readRDS(system.file("tests", "testthat", "github138-b.rds", package = "icd"))

  res <- icd10_comorbid(x,
                        icd10_map_ahrq,
                        visit_name = 'CLAIMNO',
                        icd_name = "icd10",
                        aggregate = FALSE)
})
