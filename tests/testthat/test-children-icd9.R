context("icd9 children")

test_that("NA is not expanded for children undefined", {
  expect_equivalent(children(c("27900", NA), defined = FALSE),
                    "27900")

