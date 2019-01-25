context("compare ordered long to wide methods")

pts <- generate_random_ordered_pts(5000, 13)

test_that("ordered and unordered methods on ordered data are identical", {
  ord <- long_to_wide(pts)
  expect_true(all(ord$visit_id %in% pts$visit_id))
  expect_true(all(pts$visit_id %in% ord$visit_id))
})

test_that("disordered rows", {
  df <- structure(
    list(visit_id = c("v2", "v4", "v3", "v2", "v2", "v4"),
         icd9 = structure(c("39891", "0932", "4151", "440", "4011", "4010"),
                          class = c("icd9", "character"))),
    row.names = c(NA, 6L),
    class = c("icd_long_data", "data.frame"))
  df_wide <- long_to_wide(df)
  expect_equivalent(long_to_wide(df),
                   structure(list(visit_id = c("v2", "v3", "v4"),
                                  icd_001 = structure(c("39891", "4151", "0932"),
                                                      class = c("icd9", "character")),
                                  icd_002 = structure(c("440", NA, "4010"),
                                                      class = c("icd9", "character")),
                                  icd_003 = structure(c("4011", NA, NA),
                                                      class = c("icd9", "character"))),
                             row.names = c(1L, 3L, 2L),
                             class = c("icd_wide_data", "icd_long_data", "data.frame")
                             ))
})
