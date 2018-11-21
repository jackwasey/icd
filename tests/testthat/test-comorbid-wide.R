
context("test direct wide data comorbidity calcs")

wide_dat <- data.frame(vis = c("1000", "1001"),
                       dx1 = c("41003", "39891"), # MI, CHF (charlson)
                       dx2 = c("0930", "43001"),
                       stringsAsFactors = FALSE) #PVD, Stroke (charlson)
charlson_minimap <- lapply(icd9_map_charlson[1:5], head)

test_that("simple wide data calc", {
  res <- icd:::comorbidMatMulWide(wide_dat, charlson_minimap,
                           id_name = "vis", code_names = c("dx1", "dx2"))
  expect_equivalent(res["1000", ], c(TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_equivalent(res["1001", ], c(FALSE, TRUE, FALSE, TRUE, FALSE))
})
