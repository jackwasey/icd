context("fast scoring system tests")

test_that("count wide directly (old func) same as reshape count", {

  widedf <- data.frame(visitId = c("a", "b", "c"),
                       icd9_01 = c("441", "4424", "441"),
                       icd9_02 = c(NA, "443", NA))

  # we don't get names back for the vector for 'long'
  expect_equivalent(icd9CountWide(widedf),
                    widedf %>% icd9WideToLong %>% icd9Count)

})
