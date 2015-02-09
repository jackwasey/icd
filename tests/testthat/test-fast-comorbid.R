context("quick comorbidity test")
twoPts <- list(visitId = c("v01", "v01", "v02", "v02"),
               icd9 = c("040", "000", "100", "000"))
twoMap <- list("malady" = c("100", "2000"), "ailment" = c("003", "040"))

test_that("comorbid quick test", {
  testres <- icd9Comorbid(twoPts, twoMap)
  trueres <- data.frame("visitId" = c("v01", "v02"),
                        "malady" = c(FALSE, TRUE),
                        "ailment" = c(TRUE, FALSE),
                        stringsAsFactors = FALSE)
  print(testres)
  print(trueres)
  expect_equal(testres, trueres)

})
