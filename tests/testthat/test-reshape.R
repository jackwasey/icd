context("test reshaping wide to long")

longdf <- data.frame(visitId = c("a", "b", "b", "c"),
                     icd9 = c("441", "4424", "443", "441"))

widedf <- data.frame(visitId = c("a", "b", "c"),
                     icd9_01 = c("441", "4424", "441"),
                     icd9_02 = c(NA, "443", NA))

test_that("long data to wide data", {

  expect_equal(icd9LongToWide(longdf),
               data.frame(visitId = c("a", "b", "c"),
                          icd_01 = c("441", "4424", "441"),
                          icd_02 = c(NA, "443", NA)))

  longdf2 <- data.frame(i = c("441", "4424", "443", "441"),
                        v = c("a", "b", "b", "c"))
  expect_equal(names(icd9LongToWide(longdf2,
                                    visitId = "v",
                                    icdId = "i",
                                    prefix = "ICD10_",
                                    empty = "")),
               c("v", "ICD10_01", "ICD10_02"))
})

test_that("wide data to long data", {
  expect_equivalent(icd9WideToLong(widedf),
               longdf)

  widedfempty <- data.frame(visitId = c("a", "b", "c"),
                       icd9_01 = c("441", "4424", "441"),
                       icd9_02 = c("", "443", ""))

  expect_equivalent(icd9WideToLong(widedfempty),
                    longdf)
  expect_equal(icd9WideToLong(widedfempty),
               icd9WideToLong(widedfempty))

})
