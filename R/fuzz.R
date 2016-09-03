# try using fuzzr package on some functions, ultimately this could replace and
# be more comprehensive than my scattergun tests, and make responses to bad data
# more consistent with less logic.
#
# This should never be included in the CRAN pacakge

icd_fuzz <- function() {
  if (!requireNamespace("fuzzr"))
    install.packages("fuzzr")

  mydf <- data.frame(visit_id = c("a", "b", "c", "a", "b", "d"),
                     icd9 = c("441", "412.93", "044.9", "250.0", "250.0", "250.0"),
                     stringsAsFactors = TRUE)

  fr <- fuzzr::fuzz_function(icd_charlson, "return_df", x = mydf, tests = fuzzr::test_lgl())
  knitr::kable(as.data.frame(fr))

}
