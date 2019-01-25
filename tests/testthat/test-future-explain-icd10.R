context("explain ICD-10-CM codes")

expect_icd10cm_explain_one <- function(...) {
  x <- list(...)
  for (i in seq_along(x))
    eval(bquote(
      expect_identical(explain_code(as.icd10cm(.(names(x[i]))), x[[i]]))))
}
