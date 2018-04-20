//' @examples
//' if (require(microbenchmark)) {
//'   stopifnot(identical(
//'     icd:::icd9_add_leading_zeroes_alt_cpp(c("1", "E2", "V1", "E"), short_code = TRUE),
//'     icd:::icd9_add_leading_zeroes_cpp(c("1", "E2", "V1", "E"), short_code = TRUE)
//'     ))
//'
//'   bad_codes <- sample(c("E2", "V01", "1234", "12", "1", "E99", "E987", "V"),
//'                       size = 1e4, replace = TRUE)
//'   microbenchmark::microbenchmark(
//'     icd:::icd9_add_leading_zeroes_alt_cpp(bad_codes, short_code = TRUE),
//'     icd:::icd9_add_leading_zeroes_cpp(bad_codes, short_code = TRUE)
//'   )
//' }
