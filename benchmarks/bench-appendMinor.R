//' @examples
//' \dontrun{
//' n <- 5e6
//' mjrs <- as.character(sample(1:999, n, replace = TRUE))
//' mnrs <- as.character(sample(0:99, n, replace = TRUE))
//' microbenchmark::microbenchmark(
//'   icd9MajMinToCode(mjrs, mnrs, TRUE),
//'   icd9MajMinToCodeStd(mjrs, mnrs, TRUE),
//'   icd9MajMinToCodePrePadded(mjrs, mnrs, TRUE),
//'   times = 10
//' )
//' }
//' # std method about the same with O3 (4% faster, but no NA handling), but 50% quicker with O0
//' # std method without doing padding is 5 times quicker than previous...

