nrow <- 1e5
a <- matrix(rnorm(nrow * 30) < 0.5, nrow = nrow, ncol = 30)
identical(rbind(a, matrix(FALSE, nrow = 1e5, ncol = 30)), icd:::rbind_with_empty(a, nrow))
microbenchmark::microbenchmark(
  rbind(a, matrix(FALSE, nrow = 1e5, ncol = 30)),
  icd:::rbind_with_empty(a, nrow),
  times = 5
)
