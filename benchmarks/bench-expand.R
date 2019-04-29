microbenchmark::microbenchmark(
  icd9_expand_range_worker_alt_base("100", "114", icd9_short_n, TRUE, TRUE, TRUE),
  icd9_expand_range_worker("100", "114", icd9_short_n, TRUE, TRUE, TRUE),
  times = 5
)
