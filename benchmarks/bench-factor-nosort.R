library(icd)
# benchmarking factor generation with known levels and no sorting:
n = 1e7
random_short_icd10_codes <- sample(unlist(icd::icd10_map_elix),
                                   replace = TRUE, size = n)
# getting unique levels is fast, 0.1 seconds for 1e7 codes
system.time(lvls <- unique(random_short_icd10_codes))
microbenchmark::microbenchmark(
  icd:::factor_nosort_rcpp(random_short_icd10_codes, lvls),
  icd:::factor_nosort(random_short_icd10_codes, lvls),
  factor(random_short_icd10_codes, lvls),
  times = 5
)
# Rcpp is three times as fast.

# an even fast way might be to use the existing pointer values in the global
# string cache directly...

x <- c("z", "a", "123")
icd:::factor_nosort(x)
# should return a factor without modification
x <- as.factor(x)
identical(icd:::factor_nosort(x), x)
# unless the levels change:
icd:::factor_nosort(x, levels = c("a", "z"))

# existing factor levels aren't re-ordered without also moving elements
f <- factor(c("a", "b", "b", "c"))
g <- icd:::factor_nosort(f, levels = c("a", "c", "b"))
stopifnot(g[4] == "c")
pts <- icd:::random_unordered_patients(1e6)
u <- unique.default(pts$code)
# this shows that stringr (which uses stringi) sort takes 50\% longer than
# built-in R sort.
microbenchmark::microbenchmark(sort(u), str_sort(u))

# this shows that \\code{factor_nosort} is about 50\% faster than \\code{factor}
# for big vectors of strings

# without sorting is much faster:
microbenchmark::microbenchmark(factor(pts$code),
                               # factor_(pts$code),
                               factor_nosort(pts$code),
                               times = 25)
