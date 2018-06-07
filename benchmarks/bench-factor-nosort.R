library(icd)
library(microbenchmark)
##########################
# factoring and matching #
##########################

n = 1e7
random_short_icd10_codes <- sample(unlist(icd::icd10_map_elix),
                                   replace = TRUE, size = n)
lookup <- unique(unname(unlist(icd10_map_quan_deyo)))
microbenchmark(match(random_short_icd10_codes, lookup),
               icd:::match_rcpp(random_short_icd10_codes, lookup),
               times = 10)
# yep, Rcpp is twice to thrice as fast for ten million items.


# benchmarking factor generation with known levels and no sorting:
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


################################
# benchmark refactoring in C++ #
################################

# see tests-slow to show the results are identical

# test various combos with NAs and mis-matches
n = 1e6
nl = n %/% 50L
times = 5
set.seed(1441)
v1 <- icd:::icd9RandomShort(n)
v2 <- v1
v2[1] <- "INVALID"
l1 <- sample(v1, size = nl)
l2 <- c(NA_character_, l1)
l3 <- c(l1, NA_character_)
l4 <- c(l1, "XXX")
l5 <- unique(icd:::icd9RandomShort(n * 2))
test_cases <- expand.grid(
  list(v1, v2),
  list(l1, l2, l3, l4, l5),
  list(l1, l2, l3, l4, l5))
# get empty mb to build
mb <- microbenchmark(NULL, times = 1)
mb <- mb[-1,]
for (tc in seq_along(test_cases[[1]])) {
  m <- test_cases[tc, 1][[1]]
  nl <- unique(test_cases[tc, 2][[1]])
  pl <- unique(test_cases[tc, 3][[1]])
  f <- factor(m, levels = pl)
  mb <- rbind(mb, microbenchmark(
    refactor(f, nl),
    factor(f, levels = nl),
    times = times
  ))
  mb <- rbind(mb, microbenchmark(
    refactor(f, nl, na.rm = FALSE, exclude_na = FALSE),
    factor(f, levels = nl, exclude = NULL),
    times = times
  ))
}
print(mb)

# test one huge factor
n = 1e8
nl = n %/% 50L
times = 5
set.seed(1441)
v1 <- icd:::icd9RandomShort(n)
l1 <- unique(sample(v1, size = nl))
l2 <- unique(sample(v1, size = nl))
f <- factor(v1, l1)
microbenchmark(
  refactor(f, l2),
  factor(f, levels = l2),
  times = times)
