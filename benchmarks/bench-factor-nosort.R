library(icd)
# benchmarking factor generation with known levels and no sorting:
n = 1e7
random_short_icd10_codes <- sample(unlist(icd::icd10_map_elix), replace = TRUE, size = n)
lvls <- unique(random_short_icd10_codes)

microbenchmark::microbenchmark(
  icd:::factor_nosort_rcpp(random_short_icd10_codes, lvls),
  icd:::factor_nosort(random_short_icd10_codes, lvls),
  factor(random_short_icd10_codes, lvls),
  times = 10
)



## below needs tidying up



<- unname(unlist(icd9_map_ahrq))
 codes <- c(codes, icd:::randomMajorCpp(1e7))
 microbenchmark::microbenchmark(
				  icd:::factor_fast(codes),
				    icd:::factor_nosort(codes),
				    factor(codes),
				      times = 10
				      )
 # the R factor_nosort is about as fast as Rcpp version

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

	 # this shows that \\code{factor_} is about 50\% faster than \\code{factor} for
	 # big vectors of strings

	 # without sorting is much faster:
	 microbenchmark::microbenchmark(factor(pts$code),
					                               # factor_(pts$code),
					                               factor_nosort(pts$code),
								                                      times = 25)

	 //' @examples
	 //' \dontrun{
	 //' codes <- unname(unlist(icd9_map_ahrq))
	 //' codes <- c(codes, icd:::randomMajorCpp(1e7))
	 //' microbenchmark::microbenchmark(
	 //'   icd:::factor_fast(codes),
	 //'   icd:::factor_nosort(codes),
	 //'   factor(codes),
	 //'   times = 10
	 //'   )
	 //' # the R factor_nosort is about as fast as Rcpp version
	 //' }

