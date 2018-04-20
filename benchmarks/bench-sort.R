#' x <- icd:::generate_random_decimal_icd9(1e6)
#' system.time(icd:::icd9_sort_cpp(x)) # quicker
#' system.time(icd:::icd9_order_short(x))
#' \dontrun{
#'   # fastmatch was fractionally faster, but either is very slow
#'   microbenchmark(icd:::icd9_sort_cpp(x),
#'                  icd:::icd9_order_short(x),
#'                  icd:::icd9_order_short_r(x),
#'                  times = 10)
#'   # C++ method (which also ignores NA values) is 100x faster.
#' }
