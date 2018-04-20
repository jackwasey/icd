#' \dontrun{
#' # let's do five million patients and benchmark
#' big <- icd:::generate_random_pts(5E6)
#' microbenchmark::microbenchark(
#'   charlson(big),
#'   times = 5
#' )
#' }

