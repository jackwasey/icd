#' \dontrun{
#' # benchmark attributes vs attr for getting and setting
#' rp <- "a"
#' new_attr <- list(k = "b")
#' times <- 1e5
#' microbenchmark::microbenchmark(attr(rp, "k") <- "b", attributes(rp) <- new_attr, times = times)
#' microbenchmark::microbenchmark(attr(rp, "k"), attributes(rp)[["k"]], times = times)
#' microbenchmark::microbenchmark(attr(rp, "k"), attributes(rp), times = times)
#' microbenchmark::microbenchmark(attr(rp, "k", exact = TRUE), attributes(rp), times = times)
#' }

#'
#' \dontrun{
#' # benchmark subsetting to justify using .subset2 (5% faster)
#' library(microbenchmark)
#' j <- list(as.icd9cm("E990"), as.icd9cm("10010"))
#' k <- list(rep(as.icd9cm("E990"), times = 500))
#' microbenchmark(j[[1]], .subset2(j, 1),
#'                k[[1]], .subset2(k, 1),
#'                times = 1e5)
#'
#' # logical list to vector
#' a <- list(TRUE, TRUE)
#' microbenchmark(as.logical(a), c(a, recursive = TRUE), times = 1e5)
#'
#' # c(..., recursive = TRUE) vs unlist
#' l = list(c("100", "440", "999"), c("123", "234"))
#' microbenchmark::microbenchmark(c(l, recursive = TRUE),
#'                                c(unlist(l)),
#'                                times = 1e5)
#' stopifnot(identical(c(l, recursive = TRUE), c(unlist(l))))
#'
#' }
