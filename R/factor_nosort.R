#' Fast Factor Generation
#'
#' This function generates factors more quickly, without leveraging
#' \pkg{fastmatch}. The speed increase with \pkg{fastmatch} for ICD-9 codes
#' was about 33% reduction for 10 million codes. SOMEDAY could be faster still
#' using \pkg{Rcpp}, and a hashed matching algorithm.
#'
#' \code{NaN}s are converted to \code{NA} when used on numeric values. Extracted
#' from https://github.com/kevinushey/Kmisc.git
#'
#' These feature from base R are missing: \code{exclude = NA, ordered =
#' is.ordered(x), nmax = NA}
#' @author Kevin Ushey, adapted by Jack Wasey
#' @param x An object of atomic type \code{integer}, \code{numeric},
#'   \code{character} or \code{logical}.
#' @param levels An optional character vector of levels. Is coerced to the same
#'   type as \code{x}. By default, we compute the levels as
#'   \code{sort(unique.default(x))}.
#' @param labels A set of labels used to rename the levels, if desired.
#' @examples
#' x <- c("z", "a", "123")
#' icd:::factor_nosort(x)
#' # should return a factor without modification
#' x <- as.factor(x)
#' identical(icd:::factor_nosort(x), x)
#' # unless the levels change:
#' icd:::factor_nosort(x, levels = c("a", "z"))
#'
#' # existing factor levels aren't re-ordered without also moving elements
#' f <- factor(c("a", "b", "b", "c"))
#' g <- icd:::factor_nosort(f, levels = c("a", "c", "b"))
#' stopifnot(g[4] == "c")
#' @details I don't think there is any requirement for factor levels to be
#'   sorted in advance, especially not for ICD-9 codes where a simple
#'   alphanumeric sorting will likely be completely wrong.
#' @keywords internal
factor_nosort <- function(x, levels) {
  if (missing(levels)) {
    if (is.factor(x)) return(x) else levels <- unique.default(x)
  }
  suppressWarnings(f <- match(x, levels))
  levels(f) <- as.character(levels)
  class(f) <- "factor"
  f
}

#' @describeIn factor_nosort R wrapper to the \pkg{Rcpp} function. Will
#'   re-factor a factor with new levels without converting to string vector.
#' @param na.rm Logical, if `TRUE`, simple drop all NA values, i.e., values with
#'   no corresponding level.
#' @md
#' @keywords internal
factor_nosort_rcpp <- function(x, levels, na.rm = FALSE) {
  # TODO: if re-factoring, use my refactor code
  if (missing(levels)) {
    if (is.factor(x))
      return(x)
    else
      levels <- unique.default(x)
  }
  if (na.rm)
    levels <- levels[!is.na(levels)]
  factor_nosort_rcpp_worker(as.character(x), levels, na_rm = na.rm)
  #TODO SLOW - if re-factoring, there is a faster way
}

#' Refactor by integer matching levels in C++
#'
#' Slightly slower for small factors, three times faster for one hundred million
#' elements with two million new levels. Three times faster for any `n > 1e6`.
#' With `NA` values, margin is smaller, but still beats base `factor`.
#' @param exclude_na Simpler equivalent to `base::factor` exclude. By default,
#'   `refactor` will not count `NA` as a factor level if there are `NA` elements
#'   in the input data.
#' @examples
#' \dontrun{
#'   f <- factor(c(1, 2, 3))
#'   icd:::refactor(f, c("2", "3"))
#'   f <- factor(c(1, 2, NA))
#'   icd:::refactor(f, c("2", "3", NA))
#' }
#' @md
#' @keywords internal manip
refactor <- function(x, levels, na.rm = FALSE, exclude_na = TRUE) {
  checkmate::assert_factor(x)
  checkmate::assert_character(levels)
  if (na.rm)
    refactor_narm_worker(x, levels)
  else
    refactor_worker(x, levels, exclude_na)
}
