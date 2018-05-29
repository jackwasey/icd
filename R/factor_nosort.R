#' Fast Factor Generation
#'
#' This function generates factors more quickly, without leveraging
#' \code{fastmatch}. The speed increase with \code{fastmatch} for ICD-9 codes
#' was about 33% reduction for 10 million codes. SOMEDAY could be faster still
#' using \code{Rcpp}, and a hashed matching algorithm.
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

#' @describeIn factor_nosort R wrapper to the Rcpp function. Will re-factor a
#'   factor with new levels without converting to string vector.
#' @param na.rm Logical, if `TRUE`, simple drop all NA values, i.e. values with
#'   no corresponding level.
#' @md
#' @keywords internal
factor_nosort_rcpp <- function(x, levels, na.rm = FALSE) {
  if (missing(levels)) {
    if (is.factor(x))
      return(x)
    else
      levels <- unique.default(x)
  }
  factor_nosort_rcpp_worker(as.character(x), levels, na_rm = na.rm)
  #TODO SLOW - if re-factoring, there is a faster way
}

#' (re-)factor and split into matched and unmatched elements
#'
#' Critically, this function does not convert factor to character vector and
#' back to factor in order to modify factor levels, resulting in huge speed
#' improvements for long vectors.
#' @examples
#' icd:::factor_nosort_rcpp(c("1", NA)) # NA becomes a level
#' icd:::factor_nosort_rcpp(c("1", "2"), "1") # NA not a level, just dropped!
#' icd:::factor_nosort_rcpp(c("1", "2"), c("1", NA)) # NA IS a level
#' suppressWarnings(
#'   print(icd:::factor_nosort_rcpp(c("1", "2"), c("1", NA, NA)))
#' ) # Two NA levels
#'
#' x <- c("A", "B", "C", "d", "A", "C")
#' levels <- c("A", "B")
#' stopifnot(
#'   identical(icd:::factor_split_na(factor(x), levels),
#'             icd:::factor_split_na(x, levels))
#' )
#' y <- c("A", NA, "B", "A", NA)
#' yf <- factor(y)
#' yf_na <- factor(y, levels = c("A", NA, "B"), exclude = NULL)
#' stopifnot(
#'   identical(icd:::factor_split_na(y, "A"),
#'             icd:::factor_split_na(yf, "A"))
#' )
#' stopifnot(
#'   identical(icd:::factor_split_na(y, "A"),
#'             icd:::factor_split_na(yf_na, "A"))
#' )
#' @keywords internal manip
factor_split_na <- function(x, levels, factor_fun = factor_nosort_rcpp) {
  # input may have no levels!
  if (is.factor(x)) {
    xi <- as.integer(x)
    lx <- levels(x)
    any_na_xlevels <- anyNA(lx)
    no_na_xlevels <- if (any_na_xlevels) lx[!is.na(lx)] else lx
    new_level_idx <- match(no_na_xlevels, levels)
    f <- new_level_idx[xi]
    inc_mask <- !is.na(f)
    f <- f[inc_mask]
    attr(f, "levels") <- levels
    class(f) <- "factor"
  } else {
    f <- factor_fun(x, levels)
    inc_mask <- !is.na(f)
    f <- f[inc_mask]
  }
  list(factor = f, inc_mask = inc_mask)
}

#' Refactor by integer matching levels in C++
#' @examples
#' \donttest{
#'
#' }
#' @keywords internal manip
refactor <- function(x, levels, na.rm = FALSE, exclude_na = TRUE) {
  checkmate::assert_factor(x)
  checkmate::assert_character(levels)
  refactor_worker(x, levels, na_rm = na.rm, exclude_na)
}
