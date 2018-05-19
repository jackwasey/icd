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
factor_nosort <- function(x, levels = unique.default(x)) {
  suppressWarnings(f <- match(x, levels))
  levels(f) <- as.character(levels)
  class(f) <- "factor"
  f
}
