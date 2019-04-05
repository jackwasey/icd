#' @title Sort or order ICD-9 or ICD-10 codes according to published sequence
#' @description The default method will guess whether ICD-9 or ICD-10  then sort
#'   based on that type. For ICD-10 codes, note that setting \code{short} is
#'   unnecessary and ignored. All codes should consistently use the decimal
#'   divider.
#' @section ICD-9: Sorts lists of numeric, V or E codes. Note that a simple
#'   numeric sort does not work for ICD-9 codes, since \code{162 > 1620}, and
#'   also \sQuote{V} codes precede \sQuote{E} codes. Numeric codes are first,
#'   then \sQuote{V}, then \sQuote{E}. A factor is returned if a factor is
#'   given.
#' @section ICD-10-CM and ICD-10-BE: There are some codes which are sequenced
#'   out of lexicographic order, e.g., \code{C7A} and \code{C7B} are between
#'   \code{C80} and \code{C81}; \code{D3A} is between \code{D48} and \code{D49}.
#' @details Note that \code{\link[base]{sort}} is an S3 generic, whereas
#'   \code{\link[base]{order}} is not. Thus we export \code{order.icd10cm}, but
#'   not \code{sort.icd10cm}, etc..
#' @param x vector of ICD codes to sort or order
#' @param decreasing Logical See \code{\link[base]{sort}}.
#' @param na.last Logical, analogous to \code{order}, so \code{NA} drops NA.
#'   \code{FALSE} is not currently supported.
#' @template short_code
#' @template dotdotdot
#' @examples
#' # order ICD-10-CM is not lexicographic:
#' codes <- as.icd10cm(c("C7A", "C79", "C80", "C81", "C7B"))
#' # as the class is set, use S3 dispatch to get the right answer
#' sort(codes)
#' # or call directly, but recall S3 dispatch will only work once 'icd' is
#' # attached using:
#' library(icd)
#' icd:::sort.icd10cm(c("C7A", "C79", "C80", "C81", "C7B"))
#' stopifnot(!identical(
#'   order.icd10cm(as.character(codes)),
#'   order(codes)
#' ))
#' icd::order.icd9(c("V20", NA, "100", NA, "E998", "101"))
#' codes[order.icd10cm(codes)]
#' # Note that base::order does NOT do S3 dispatch, so the following does not work:
#' codes[order(codes)]
#' @return For sort, a sorted vector of ICD-9 codes. Numeric, then E codes, then
#'   V codes. For order, an integer vector is returned with the order of each
#'   code.
#' @keywords manip
#' @export
sort_icd <- function(x,
                     decreasing = FALSE,
                     short_code = guess_short(x),
                     ...) {
  switch(
    guess_version(x, short_code = short_code),
    "icd9" = sort.icd9(x, decreasing = decreasing, short_code),
    "icd10" = sort.icd10(x, decreasing = decreasing, short_code),
    stop("ICD version not known")
  )
}

#' @rdname sort_icd
#' @export
sort.icd10 <- function(x,
                       decreasing = FALSE,
                       ...) {
  res <- sort(x)
  # names are preserved, but using attributes would overwrite
  attr(res, "icd_short_diag") <- attr(x, "icd_short_diag")
  class(res) <- class(x)
  res
}

#' @rdname sort_icd
#' @export
sort.icd10cm <- function(x,
                         decreasing = FALSE,
                         ...) {
  # ignore short, it doesn't matter
  o <- icd10cm_order_rcpp(x)
  if (decreasing) o <- rev(o)
  res <- x[o]
  attr(res, "icd_short_diag") <- attr(x, "icd_short_diag")
  names(res) <- names(x)[o]
  class(res) <- class(x)
  res
}

#' @rdname sort_icd
#' @export
sort.icd10be <- function(x,
                         decreasing = FALSE,
                         ...) {
  sort.icd10cm(x, decreasing = decreasing, ...)
}
#' @rdname sort_icd
#' @export
sort.icd9 <- function(x,
                      decreasing = FALSE,
                      short_code = guess_short(x),
                      ...) {
  # no assertions here: they are slower than the actual sorting...
  y <- if (short_code) {
    x
  } else {
    decimal_to_short.icd9(x)
  }
  res <- if (is.factor(x)) {
    x[o <- icd9_order_rcpp(as_char_no_warn(y))]
  } else {
    x[o <- icd9_order_rcpp(y)]
  }
  o <- match(seq_along(x), o)
  if (decreasing) o <- rev(o)
  class(res) <- class(x)
  attributes(res) <- attributes(x)
  names(res) <- names(x)[o]
  res
}

# simple backport
isFALSE <- function(x)
  is.logical(x) && length(x) == 1L && !is.na(x) && !x

#' @rdname sort_icd
#' @export
order.icd9 <- function(x, na.last = TRUE) {
  if (isFALSE(na.last)) {
    stop("na.last = NA drops NA. na.last = FALSE not implemented.")
  }
  na <- is.na(x)
  n_na <- sum(na)
  if (is.na(na.last) && n_na != 0) {
    x <- x[!na]
    if (length(x) == 0) return(integer())
  }
  if (!is.factor(x)) {
    res <- icd9_order_rcpp(x)
  } else {
    res <- icd9_order_rcpp(as_char_no_warn(x))
  }
  res
}

#' @rdname sort_icd
#' @export
order.icd10cm <- function(x) {
  icd10cm_order_rcpp(x)
}

#' @rdname sort_icd
#' @export
order.icd10be <- function(x) {
  order.icd10cm(x)
}
