#' Sort short-form ICD-9 codes
#'
#' Sorts lists of numeric only, V or E codes. Note that a simple numeric sort
#' does not work for ICD-9 codes, since "162" > "1620", and also V codes precede
#' E codes.
#' @details Implementation used fast built-in sort, then shuffles the E codes to
#'   the end.
#' @param x vector of ICD codes to sort
#' @template short_code
#' @template dotdotdot
#' @return sorted vector of ICD-9 codes. Numeric, then E codes, then V codes.
#' @keywords manip
#' @export
sort_icd <- function(x, ...)
  UseMethod("sort_icd")

#' @describeIn sort_icd Guess whether ICD-9 or ICD-10 (or possibly sub-type in
#'   the future) then sort based on that type. ICD-10 codes, note that setting
#'   \code{short} is unnecessary and ignored.
#' @export
#' @keywords internal
sort_icd.default <- function(x, short_code = guess_short(x), ...) {
  switch(
    guess_version(x, short_code = short_code),
    "icd9" = sort_icd.icd9(x, short_code),
    "icd10" = sort_icd.icd10(x, short_code),
    stop("ICD version not known")
  )
}

#' @describeIn sort_icd Sort ICD-10 codes, note that setting \code{short} is
#'   unnecessary and ignored.
#' @keywords internal
#' @export
sort_icd.icd10 <- function(x, short_code = NULL, ...) {
  # ignore short, it doesn't matter
  sort(x)
}

#' @describeIn sort_icd sort ICD-9 codes respecting numeric, then 'V', then 'E'
#'   codes, and accounting for leading zeroes. Will return a factor if a factor is given.
#' @keywords internal
#' @export
sort_icd.icd9 <- function(x, short_code = guess_short(x), ...) {
  # no assertions here: they are slower than the actual sorting...
  y <- if (short_code)
    x
  else
    decimal_to_short.icd9(x)
  res <- if (is.factor(x))
    x[icd9_order_cpp(as_char_no_warn(y))]
  else
    x[icd9_order_cpp(y)]
  class(res) <- class(x)
  keep_names <- names(res)
  attributes(res) <- attributes(x)
  names(res) <- keep_names
  res
}

#' Get order of a vector of ICD codes
#'
#' @section ICD-9: Puts E codes after V codes. \code{NA} values can't be ordered
#'   and are dropped with a warning if found.
#' @section ICD-10-CM: there are some codes which are sequenced out of
#'   lexicographic order, e.g., \code{C7A} and \code{C7B} are between \code{C75}
#'   and \code{C76}; \code{D3A} is between \code{D48} and \code{D49}.
#' @param x vector or factor of ICD-9 codes
#' @return vector of integers with length of the non-NA values in \code{x}
#' @export
order.icd9 <- function(x) {
  if (anyNA(x)) {
    warning("Dropping NA values")
    x <- x[!is.na(x)]
    if (length(x) == 0) return(integer())
  }
  icd9_order_cpp(x)
}

#' @rdname order.icd9
#' @examples
#' # order ICD-10-CM is not lexicographic:
#' codes <- c("C7A", "C74", "C75", "C76", "C7B")
#' stopifnot(!identical(order(codes), icd::order.icd10cm(codes)))
#' codes[icd::order.icd10cm(codes)]
#' @export
order.icd10cm <- function(x) {
  icd10cm_order_cpp(x);
}

order.icd10be <- function(x) {
 order.icd10cm(x)
}
