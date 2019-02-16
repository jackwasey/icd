#' @title Sort or get order of ICD-9 or ICD-10 codes according to published
#'   sequence
#' @description The default method will guess whether ICD-9 or ICD-10  then sort
#'   based on that type. For ICD-10 codes, note that setting \code{short} is
#'   unnecessary and ignored. All codes should consistently use the decimal
#'   divider.
#' @section ICD-9: Sorts lists of numeric, V or E codes. Note that a simple
#'   numeric sort does not work for ICD-9 codes, since "162" > "1620", and also
#'   V codes precede E codes. Numeric codes are first, then 'V', then 'E'. Will
#'   return a factor if a factor is given.
#' @section ICD-10-CM and ICD-10-BE: There are some codes which are sequenced
#'   out of lexicographic order, e.g., \code{C7A} and \code{C7B} are between
#'   \code{C75} and \code{C76}; \code{D3A} is between \code{D48} and \code{D49}.
#' @param x vector of ICD codes to sort or order
#' @template short_code
#' @template dotdotdot
#' @return For sort, a sorted vector of ICD-9 codes. Numeric, then E codes, then
#'   V codes. For order, an integer vector is returned with the order of each
#'   code.
#' @keywords manip
#' @export
sort_icd <- function(x, decreasing = FALSE, short_code = guess_short(x), ...) {
  switch(
    guess_version(x, short_code = short_code),
    "icd9" = sort.icd9(x, decreasing = decreasing, short_code),
    "icd10" = sort.icd10(x, decreasing = decreasing, short_code),
    stop("ICD version not known")
  )
}

#' @rdname sort_icd
#' @keywords internal
#' @export
sort.icd10 <- function(
  x,
  decreasing = FALSE,
  ...
) {
  r <- sort(x, index.return = TRUE)
  res <- r[["x"]]
  attributes(res) <- attributes(x)
  names(res) <- names(x)[r[["ix"]]]
  class(res) <- class(x)
  res
}

#' @rdname sort_icd
#' @keywords internal
#' @export
sort.icd10cm <- function(
  x,
  ...
) {
  # ignore short, it doesn't matter
  o <- icd10cm_order_rcpp(x)
  o <- match(seq_along(x), o)
  res <- x[o]
  attributes(res) <- attributes(x)
  names(res) <- names(x)[o]
  class(res) <- class(x)
  res
}

#' @rdname sort_icd
#' @keywords internal
#' @export
sort.icd9 <- function(
  x,
  decreasing = FALSE,
  short_code = guess_short(x),
  ...
) {
  # no assertions here: they are slower than the actual sorting...
  y <- if (short_code)
    x
  else
    decimal_to_short.icd9(x)
  res <- if (is.factor(x))
    x[o <- icd9_order_rcpp(as_char_no_warn(y))]
  else
    x[o <- icd9_order_rcpp(y)]
  o <- match(seq_along(x), o)
  class(res) <- class(x)
  attributes(res) <- attributes(x)
  names(res) <- names(x)[o]
  if (decreasing)
    rev(res)
  else
    res
}

#' @rdname sort_icd
#' @export
order.icd9 <- function(x) {
  if (anyNA(x)) {
    warning("Dropping NA values")
    x <- x[!is.na(x)]
    if (length(x) == 0) return(integer())
  }
  icd9_order_rcpp(x)
}

#' @rdname sort_icd
#' @examples
#' # order ICD-10-CM is not lexicographic:
#' codes <- c("C7A", "C74", "C75", "C76", "C7B")
#' stopifnot(!identical(order(codes), icd::order.icd10cm(codes)))
#' codes[icd::order.icd10cm(codes)]
#' @export
order.icd10cm <- function(x) {
  icd10cm_order_rcpp(x);
}


#' @rdname sort_icd
#' @export
order.icd10be <- function(x) {
  order.icd10cm(x)
}
