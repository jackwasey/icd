#' Get or set whether ICD codes have have an attribute indicating 'short' or
#' 'decimal' format
#'
#' @section Getting the attribute: \code{is.short_diag} tests for presence of an
#'   attribute, not whether the code is a valid ICD code. To test validity of
#'   the codes themselves, see \code{\link{is_valid}}.
#' @section Setting the attribute: Similarly, \code{as.icd_short_diag} and
#'   \code{as.icd_decimal_diag} set the attribute, but do not convert the codes
#'   themselves. For conversion between 'short' and 'decimal' forms, use
#'   \code{\link{decimal_to_short}} and \code{\link{short_to_decimal}}.
#'
#'   The attribute \code{icd_short_code} should be either \code{TRUE} or
#'   \code{FALSE}. There is no attribute named \code{icd_decimal_code}. These
#'   functions set and get the attribute safely. If the attribute is not
#'   present, both \code{is.icd_short_diag} and \code{is.icd_decimal_diag} (or
#'   their synonyms \code{is.short_diag} and \code{is.decimal_diag}) will return
#'   \code{FALSE}.
#' @param x ICD data
#' @param value \code{TRUE} or \code{FALSE}, default is \code{TRUE} which sets
#'   the attribute to whatever is indicated in the function name. See examples.
#' @examples
#' library(icd)
#' as.icd_short_diag("6670")
#' as.icd_short_diag("667.0") # no warning or error!
#' is.icd_short_diag(decimal_to_short("667.0"))
#' decimal_type_code <- as.icd_short_diag("667.0", FALSE)
#' stopifnot(is.icd_decimal_diag(decimal_type_code))
#' codes <- as.icd9(c("100.1", "441.3"))
#' codes <- as.decimal_diag(codes)
#' codes
#' @export
as.decimal_diag <- function(x, value = TRUE) {
  as.icd_decimal_diag(x, value)
}

#' @rdname as.decimal_diag
#' @export
as.icd_decimal_diag <- function(x, value = TRUE) {
  assert_flag(value)
  attr(x, "icd_short_diag") <- !value
  x
}

#' @rdname as.decimal_diag
#' @export
as.short_diag <- function(x, value = TRUE) {
  as.icd_short_diag(x, value)
}

#' @rdname as.decimal_diag
#' @export
as.icd_short_diag <- function(x, value = TRUE) {
  assert_flag(value)
  attr(x, "icd_short_diag") <- value
  x
}

#' @rdname as.decimal_diag
#' @export
is.decimal_diag <- function(x) {
  is.icd_decimal_diag(x)
}

#' @rdname as.decimal_diag
#' @export
is.icd_decimal_diag <- function(x) {
  # isFALSE is not available until R 3.4
  y <- attr(x, "icd_short_diag", exact = TRUE)
  is.logical(y) && length(y) == 1L && !is.na(y) && !y
}

#' @rdname as.decimal_diag
#' @export
is.short_diag <- function(x) {
  is.icd_short_diag(x)
}

#' @rdname as.decimal_diag
#' @export
is.icd_short_diag <- function(x) {
  isTRUE(attr(x, "icd_short_diag", exact = TRUE))
}

#' Remove any attributes set by 'icd'
#' @keywords internal
#' @noRd
icd_attr_clean <- function(x) {
  attr(x, "icd_short_diag") <- NULL
  x
}
