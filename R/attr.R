icd_short_diag <- function(x) {
  attr(x, "icd_short_diag") <- TRUE
  x
}

icd_decimal_diag <- function(x) {
  attr(x, "icd_short_diag") <- FALSE
  x
}

#' @rdname is.icd9
#' @details \code{is.icd_short_diag} tests for presence of an attribute, not whether the code is a valid ICD code.
#' @export
is.icd_short_diag <- function(x) {
  attr(x, "icd_short_diag", exact = TRUE)
}

#' @rdname is.icd9
#' @export
is.icd_decimal_diag <- function(x) {
  res <- attr(x, "icd_short_diag", exact = TRUE)
  if (is.null(res))
    return()
  else
    !res
}

#' @rdname is.icd9
#' @examples
#' \dontrun{
#' f <- function(x) {attr(x, "jack") <- TRUE; x}
#' y <- 1
#' pryr::address(y)
#' y <- f(y)
#' pryr::address(y)
#' }
#' @export
as.icd_short_diag <- function(x, warn = TRUE) {
  if (warn)
    warning("setting icd_short_diag attribute: this probably makes R do an implicit copy. ",
            "For faster setting, if performance is required, just use: ",
            "attr(x, \"icd_short_diag\") <- TRUE")


  if (warn && identical(attr(x, "icd_short_diag"), FALSE))
    warning("setting icd_short_diag to TRUE, but currently false")
  icd_short_diag(x)
}

#' @rdname is.icd9
#' @export
as.icd_decimal_diag <- function(x, warn = TRUE) {
  if (warn)
    warning("setting icd_short_diag attribute: this probably makes R do an implicit copy. ",
            "For faster setting, if performance is required, just use: ",
            "attr(x, \"icd_short_diag\") <- TRUE")

  if (warn && identical(attr(x, "icd_short_diag"), TRUE))
    warning("setting icd_short_diag to TRUE, but currently false")
  icd_decimal_diag(x)
}
