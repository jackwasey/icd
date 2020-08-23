#' convert to character vector without warning
#' @param x vector, typically numeric or a factor
#' @return character vector
#' @keywords internal
#' @noRd
as_char_no_warn <- function(x) {
  if (is.character(x)) {
    return(x)
  }
  if (is.integer(x)) {
    return(fastIntToStringRcpp(x))
  }
  if (is.factor(x)) {
    return(levels(x)[x])
  }
  suppressWarnings(as.character(x))
}
