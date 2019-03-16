#' convert to character vector without warning
#' @param x vector, typically numeric or a factor
#' @return character vector
#' @keywords internal
as_char_no_warn <- function(x) {
  if (is.character(x)) return(x)
  old <- options(warn = -1)
  on.exit(options(old))
  if (is.integer(x)) {
    return(fastIntToStringRcpp(x))
  }
  if (is.factor(x)) {
    return(levels(x)[x])
  }
  as.character(x)
}
