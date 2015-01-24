# work around Rcpp bug https://github.com/RcppCore/Rcpp/issues/227 (fix
# commited), by temporarily renaming the function arguments manually to major
# and minor.

# EXCLUDE COVERAGE START

#' @rdname convert
#' @export
icd9MajMinToCode <- function(major, minor, isShort) {
  .Call('icd9_icd9MajMinToCode', PACKAGE = 'icd9', major, minor, isShort)
}

#' @rdname convert
#' @export
icd9MajMinToShort <- function(major, minor) {
  .Call('icd9_icd9MajMinToShort', PACKAGE = 'icd9', major, minor)
}

#' @rdname convert
#' @export
icd9MajMinToDecimal <- function(major, minor) {
  .Call('icd9_icd9MajMinToDecimal', PACKAGE = 'icd9', major, minor)
}

#' @rdname convert
#' @export
icd9MajMinToParts <- function(major, minor) {
  .Call('icd9_icd9MajMinToParts', PACKAGE = 'icd9', major, minor)
}

icd9MajMinToParts_list <- function(major, minor) {
  .Call('icd9_icd9MajMinToParts_list', PACKAGE = 'icd9', major, minor)
}

icd9AddLeadingZeroesMajorSingle <- function(major) {
  .Call('icd9_icd9AddLeadingZeroesMajorSingle', PACKAGE = 'icd9', major)
}

#' @rdname icd9AddLeadingZeroes
icd9AddLeadingZeroesMajor <- function(major) {
  .Call('icd9_icd9AddLeadingZeroesMajor', PACKAGE = 'icd9', major)
}

#' @title expand decimal part of ICD-9 code to cover all possible sub-codes
#' @description Accepts a single number or character input starting point for
#'   generation of all possible decimal parts of ICD9 code. e.g. giving an empty
#'   input will fill out 111 combinations, e..g .1 .11 .12 .... .2 ....
#' @template minor
#' @param isE single logical, which if TRUE, treats the minor as part of an E
#'   code (which is one character), as opposed to a V or numeric-only code,
#'   which is two character. Default is \code{FALSE}.
#' @examples
#'   # return all possible decimal parts of ICD9 codes (111 in total)
#'   length(icd9:::icd9ExpandMinor("", isE = FALSE))
#'   icd9:::icd9ExpandMinor("1") # "1"  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
#' @return NA for invalid minor, otherwise a vector of all possible (perhaps
#'   non-existent) sub-divisions.
#' @family ICD-9 ranges
#' @keywords internal manip
icd9ExpandMinor <- function(minor, isE = FALSE) {
  .Call('icd9_icd9ExpandMinor', PACKAGE = 'icd9', minor, isE)
}
# EXCLUDE COVERAGE END
