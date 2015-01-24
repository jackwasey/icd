# EXCLUDE COVERAGE START

# icd9Children separate from C++ docs so that I can guess isShort

#' @name icd9Children
#' @title Expand 5 character form 'short' ICD9 to all possible sub codes
#' @description Much faster in C++, but gains diminished with increasing numbers
#'   of input shorts, probably because I am copying memory unnecessarily.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @template onlyReal
#' @keywords manip
#' @family ICD-9 ranges
#' @aliases icd9ChildrenShort icd9ChildrenDecimal icd9ChildrenShort
#' @examples
#' library(magrittr)
#' icd9ChildrenShort("10201", FALSE) # no children other than self
#' icd9Children("0032", FALSE) # guess it was a short, not decimal code
#' icd9ChildrenShort("10201", TRUE) # empty because 102.01 is not meaningful
#' icd9ChildrenShort("003", TRUE) %>% icd9ExplainShort(doCondense = FALSE)
#' icd9ChildrenDecimal("100.0")
#' icd9ChildrenDecimal("100.00")
#' icd9ChildrenDecimal("2.34")
#'
#' @export
icd9Children <- function(icd9, isShort = icd9GuessIsShort(icd9), onlyReal = TRUE) {
  checkmate::checkCharacter(icd9)
  checkmate::checkLogical(isShort, any.missing = FALSE, len = 1)
  checkmate::checkLogical(onlyReal, any.missing = FALSE, len = 1)
  .Call('icd9_icd9Children', PACKAGE = 'icd9', icd9, isShort, onlyReal)
}
# EXCLUDE COVERAGE END
