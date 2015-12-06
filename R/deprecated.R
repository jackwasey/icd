# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

#' @rdname icd_children
#' @export
icd9Children <- function(icd9, isShort = icd_guess_short(icd9), onlyReal = TRUE, onlyBillable = FALSE) {
  .Deprecated("icd_children")
  icd_children.icd9(icd9, short_code = isShort, real = onlyReal, billable = onlyBillable)
}

#' @rdname icd_children
#' @export
icd9ChildrenShort <- function(icd9Short, onlyReal = TRUE, onlyBillable = FALSE) {
  .Deprecated("icd_children")
  icd_children.icd9(icd9Short, short_code = TRUE, real = onlyReal, billable = onlyBillable)
}

#' @rdname icd_children
#' @export
icd9ChildrenDecimal <- function(icd9Decimal, onlyReal = TRUE, onlyBillable = FALSE) {
  .Deprecated("icd_children")
  icd_children.icd9(icd9Decimal, short_code = FALSE, real = onlyReal, billable = onlyBillable)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbidShort <- function(...) {
  .Deprecated("icd_comorbid")
  icd_comorbid.icd9(..., short = TRUE)
}
#' @rdname icd_comorbid
#' @param ... arguments passed to the corresponding function from the alias.
#'   E.g. all the arguments passed to \code{icd9ComorbiditiesAhrq} are passed on
#'   to \code{icd9ComorbidAhrq}
#' @export
icd9Comorbidities <- function(...) {
  .Deprecated("icd_comorbid")
  icd9Comorbid(...)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbiditiesAhrq <- function(...) {
  .Deprecated("icd_comorbid")
  icd9ComorbidAhrq(...)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbiditiesElixHauser <- function(...) {
  .Deprecated("icd_comorbid")
  icd9ComorbidElix(...)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbiditiesQuanDeyo <- function(...) {
  .Deprecated("icd_comorbid")
  icd9ComorbidQuanDeyo(...)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbiditiesQuanElixhauser <- function(...) {
  .Deprecated("icd_comorbid")
  icd9ComorbidQuanElix(...)
}

#' @rdname icd_comorbid
#' @param ... arguments passed to the corresponding function from the alias.
#'   E.g. all the arguments passed to \code{icd9ComorbiditiesAhrq} are passed on
#'   to \code{icd9ComorbidAhrq}
#' @export
icd9Comorbid <- function(...) {
  .Deprecated("icd_comorbid")
  icd9Comorbid(...)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbidAhrq <- function(...) {
  .Deprecated("icd_comorbid")
  icd9ComorbidAhrq(...)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbidElixHauser <- function(...) {
  .Deprecated("icd_comorbid")
  icd9ComorbidElix(...)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbidQuanDeyo <- function(...) {
  .Deprecated("icd_comorbid")
  icd9ComorbidQuanDeyo(...)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbidQuanElixhauser <- function(...) {
  .Deprecated("icd_comorbid")
  icd9ComorbidQuanElix(...)
}

#' @rdname icd_condense
#' @export
icd9Condense <- function(icd9, isShort = icd_guess_short(icd9),
                         onlyReal = NULL, warn = TRUE) {
  .Deprecated("icd_condense.icd9")
  icd_condense.icd9(x = icd9, short_code = isShort, real = onlyReal, warn = warn)
}

#' @rdname icd_condense
#' @export
icd9CondenseDecimal <- function(icd9Decimal, onlyReal = NULL, warn = TRUE) {
  .Deprecated("icd_condense.icd9")
  icd_condense.icd9(x = icd9, short_code = FALSE, real = onlyReal, warn = warn)
}

#' @rdname icd_condense
#' @export
icd9CondenseShort <- function(icd9, onlyReal = NULL, warn = TRUE) {
  .Deprecated("icd_condense.icd9")
  icd_condense.icd9(x = icd9, short_code = TRUE, real = onlyReal, warn = warn)
}


#' @rdname icd_comorbid_mat_to_df
#' @export
icd9ComorbidMatToDf <- function(icd9df, visitId = "visit_name",
                                stringsAsFactors = getOption("stringsAsFactors")) {
  .Deprecated("icd_comorbid_mat_to_df")
  icd_comorbid_mat_to_df(icd9df, visitId, stringsAsFactors)
}

#' @rdname icd_comorbid_df_to_mat
#' @export
icd9ComorbidDfToMat <- function(icd9df, visitId = get_visit_name(icd9df),
                                stringsAsFactors = getOption("stringsAsFactors")) {
  .Deprecated("icd_comorbid_df_to_mat")
  icd_comorbid_df_to_mat(icd9df, visitId, stringsAsFactors)
}

#' @rdname icd_explain
#' @export
icd9Explain <- function(icd9, short_code = icd_guess_short(icd9), condense = TRUE, brief = FALSE, warn = TRUE) {
  .Deprecated("icd_explain")
  icd_explain(icd9, short_code, condense, brief, warn)
}

#' @rdname icd_explain
#' @export
icd9Explain.numeric <- function(icd9, short_code = icd_guess_short(icd9),
                                condense = TRUE, brief = FALSE, warn = FALSE) {
  .Deprecated("icd_explain.numeric")
  icd_explain.numeric(icd9, short_code, condense, brief, warn)
}
#' @rdname icd_explain
#' @export
icd9ExplainShort <- function(icd9Short, condense = TRUE, brief = FALSE, warn = TRUE) {
  .Deprecated("icd_explain")
  icd_explain(icd9Short, short_code = TRUE, condense = condense, brief = brief, warn = warn)
}

#' @rdname icd_explain
#' @export
icd9ExplainDecimal <- function(icd9Decimal, condense = TRUE, brief = FALSE, warn = TRUE) {
  .Deprecated("icd_explain")
  icd_explain(icd9Decimal, short_code = FALSE, condense = condense, brief = brief, warn = warn)
}

#' @rdname icd_guess_short
icd9GuessIsShort <- function(icd9) {
  .Deprecated("icd_guess_short")
  icd_guess_short(icd9)
}

#' @rdname icd_filter
#' @export
icd9FilterValid <- function(icd9df, icd9Field = NULL,
                            isShort =  NULL, invert = FALSE) {
  .Deprecated("icd_filter_valid")
  icd_filter_valid.icd9(icd_df = icd9df, icd_name = icd9Field, short_code = isShort, invert = invert)
}

#' @rdname icd_filter
#' @export
icd9FilterInvalid <- function(icd9df, icd9Field = NULL, isShort = NULL, invert = FALSE) {
  .Deprecated("icd_filter_valid")
  icd_filter_valid.icd9(icd_df = icd9df, icd_name = icd9Field, short_code = isShort, invert = !invert)
}

#' @rdname icd_filter
#' @export
icd9FilterPoa <- function(icd9df, poaField = "poa", poa = icd9PoaChoices) {
  .Deprecated("icd_filter_poa")
  icd_filter_poa(icd_df = icd9df, poa_name = poaField, poa = poa)
}

#' @rdname icd_filter
#' @export
icd9FilterPoaYes <- function(icd9df, poaField = "poa") {
  .Deprecated("icd_filter_poa_yes")
  icd_filter_poa_yes(icd_df = icd9df, poa_name = poaField)
}

#' @rdname icd_filter
#' @export
icd9FilterPoaNo <- function(icd9df, poaField = "poa") {
  .Deprecated("icd_filter_poa_no")
  icd_filter_poa_no(icd_df = icd9df, poa_name = poaField)
}

#' @rdname icd_filter
#' @export
icd9FilterPoaNotNo <- function(icd9df, poaField = "poa") {
  .Deprecated("icd_filter_poa_not_no")
  icd_filter_poa_not_no(icd_df = icd9df, poa_name = poaField)
}

#' @rdname icd_filter
#' @export
icd9FilterPoaNotYes <- function(icd9df, poaField = "poa") {
  .Deprecated("icd_filter_poa_not_yes")
  icd_filter_poa_not_yes(icd_df = icd9df, poa_name = poaField)
}


#' @rdname icd_expand_range
#' @export
icd9ExpandRange <- function(start, end,
                            isShort = icd_guess_short.icd9(c(start, end)),
                            onlyReal = TRUE,
                            excludeAmbiguousStart = TRUE,
                            excludeAmbiguousEnd = TRUE) {
  .Deprecated("icd_expand_range")
  icd_expand_range.icd9(start = start, end = end, short_code = isShort,
                        real = onlyReal,
                        excludeAmbiguousStart = excludeAmbiguousStart,
                        excludeAmbiguousEnd = excludeAmbiguousEnd)
}

#' @rdname icd_expand_range
#' @export
icd9ExpandRangeShort <- function(start, end,
                                 onlyReal = TRUE,
                                 excludeAmbiguousStart = TRUE,
                                 excludeAmbiguousEnd = TRUE) {
  .Deprecated("icd_expand_range")
  icd_expand_range.icd9(start = start, end = end, short_code = TRUE,
                        real = onlyReal,
                        excludeAmbiguousStart = excludeAmbiguousStart,
                        excludeAmbiguousEnd = excludeAmbiguousEnd)
}

#' @rdname icd_expand_range
#' @export
icd9ExpandRangeDecimal <- function(start, end,
                                   onlyReal = TRUE,
                                   excludeAmbiguousStart = TRUE,
                                   excludeAmbiguousEnd = TRUE) {
  .Deprecated("icd_expand_range")
  icd_expand_range.icd9(start = start, end = end, short_code = FALSE,
                        real = onlyReal,
                        excludeAmbiguousStart = excludeAmbiguousStart,
                        excludeAmbiguousEnd = excludeAmbiguousEnd)
}

#' @rdname icd_expand_range
#' @export
icd9ExpandRangeMajor <- function(start, end, onlyReal = TRUE) {
  .Deprecated("icd_expand_range_major")
  icd_expand_range_major.icd9(start = start, end = end, real = onlyReal)
}
#' @rdname icd_sort
#' @export
icd9Sort <- function(icd9, isShort = icd_guess_short(icd9)) {
  .Deprecated("icd_sort")
  icd_sort.icd9(icd9, isShort)
}

#' @rdname icd_sort
#' @export
icd9SortShort <- function(icd9Short) {
  .Deprecated("icd_sort")
  icd_sort.icd9(icd9Short, short_code = TRUE)
}

#' @rdname icd_sort
#' @export
icd9SortDecimal <- function(icd9Decimal) {
  .Deprecated("icd_sort")
  icd_sort.icd9(icd9Decimal, short_code = FALSE)
}


#' @rdname icd_is_valid_map
#' @template mapping-icd9
#' @template short_code
#' @export
icd9ValidMapping <- function(icd9Mapping, isShort) {
  .Deprecated("icd_is_valid_map")
  icd_is_valid_map(icd9Mapping, isShort)
}

#' @rdname icd_is_valid_map
#' @template mapping-icd9
#' @export
icd9ValidMappingShort <- function(icd9Mapping) {
  .Deprecated("icd_is_valid_map")
  icd_is_valid_map(icd9Mapping, TRUE)
}

#' @rdname icd_is_valid_map
#' @template mapping-icd9
#' @export
icd9ValidMappingDecimal <- function(icd9Mapping) {
  .Deprecated("icd_is_valid_map")
  icd_is_valid_map(icd9Mapping, FALSE)
}

#' @rdname icd_is_valid_map
#' @template mapping-icd9
#' @export
icd9GetInvalidMappingShort <- function(icd9Mapping) {
  .Deprecated("icd_is_valid_map")
  x <- lapply(icd9Mapping, FUN = icd9GetInvalidShort)
  x[lapply(x, length) > 0]
}

#' @rdname icd_is_valid_map
#' @template mapping-icd9
#' @export
icd9GetInvalidMappingDecimal <- function(icd9Mapping) {
  .Deprecated("icd_is_valid_map")
  x <- lapply(icd9Mapping, FUN = icd9GetInvalidDecimal)
  x[lapply(x, length) > 0]
}

#' @rdname icd_is_valid
#' @template short_code
#' @export
icd9Valid <- function(icd9, isShort) {
  .Deprecated("icd_is_valid")
  icd_is_valid.icd9(icd9, isShort)
}

#' @rdname icd_is_valid
#' @export
icd9IsValid <- function(icd9, isShort) {
  .Deprecated("icd_is_valid")
  icd_is_valid.icd9(icd9, isShort)
}

#' @rdname icd_is_valid
#' @export
icd9ValidShort <- function(icd9) {
  .Deprecated("icd_is_valid")
  icd9_is_valid_short(icd9)
}

#' @rdname icd_is_valid
#' @export
icd9ValidDecimal <- function(icd9Decimal) {
  .Deprecated("icd_is_valid")
  icd_is_valid.icd9(icd9Decimal, short_code = FALSE)
}

#' @rdname icd_get_valid
icd9GetValid <- function(icd9, isShort = icd_guess_short(icd9)) {
  .Deprecated("icd_get_valid")
  icd_get_valid(icd9, isShort)
}

# Returns subset of codes which are in valid decimal format, e.g. "100" or "V01.10"
#' @rdname icd_get_valid
#' @export
icd9GetValidDecimal <- function(icd9Decimal) {
  .Deprecated("icd_get_valid")
  icd9Decimal[icd_is_valid(icd9Decimal, short_code = FALSE)]
}

# Returns subset of codes which are in valid short_code format, e.g. "E800" or "41001"
#' @rdname icd_get_valid
#' @export
icd9GetValidShort <- function(icd9Short) {
  .Deprecated("icd_get_valid")
  icd9Short[icd_is_valid(icd9Short, short_code = TRUE)]
}

# Returns subset of codes which are not in valid short_code or decimal format.
#' @rdname icd_get_valid
#' @export
icd9GetInvalid <- function(icd9, isShort = icd_guess_short(icd9)) {
  .Deprecated("icd_get_invalid")
  icd_get_invalid(icd9, short_code = isShort)
}

#' @rdname icd9_is_n
#' @export
icd9IsN <- function(icd9) {
  .Deprecated("icd9_is_n")
  icd9_is_n(icd9)
}

#' @rdname icd9_is_n
#' @export
icd9IsV <- function(icd9) {
  .Deprecated("icd9_is_v")
  icd9_is_v(icd9)
}

#' @rdname icd9_is_n
#' @export
icd9IsE <- function(icd9) {
  .Deprecated("icd9_is_e")
  icd9_is_e(icd9)
}

#' @describeIn icd_get_invalid Returns subset of codes which are not in valid decimal format.
#' @export
icd9GetInvalidDecimal <- function(icd9Decimal) {
  .Deprecated("icd_get_valid")
  icd9Decimal[!icd_is_valid.icd9(icd9Decimal, short_code = FALSE)]
}

#' @describeIn icd_get_invalid Returns subset of codes which are not in valid short_code format.
#' @export
icd9GetInvalidShort <- function(icd9Short) {
  .Deprecated("icd_get_valid")
  icd9Short[!icd_is_valid.icd9(icd9Short, short_code = TRUE)]
}

