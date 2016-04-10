# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

asCharacterNoWarn <- function(x) {
  icd_deprecated("as_char_no_warn")
  as_char_no_warn(x)
}

#' Deprecated: get children of ICD-9 codes
#'
#' Use \code{icd_children} instead
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @template onlyReal
#' @template onlyBillable
#' @export
icd9Children <- function(icd9, isShort = icd_guess_short(icd9), onlyReal = TRUE, onlyBillable = FALSE) {
  icd_deprecated("icd_children")
  icd_children.icd9(icd9, short_code = isShort, defined = onlyReal, billable = onlyBillable)
}

#' @rdname icd9Children
#' @export
icd9ChildrenShort <- function(icd9Short, onlyReal = TRUE, onlyBillable = FALSE) {
  icd_deprecated("icd_children")
  icd_children.icd9(icd9Short, short_code = TRUE, defined = onlyReal, billable = onlyBillable) %>% unclass
}

#' @rdname icd9Children
#' @export
icd9ChildrenDecimal <- function(icd9Decimal, onlyReal = TRUE, onlyBillable = FALSE) {
  icd_deprecated("icd_children")
  icd_children.icd9(icd9Decimal, short_code = FALSE, defined = onlyReal, billable = onlyBillable)
}

#' Deprecated: get comorbidities from patient data
#'
#' Use \code{icd9_comorbid} instead
#' @template icd9df
#' @template mapping-icd9
#' @template visitId
#' @template icd9field
#' @template isShort
#' @param isShortMapping logical
#' @template return.df
#' @param ... arguments passed to the corresponding function from the alias.
#'   E.g. all the arguments passed to \code{icd9ComorbiditiesAhrq} are passed on
#'   to \code{icd9ComorbidAhrq}
#' @export
icd9Comorbid <- function(icd9df,
                         icd9Mapping,
                         visitId = NULL,
                         icd9Field = NULL,
                         isShort = icd_guess_short(icd9df),
                         isShortMapping = icd_guess_short(icd9Mapping),
                         return.df = FALSE, ...) {
  icd_deprecated("icd9_comorbid")
  icd9_comorbid(x = icd9df, map = icd9Mapping, visit_name = visitId, icd_name = icd9Field,
                    short_code = isShort, short_map = isShortMapping, return_df = return.df, ...)
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbidShort <- function(...) {
  icd_deprecated("icd9_comorbid")
  icd9_comorbid(..., short_code = TRUE)
}
#' @rdname icd9Comorbid
#' @export
icd9Comorbidities <- function(...) {
  icd_deprecated("icd9_comorbid")
  icd9Comorbid(...)
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbiditiesAhrq <- function(...) {
  icd_deprecated("icd9_comorbid")
  icd9ComorbidAhrq(...)
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbiditiesElixHauser <- function(...) {
  icd_deprecated("icd9_comorbid")
  icd9ComorbidElix(...)
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbiditiesQuanDeyo <- function(...) {
  icd_deprecated("icd9_comorbid")
  icd9ComorbidQuanDeyo(...)
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbiditiesQuanElixhauser <- function(...) {
  icd_deprecated("icd9_comorbid")
  icd9ComorbidQuanElix(...)
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbidQuanElixhauser <- function(...) {
  icd_deprecated("icd9_comorbid_quan_elix")
  icd9ComorbidQuanElix(...)
}

#' @rdname icd9Comorbid
#' @param abbrevNames logical
#' @param applyHierarchy logical
#' @export
icd9ComorbidAhrq <- function(icd9df,
                             visitId = NULL,
                             icd9Field = NULL,
                             isShort = icd_guess_short(icd9df, n = 100, icd_name = icd9Field),
                             return.df = FALSE, abbrevNames = TRUE, applyHierarchy = TRUE, ...) {
  icd_deprecated("icd9_comorbid_ahrq")
  icd9_comorbid_ahrq(icd9df, visit_name = visitId,
                         icd_name = icd9Field, short_code = isShort,
                         abbrev_names = abbrevNames,
                         hierarchy = applyHierarchy,
                         return_df = return.df, ...)
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbidElix <- function(icd9df,
                             visitId = NULL,
                             icd9Field = NULL,
                             isShort = icd_guess_short(icd9df, n = 100, icd_name = icd9Field),
                             return.df = FALSE, abbrevNames = TRUE, applyHierarchy = TRUE, ...) {
  icd_deprecated("icd9_comorbid_elix")
  icd9_comorbid_elix(icd9df,visit_name = visitId,
                         icd_name = icd9Field, short_code = isShort,
                         abbrev_names = abbrevNames,
                         hierarchy = applyHierarchy, return_df = return.df,...)
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbidQuanElix <- function(icd9df,
                                 visitId = NULL,
                                 icd9Field = NULL,
                             isShort = icd_guess_short(icd9df, n = 100, icd_name = icd9Field),
                                 return.df = FALSE, abbrevNames = TRUE, applyHierarchy = TRUE, ...) {
  icd_deprecated("icd9_comorbid_quan_elix")
  icd9_comorbid_quan_elix(icd9(icd9df), visit_name = visitId,
                              icd_name = icd9Field, short_code = isShort,
                              abbrev_names = abbrevNames,
                              hierarchy = applyHierarchy, return_df = return.df, ...)
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbidQuanDeyo <- function(icd9df,
                                 visitId = NULL,
                                 icd9Field = NULL,
                             isShort = icd_guess_short(icd9df, n = 100, icd_name = icd9Field),
                                 return.df = FALSE, abbrevNames = TRUE, applyHierarchy = TRUE, ...) {
  icd_deprecated("icd9_comorbid_quan_deyo")
  icd9_comorbid_quan_deyo(icd9df, visit_name = visitId,
                              icd_name = icd9Field, short_code = isShort,
                              abbrev_names = abbrevNames,
                              hierarchy = applyHierarchy, return_df = return.df, ...)
}

#' Deprecated: condense list of ICD-9 codes
#'
#' Use \code{icd_condense} instead
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @template onlyReal
#' @template warn
#' @param keepFactorLevels logical
#' @export
icd9Condense <- function(icd9, isShort = icd_guess_short(icd9),
                         onlyReal = NULL, warn = TRUE) {
  icd_deprecated("icd_condense.icd9")
  icd_condense.icd9(x = icd9, short_code = isShort, defined = onlyReal, warn = warn)
}

#' @rdname icd9Condense
#' @export
icd9CondenseDecimal <- function(icd9Decimal, onlyReal = NULL, warn = TRUE, keepFactorLevels = FALSE) {
  icd_deprecated("icd_condense.icd9")
  icd_condense.icd9(x = icd9Decimal, short_code = FALSE, defined = onlyReal,
                    warn = warn, keep_factor_levels = keepFactorLevels)
}

#' @rdname icd9Condense
#' @export
icd9CondenseShort <- function(icd9Short, onlyReal = NULL, warn = TRUE, keepFactorLevels = FALSE) {
  icd_deprecated("icd_condense.icd9")
  icd_condense.icd9(x = icd9Short, short_code = TRUE, defined = onlyReal,
                    warn = warn, keep_factor_levels = keepFactorLevels)
}

#' Deprecated: convert matrix of comorbidities to data frame
#'
#' Use \code{icd9_comorbid_mat_to_df} or \code{icd9_comorbid_df_to_mat} instead.
#' @param x matrix or data frame of comorbidities
#' @template visitId
#' @template stringsAsFactors
#' @export
icd9ComorbidMatToDf <- function(x, visitId = "visit_name",
                                stringsAsFactors = getOption("stringsAsFactors")) {
  icd_deprecated("icd9_comorbid_mat_to_df")
  icd_comorbid_mat_to_df(x, visitId, stringsAsFactors)
}

#' @rdname icd9ComorbidMatToDf
#' @export
icd9ComorbidDfToMat <- function(x, visitId = get_visit_name(x),
                                stringsAsFactors = getOption("stringsAsFactors")) {
  icd_deprecated("icd9_comorbid_df_to_mat")
  icd_comorbid_df_to_mat(x, visitId, stringsAsFactors)
}

#' Deprecated: explain ICD codes in words
#'
#' Use \code{icd_explain} instead.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @param doCondense single logical value which indicates whether to condense
#'   the given set of ICD-9 codes by replacing subsets of codes with 'parent'
#'   codes which exactly encompass certain subsets. E.g. If all cholera
#'   diagnoses are provided, only '001 - Cholera' needs to be displayed, not all
#'   sub-types.
#' @param brief logical
#' @template warn
#' @export
icd9Explain <- function(icd9, isShort = icd_guess_short(icd9),
                        doCondense = TRUE, brief = FALSE, warn = TRUE) {
  icd_deprecated("icd_explain")
  UseMethod("icd9Explain")
}

#' @rdname icd9Explain
#' @export
#' @keywords internal
icd9Explain.character <- function(icd9, isShort = icd9GuessIsShort(icd9),
                                  doCondense = TRUE, brief = FALSE, warn = TRUE) {
  icd_deprecated("icd_explain")
  icd_explain.default(icd9, short_code = isShort,
                        condense = doCondense, brief = brief, warn = warn)
}

#' @rdname icd9Explain
#' @export
#' @keywords internal
icd9Explain.numeric <- function(icd9, isShort = icd_guess_short(icd9),
                                doCondense = TRUE, brief = FALSE, warn = FALSE) {
  icd_deprecated("icd_explain")
  icd_explain(as_char_no_warn(icd9), short_code = isShort, condense = doCondense, brief = brief, warn = warn)
}

#' @rdname icd9Explain
#' @export
#' @keywords internal
icd9Explain.list <- function(icd9, isShort = icd_guess_short(icd9),
                             doCondense = TRUE, brief = FALSE, warn = TRUE) {
  icd_deprecated("icd_explain")
  lapply(icd9, icd_explain.icd9cm, short_code = isShort, condense = doCondense, brief = brief, warn = warn)
}

#' @rdname icd9Explain
#' @export
icd9ExplainShort <- function(icd9Short, doCondense = TRUE, brief = FALSE, warn = TRUE) {
  icd_deprecated("icd_explain")
  icd_explain(icd9Short, short_code = TRUE, condense = doCondense, brief = brief, warn = warn)
}

#' @rdname icd9Explain
#' @export
icd9ExplainDecimal <- function(icd9Decimal, doCondense = TRUE, brief = FALSE, warn = TRUE) {
  icd_deprecated("icd_explain")
  icd_explain(icd9Decimal, short_code = FALSE, condense = doCondense, brief = brief, warn = warn)
}

icd9GuessIsShort <- function(icd9) {
  icd_deprecated("icd_guess_short")
  icd_guess_short(icd9)
}

#' Deprecated: filter valid ICD-9 codes in or out
#'
#' Use \code{icd_filter_valid} instead
#' @template icd9df
#' @template icd9field
#' @template isShort
#' @param invert logical
#' @export
icd9FilterValid <- function(icd9df, icd9Field = get_icd_name(icd9df),
                            isShort = icd_guess_short(icd9df[[icd9Field]]), invert = FALSE) {
  icd_deprecated("icd_filter_valid")
  icd9_filter_valid(x = icd9df, icd_name = icd9Field, short_code = isShort, invert = invert)
}

#' @rdname icd9FilterValid
#' @export
icd9FilterInvalid <- function(icd9df, icd9Field = get_icd_name(icd9df),
                            isShort = icd_guess_short(icd9df[[icd9Field]]), invert = FALSE) {
  icd_deprecated("icd_filter_valid")
  icd9_filter_valid(x = icd9df, icd_name = icd9Field, short_code = isShort, invert = !invert)
}

#' @rdname icd9FilterValid
#' @template poaField
#' @template poa
#' @export
icd9FilterPoa <- function(icd9df, poaField = "poa", poa = icd9PoaChoices) {
  icd_deprecated("icd_filter_poa")
  icd_filter_poa(x = icd9df, poa_name = poaField, poa = poa)
}

#' @rdname icd9FilterValid
#' @export
icd9FilterPoaYes <- function(icd9df, poaField = "poa") {
  icd_deprecated("icd_filter_poa_yes")
  icd_filter_poa_yes(x = icd9df, poa_name = poaField)
}

#' @rdname icd9FilterValid
#' @export
icd9FilterPoaNo <- function(icd9df, poaField = "poa") {
  icd_deprecated("icd_filter_poa_no")
  icd_filter_poa_no(x = icd9df, poa_name = poaField)
}

#' @rdname icd9FilterValid
#' @export
icd9FilterPoaNotNo <- function(icd9df, poaField = "poa") {
  icd_deprecated("icd_filter_poa_not_no")
  icd_filter_poa_not_no(x = icd9df, poa_name = poaField)
}

#' @rdname icd9FilterValid
#' @export
icd9FilterPoaNotYes <- function(icd9df, poaField = "poa") {
  icd_deprecated("icd_filter_poa_not_yes")
  icd_filter_poa_not_yes(x = icd9df, poa_name = poaField)
}


#' Deprecated: expand range of ICD-9 codes
#'
#' Use \code{icd_expand_range} instead.
#' @param start ICD-9 code
#' @param end ICD-9 code
#' @template isShort
#' @param onlyReal logical
#' @param excludeAmbiguousStart logical
#' @param excludeAmbiguousEnd logical
#' @export
icd9ExpandRange <- function(start, end,
                            isShort = icd_guess_short(c(start, end)),
                            onlyReal = TRUE,
                            excludeAmbiguousStart = TRUE,
                            excludeAmbiguousEnd = TRUE) {
  icd_deprecated("icd_expand_range")
  icd_expand_range.icd9(start = start, end = end, short_code = isShort,
                        defined = onlyReal,
                        ex_ambig_start = excludeAmbiguousStart,
                        ex_ambig_end = excludeAmbiguousEnd)
}

#' @rdname icd9ExpandRange
#' @export
icd9ExpandRangeShort <- function(start, end,
                                 onlyReal = TRUE,
                                 excludeAmbiguousStart = TRUE,
                                 excludeAmbiguousEnd = TRUE) {
  icd_deprecated("icd_expand_range")
  icd_expand_range.icd9(start = start, end = end, short_code = TRUE,
                        defined = onlyReal,
                        ex_ambig_start = excludeAmbiguousStart,
                        ex_ambig_end = excludeAmbiguousEnd)
}

#' @rdname icd9ExpandRange
#' @export
icd9ExpandRangeDecimal <- function(start, end,
                                   onlyReal = TRUE,
                                   excludeAmbiguousStart = TRUE,
                                   excludeAmbiguousEnd = TRUE) {
  icd_deprecated("icd_expand_range")
  icd_expand_range.icd9(start = start, end = end, short_code = FALSE,
                        defined = onlyReal,
                        ex_ambig_start = excludeAmbiguousStart,
                        ex_ambig_end = excludeAmbiguousEnd)
}

#' @rdname icd9ExpandRange
#' @export
icd9ExpandRangeMajor <- function(start, end, onlyReal = TRUE) {
  icd_deprecated("icd_expand_range_major")
  icd_expand_range_major.icd9(start = start, end = end, defined = onlyReal)
}
#' Deprecated: sort ICD-9 codes
#'
#' Use \code{icd_sort} instead.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @export
icd9Sort <- function(icd9, isShort = icd_guess_short(icd9)) {
  icd_deprecated("icd_sort")
  icd_sort.icd9(icd9, isShort)
}

#' @rdname icd9Sort
#' @export
icd9SortShort <- function(icd9Short) {
  icd_deprecated("icd_sort")
  icd_sort.icd9(icd9Short, short_code = TRUE)
}

#' @rdname icd9Sort
#' @export
icd9SortDecimal <- function(icd9Decimal) {
  icd_deprecated("icd_sort")
  icd_sort.icd9(icd9Decimal, short_code = FALSE)
}

#' Deprecated: check whether a mapping has only valid ICD-9 codes
#'
#' Use \code{icd_is_valid} instead.
#' @template mapping-icd9
#' @template isShort
#' @export
icd9IsValidMapping <- function(icd9Mapping, isShort) {
  icd_deprecated("icd_is_valid")
  icd_is_valid.icd_comorbidity_map(icd9Mapping, short_code = isShort)
}

#' @rdname icd9IsValidMapping
#' @export
icd9IsValidMappingShort <- function(icd9Mapping) {
  icd_deprecated("icd_is_valid")
  icd_is_valid.icd_comorbidity_map(icd9Mapping, short_code = TRUE)
}

#' @rdname icd9IsValidMapping
#' @export
icd9IsValidMappingDecimal <- function(icd9Mapping) {
  icd_deprecated("icd_is_valid")
  icd_is_valid.icd_comorbidity_map(icd9Mapping, short_code = FALSE)
}

#' @rdname icd9IsValidMapping
#' @export
icd9GetInvalidMappingShort <- function(icd9Mapping) {
  icd_deprecated("icd_is_valid")
  x <- lapply(icd9Mapping, FUN = icd9GetInvalidShort)
  x[lapply(x, length) > 0]
}

#' @rdname icd9IsValidMapping
#' @export
icd9GetInvalidMappingDecimal <- function(icd9Mapping) {
  icd_deprecated("icd_is_valid")
  x <- lapply(icd9Mapping, FUN = icd9GetInvalidDecimal)
  x[lapply(x, length) > 0]
}

#' Deprecated: check validity of ICD-9 codes
#'
#' Use \code{icd_is_valid} instead.
#' @template icd9-any
#' @template icd9-decimal
#' @template major
#' @template isShort
#' @export
icd9Valid <- function(icd9, isShort) {
  icd_deprecated("icd_is_valid")
  icd_is_valid.icd9(icd9, isShort)
}

#' @rdname icd9Valid
#' @export
icd9IsValid <- function(icd9, isShort) {
  icd_deprecated("icd_is_valid")
  icd_is_valid.icd9(icd9, isShort)
}

#' @rdname icd9Valid
#' @export
icd9IsValidMajorN <- function(major) {
  icd_deprecated("icd_is_valid_major")
  icd9_is_valid_major_n(major)
}

#' @rdname icd9Valid
#' @export
icd9IsValidMajorV <- function(major) {
  icd_deprecated("icd_is_valid_major")
  icd9_is_valid_major_v(major)
}

#' @rdname icd9Valid
#' @export
icd9IsValidMajorE <- function(major) {
  icd_deprecated("icd_is_valid_major")
  icd9_is_valid_major_e(major)
}

#' @rdname icd9Valid
#' @export
icd9ValidShort <- function(icd9) {
  icd_deprecated("icd_is_valid")
  icd9_is_valid_short(icd9)
}

#' @rdname icd9Valid
#' @export
icd9IsValidShort <- function(icd9) {
  icd_deprecated("icd_is_valid")
  icd9_is_valid_short(icd9)
}

#' @rdname icd9Valid
#' @export
icd9IsValidShortN <- function(icd9) {
  icd_deprecated("icd_is_valid")
  icd9_is_valid_short_n(icd9)
}

#' @rdname icd9Valid
#' @export
icd9IsValidShortV <- function(icd9) {
  icd_deprecated("icd_is_valid")
  icd9_is_valid_short_v(icd9)
}

#' @rdname icd9Valid
#' @export
icd9IsValidShortE <- function(icd9) {
  icd_deprecated("icd_is_valid")
  icd9_is_valid_short_e(icd9)
}

#' @rdname icd9Valid
#' @export
icd9ValidDecimal <- function(icd9Decimal) {
  icd_deprecated("icd_is_valid")
  icd9_is_valid_decimal(icd9Decimal)
}

#' @rdname icd9Valid
#' @export
icd9IsValidDecimal <- function(icd9Decimal) {
  icd_deprecated("icd_is_valid")
  icd9_is_valid_decimal(icd9Decimal)
}

#' @rdname icd9Valid
#' @export
icd9IsValidDecimalN <- function(icd9Decimal) {
  icd_deprecated("icd_is_valid")
  icd9_is_valid_decimal_n(icd9Decimal)
}

#' @rdname icd9Valid
#' @export
icd9IsValidDecimalV <- function(icd9Decimal) {
  icd_deprecated("icd_is_valid")
  icd9_is_valid_decimal_v(icd9Decimal)
}

#' @rdname icd9Valid
#' @export
icd9IsValidDecimalE <- function(icd9Decimal) {
  icd_deprecated("icd_is_valid")
  icd9_is_valid_decimal_e(icd9Decimal)
}

#' Deprecated: get valid ICD-9 codes
#'
#' Use \code{icd_get_valid} instead.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @export
icd9GetValid <- function(icd9, isShort = icd_guess_short(icd9)) {
  icd_deprecated("icd_get_valid")
  icd_get_valid.icd9(icd9, isShort)
}

#' @rdname icd9GetValid
#' @export
icd9GetValidDecimal <- function(icd9Decimal) {
  icd_deprecated("icd_get_valid")
  icd9Decimal[icd_is_valid.icd9(icd9Decimal, short_code = FALSE)]
}

#' @rdname icd9GetValid
#' @export
icd9GetValidShort <- function(icd9Short) {
  icd_deprecated("icd_get_valid")
  icd9Short[icd_is_valid.icd9(icd9Short, short_code = TRUE)]
}

#' @rdname icd9GetValid
#' @export
icd9GetInvalid <- function(icd9, isShort = icd_guess_short(icd9)) {
  icd_deprecated("icd_get_invalid")
  icd_get_invalid.icd9(icd9, short_code = isShort)
}

#' Deprecated: get or identify major part of an ICD-9 code
#'
#' Not a public function anymore.
#' If you really need it, use \code{icd:::icd_get_major} instead.
#' @param x vector of ICD-9 codes
#' @param icd vector of ICD-9 codes
#' @template isShort
#' @export
icd9GetMajor <- function(x, isShort) {
  icd_deprecated("icd_get_major")
  icd_get_major.icd9(x, short_code = isShort)
}

#' @rdname icd9GetMajor
#' @export
icd9IsMajor <- function(x) {
  icd_deprecated("icd_is_major.icd9")
  icd_is_major.icd9(x)
}

#' @rdname icd9GetMajor
#' @export
icd9IsValidMajor <- function(icd) {
  icd_deprecated("icd_is_valid_major.icd9")
  icd_is_valid_major.icd9(icd)
}

#' Deprecated: is a code a particular ICD-9 sub-type?
#'
#' Not exported anymore. If you really need it, use:
#' \code{icd9_is_n} etc.
#' @template icd9-any
#' @export
icd9IsN <- function(icd9) {
  icd_deprecated("icd9_is_n")
  icd9_is_n(icd9)
}

#' @rdname icd9IsN
#' @export
icd9IsV <- function(icd9) {
  icd_deprecated("icd9_is_v")
  icd9_is_v(icd9)
}

#' @rdname icd9IsN
#' @export
icd9IsE <- function(icd9) {
  icd_deprecated("icd9_is_e")
  icd9_is_e(icd9)
}

#' @rdname icd9GetValid
#' @export
icd9GetInvalidDecimal <- function(icd9Decimal) {
  icd_deprecated("icd_get_valid")
  icd9Decimal[!icd_is_valid.icd9(icd9Decimal, short_code = FALSE)]
}

#' @rdname icd9GetValid
#' @export
icd9GetInvalidShort <- function(icd9Short) {
  icd_deprecated("icd_get_valid")
  icd9Short[!icd_is_valid.icd9(icd9Short, short_code = TRUE)]
}

#' Deprecated: calculate ICD-9 Charlson score
#'
#' Use \code{icd_charlson} instead.
#' @param x vector of ICD-9 codes
#' @template visitId
#' @param scoringSystem Deprecated. Use \code{icd_charlson} with
#'   \code{scoring_system}. One of \code{original}, \code{charlson}, or
#'   \code{quan}. The first two will give the original Charlson weights for each
#'   comorbidity, whereas \code{quan} uses the updated weights from Quan 2001.
#' @param return.df Deprecated. Use \code{icd_charlson} with \code{return_df}.
#'   single logical value, if true, a two column data frame will be returned,
#'   with the first column named as in input data frame (i.e.
#'   \code{visit_name}), containing all the visits, and the second column
#'   containing the Charlson Comorbidity Index.
#' @template stringsAsFactors
#' @template dotdotdot
#' @export
icd9Charlson <- function(x, visitId = NULL,
                         scoringSystem = c("original", "charlson", "quan"),
                         return.df = FALSE,
                         stringsAsFactors = getOption("stringsAsFactors"),
                         ...) {
  icd_deprecated("icd_charlson")
  UseMethod("icd9Charlson")
}

#' @describeIn icd9Charlson \code{data.frame} method
#' @export
#' @keywords internal
icd9Charlson.data.frame <- function(x, visitId = NULL,
                                    scoringSystem = c("original", "charlson", "quan"),
                                    return.df = FALSE,
                                    stringsAsFactors = getOption("stringsAsFactors"),
                                    ...) {
  icd_deprecated("icd_charlson.data.frame")
  icd_charlson.data.frame(x, visit_name = visitId, scoring_system = scoringSystem,
                          return_df = return.df, stringsAsFactors = stringsAsFactors, ...)
}

#' @rdname icd9Charlson
#' @param applyHierarchy single logical value, default is FALSE. If TRUE, will drop
#'   \code{DM} if \code{DMcx} is present, etc.
#' @export
icd9CharlsonComorbid <- function(x, visitId = NULL, applyHierarchy = FALSE,
                                 scoringSystem = c("original", "charlson", "quan")) {
  icd_deprecated("icd_charlson_from_comorbid")
  icd_charlson_from_comorbid(x = x, visit_name = visitId, hierarchy = applyHierarchy, scoring_system = scoringSystem)
}

#' Deprecated: calculate Van Walraven score from ICD-9 codes
#'
#' Use \code{icd_van_walraven} instead.
#' @param x vector of ICD-9 codes
#' @template visitId
#' @template return.df
#' @template stringsAsFactors
#' @template dotdotdot
#' @export
icd9VanWalraven <- function(x, visitId = NULL,
                            return.df = FALSE,
                            stringsAsFactors = getOption("stringsAsFactors"),
                            ...) {
  icd_deprecated("icd_van_walraven")
  UseMethod("icd9VanWalraven")
}

icd9VanWalraven.data.frame <-
  function(x, visitId = NULL,
           return.df = FALSE,
           stringsAsFactors = getOption("stringsAsFactors"),
           ...) {
    icd_deprecated("icd_van_walraven.data.frame")
    icd_van_walraven.data.frame(x = x, visit_name = visitId,
                                return_df = return.df,
                                stringsAsFactors = stringsAsFactors, ...)
  }

#' @rdname icd9VanWalraven
#' @param applyHierarchy single logical value that defaults to \code{TRUE}, in
#'   which case the hierarchy defined for the mapping is applied. E.g. in
#'   Elixhauser, you can't have uncomplicated and complicated diabetes both
#'   flagged.
#' @export
icd9VanWalravenComorbid <- function(x, visitId = NULL, applyHierarchy = FALSE) {
  icd_deprecated("icd_van_walraven_from_comorbid")
  icd_van_walraven_from_comorbid(x = x, visit_name = visitId,
                                 hierarchy = applyHierarchy)
}

#' Deprecated: find difference between comorbidity maps
#'
#' Use \code{icd_diff_comorbid} instead.
#' @param x comorbidity map
#' @param y comorbidity map
#' @param names character vector of the comorbidity names
#' @param x.names character vector of the comorbidity names from \code{x} to
#'   compare
#' @param y.names character vector of the comorbidity names from \code{y} to
#' @param show logical
#' @param explain logical
#'   compare
#' @export
icd9DiffComorbid <- function(x, y, names = NULL, x.names = NULL, y.names = NULL,
                             show = TRUE, explain = TRUE) {
  icd_deprecated("icd_diff_comorbid")
  icd_diff_comorbid(x = x, y = y,
                    all_names = names, x_names = x.names, y_names = y.names,
                    show = show, explain = explain)
}

#' Deprecated: get chapters for ICD-9 codes
#'
#' Use \code{icd9_get_chapters} instead.
#' @template icd9-any
#' @template isShort
#' @template verbose
#' @export
icd9GetChapters <- function(icd9, isShort = icd_guess_short(icd9), verbose = FALSE) {
  out <- icd9_get_chapters(icd9, isShort, verbose)
  names(out) <- c("threedigit", "major", "subchapter", "chapter")
  out
}

#' Deprecated: convert ICD-9 patient data from long to wide format
#'
#' Use \code{icd_long_to_wide} or \code{icd_wide_to_long} instead.
#' @param icd9df \code{data.frame} of long-form data, one column for
#'   \code{visitId} and one for ICD code
#' @param x wide format data
#' @template visitId
#' @template icd9field
#' @param prefix character
#' @param min.width, single integer, if specified, writes out this many columns
#'   even if no patients have that many codes. Must be greater than or equal to
#'   the maximum number of codes per patient.
#' @param aggregate single logical value, if \code{TRUE} (the default) will take more
#'   time to find out-of-order \code{visitId}s, and combine all the codes for
#'   each unique \code{visitId}. If \code{FALSE}, then out-of-order
#'   \code{visitId}s will result in a row in the output data per contiguous
#'   block of identical \code{visitId}s.
#' @param return.df single logical value, if \code{TRUE}, return a data frame
#'   with a field for the \code{visitId}. This may be more convenient, but the
#'   default of \code{FALSE} gives the more natural return data of a matrix with
#'   row names being the \code{visitId}s.
#' @export
icd9LongToWide <- function(icd9df,
                           visitId =  get_visit_name(icd9df),
                           icd9Field = get_icd_name(icd9df),
                           prefix = "icd_",
                           min.width = 0,
                           aggregate = TRUE,
                           return.df = FALSE) {
  icd_deprecated("icd_long_to_wide")
  icd_long_to_wide(x = icd9df, visit_name = visitId, icd_name = icd9Field,
                   prefix = prefix, min_width = min.width,
                   aggr = aggregate, return_df = return.df)
}

#' @rdname icd9LongToWide
#' @param icdLabels Deprecated. Character vector of column names in which codes
#'   are found. If \code{NULL}, all columns matching \code{icdRegex} will be included.
#' @param icdName Deprecated. Character vector length one containing the new
#'   column name for the ICD codes, defaults to \code{"icdCode"}
#' @param icdRegex Deprecated. Vector of character strings containing a regular
#'   expression to identify ICD-9 diagnosis columns to try (case-insensitive) in
#'   order. Default is \code{c("icd", "diag", "dx_", "dx")}
#' @template verbose
#' @export
icd9WideToLong <- function(x,
                           visitId = get_visit_name(x),
                           icdLabels = NULL,
                           icdName = "icdCode",
                           icdRegex = c("icd", "diag", "dx_", "dx"),
                           verbose = FALSE) {
  icd_deprecated("icd_wide_to_long")
  icd_wide_to_long(x = x, visit_name = visitId, icd_labels = icdLabels,
                   icd_name = icdName, icd_regex = icdRegex)
}

#' Deprecated: convert between short, decimal ICD-9 codes
#'
#' Use \code{icd_decimal_to_short} etc. instead.
#' @template icd9-decimal
#' @param minorEmpty vector of length one, to be used in place of minor part of
#'   zero. Defaults to ""
#' @export
icd9DecimalToParts <- function(icd9Decimal, minorEmpty = "") {
  icd_deprecated("icd_decimal_to_parts")
  icd_decimal_to_parts(icd9Decimal, minorEmpty)
}

#' @rdname icd9DecimalToParts
#' @template icd9-short
#' @export
icd9DecimalToShort <- function(icd9Short) {
  icd_deprecated("icd_decimal_to_short")
  icd_decimal_to_short.icd9(icd9Short)
}

#' @rdname icd9DecimalToParts
#' @export
icd9ShortToDecimal <- function(icd9Short) {
  icd_deprecated("icd_short_to_decimal")
  icd_short_to_decimal.icd9(icd9Short)
}

#' @rdname icd9DecimalToParts
#' @export
icd9ShortToParts <- function(icd9Short) {
  icd_deprecated("icd_short_to_parts")
  icd_short_to_parts.icd9(icd9Short)
}

icd9InReferenceCode <- function(icd9, icd9Reference,
                                isShort, isShortReference = TRUE) {
  icd_deprecated("icd_in_reference_code")
  icd_in_reference_code(icd = icd9, icd_reference = icd9Reference,
                        short_code = isShort,
                        short_reference = isShortReference)
}

expandRangeWorker <- function(start, end, lookup, onlyReal,
                              excludeAmbiguousStart, excludeAmbiguousEnd){
  icd_deprecated("expand_range_worker")
  expand_range_worker(start, end, lookup, defined = onlyReal,
                      excludeAmbiguousStart, excludeAmbiguousEnd)
}

icd9ExpandMinor <- function(minor, isE = FALSE) {
  icd_deprecated("icd_expand_minor")
  icd_expand_minor.icd9(minor, is_e = isE)
}

#' Deprecated: count number of ICD codes per patient
#'
#' Use \code{icd_count_codes} instead.
#' @param x \code{data.frame}
#' @template visitId
#' @template return.df
#' @export
icd9Count <- function(x, visitId = get_visit_name(x), return.df = FALSE) {
  icd_deprecated("icd_count_codes")
  icd_count_codes(x = x, visit_name = visitId, return_df = return.df)
}

#' @rdname icd9Count
#' @param aggregate, single logical, default is FALSE. If TRUE, the length (or
#'   rows) of the output will no longer match the input, but duplicate
#'   \code{visitId}s will be counted together.
#' @export
icd9CountWide <- function(x,
                          visitId = get_visit_name(x),
                          return.df = FALSE,
                          aggregate = FALSE) {
  icd_deprecated("icd_count_wide")
  icd_count_codes_wide(x = x, visit_name = visitId,
                       return_df = return.df, aggr = aggregate)
}

#' @rdname icd9Count
#' @export
icd9CountComorbidBin <- function(x, visitId = get_visit_name(x),
                                 return.df = FALSE) {
  icd_deprecated("icd_count_comorbid")
  icd_count_comorbid(x, visit_name = visitId, return_df = return.df)
}

getLatestBillableVersion <- function() {
  icd_deprecated("icd9cm_latest_edition")
  icd9cm_latest_edition()
}

#' Deprecated: get or test for billable (leaf node) ICD-9 codes
#'
#' Use \code{icd_is_billable} or \code{icd_get_billable} instead.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @param version character
#' @param invert logical
#' @export
icd9IsBillable <- function(icd9, isShort = icd9GuessIsShort(icd9),
                           version = getLatestBillableVersion()) {
  icd_deprecated("icd_is_billable")
  icd_is_billable.icd9(x = icd9, short_code = isShort, icd9cm_edition = version)
}

#' @rdname icd9IsBillable
#' @export
icd9IsBillableShort <- function(icd9Short, version = icd9cm_latest_edition())
  icd_is_billable.icd9(icd9Short, short_code = TRUE, icd9cm_edition = version)

#' @rdname icd9IsBillable
#' @export
icd9IsBillableDecimal <- function(icd9Decimal,
                                  version = icd9cm_latest_edition())
  icd_is_billable.icd9(icd9Decimal, short_code = FALSE, icd9cm_edition = version)

#' @rdname icd9IsBillable
#' @template isShort
#' @export
icd9GetBillable <- function(icd9, isShort = icd9GuessIsShort(icd9),
                            invert = FALSE, version = icd9cm_latest_edition()) {
  assert_vector(icd9)
  assert_flag(isShort)
  assert_flag(invert)
  assert_string(version)
  if (isShort)
    return(icd9[icd9IsBillableShort(icd9, version = version) != invert])
  icd9[icd9IsBillableDecimal(icd9, version = version) != invert]
}

#' @rdname icd9IsBillable
#' @export
icd9GetBillableShort <- function(icd9Short, version = getLatestBillableVersion()) {
  icd_deprecated("icd_get_billable")
  icd_get_billable.icd9(icd9Short, short_code = TRUE, icd9cm_edition = version)
}


#' @rdname icd9IsBillable
#' @export
icd9GetBillableDecimal <- function(icd9Decimal, version = getLatestBillableVersion()) {
  icd_deprecated("icd_get_billable")
  icd_get_billable.icd9(icd9Decimal, short_code = FALSE, icd9cm_edition = version)
}

#' @rdname icd9IsBillable
#' @export
icd9GetNonBillableShort <- function(icd9Short, version = getLatestBillableVersion()) {
  icd_deprecated("icd_get_billable")
  icd_get_billable.icd9(icd9Short, short_code = TRUE,
                        invert = TRUE, icd9cm_edition = version)
}


#' @rdname icd9IsBillable
#' @export
icd9GetNonBillableDecimal <- function(icd9Decimal, version = getLatestBillableVersion()) {
  icd_deprecated("icd_get_billable")
  icd_get_billable.icd9(icd9Decimal, short_code = FALSE,
                        invert = TRUE, icd9cm_edition = version)
}

#' @rdname icd9IsBillable
#' @export
icd9GetNonBillable <- function(icd9, isShort = icd9GuessIsShort(icd9),
                               version =  getLatestBillableVersion()) {
  icd_deprecated("icd_get_billable")
  icd_get_billable.icd9(icd9, short_code = isShort,
                        invert = TRUE, icd9cm_edition = version)
}

icd9ExtractAlphaNumeric <- function(icd9) {
  icd_deprecated("icd9_extract_alpha_numeric")
  icd9_extract_alpha_numeric(icd9)
}

generateSysData <- function(sysdata.path = file.path("R", "sysdata.rda"),
                            save = TRUE, verbose = NULL) {
  icd_deprecated("generate_sys")
  if (!missing(verbose))
    warning("verbose is deprecated, always on now for internal package building functions")

  generate_sysdata(save_data = save)
}

icd9DropLeadingZeroes <- function(x, isShort) {
  icd_deprecated("icd9_drop_leading_zeroes")
  unclass(icd9_drop_leading_zeroes(x, short_code = isShort))
}

icd9DropLeadingZeroesShort <- function(x) {
  icd_deprecated("icd9_drop_leading_zeroes")
  icd9_drop_leading_zeroes(x, short_code = TRUE)
}

icd9DropLeadingZeroesDecimal <- function(x) {
  icd_deprecated("icd9_drop_leading_zeroes")
  icd9_drop_leading_zeroes(x, short_code = FALSE)
}

icd9DropLeadingZeroesMajor <- function(major)  {
  icd_deprecated("icd9_drop_leading_zeroes_major")
  icd9_drop_leading_zeroes_major(major)
}

icd9AddLeadingZeroesShort <- function(x) {
  icd_deprecated("icd9_add_leading_zeroes")
  icd9_add_leading_zeroes(x, short_code = TRUE)
}

icd9AddLeadingZeroesDecimal <- function(x) {
  icd_deprecated("icd9_add_leading_zeroes")
  icd9_add_leading_zeroes(x, short_code = FALSE)
}

icd9AddLeadingZeroesMajor <- function(x) {
  icd_deprecated("icd9_add_leading_zeroes_major")
  icd9_add_leading_zeroes_major(x)
}

#' Deprecated: get or identify 'real' defined ICD-9-CM codes.
#'
#' Use \code{icd_is_defined} instead.
#' @template icd9-any
#' @template isShort
#' @template onlyBillable
#' @export
icd9IsReal <- function(icd9, isShort = icd_guess_short(icd9),
                       onlyBillable = FALSE) {
  icd_deprecated("icd_is_defined")
  icd_is_defined.icd9(icd9, short_code = isShort, billable = onlyBillable)
}

#' @rdname icd9IsReal
#' @template icd9-short
#' @export
icd9IsRealShort <- function(icd9Short, onlyBillable = FALSE) {
  icd_deprecated("icd_is_defined")
  icd_is_defined.icd9(icd9Short, short_code = TRUE, billable = onlyBillable)
}

#' @rdname icd9IsReal
#' @template icd9-decimal
#' @export
icd9IsRealDecimal <- function(icd9Decimal, onlyBillable = FALSE) {
  icd_deprecated("icd_is_defined")
  icd_is_defined.icd9(icd9Decimal, short_code = FALSE, billable = onlyBillable)
}

#' @rdname icd9IsReal
#' @export
icd9GetReal <- function(icd9, isShort = icd_guess_short(icd9),
                        onlyBillable = FALSE) {
  icd_deprecated("icd_get_defined")
  icd_get_defined.icd9(icd9, short_code = isShort, billable = onlyBillable)
}

#' @rdname icd9IsReal
#' @export
icd9GetRealShort <- function(icd9Short, onlyBillable = FALSE) {
  icd_deprecated("icd_get_defined")
  icd_get_defined.icd9(icd9Short, short_code = TRUE, billable = onlyBillable)
}

#' @rdname icd9IsReal
#' @export
icd9GetRealDecimal <- function(icd9Decimal, onlyBillable = FALSE) {
  icd_deprecated("icd_get_defined")
  icd_get_defined.icd9(icd9Decimal, short_code = FALSE, billable = onlyBillable)
}


