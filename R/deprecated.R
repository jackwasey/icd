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
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @template onlyReal
#' @template onlyBillable
#' @export
icd9Children <- function(icd9, isShort = icd_guess_short(icd9), onlyReal = TRUE, onlyBillable = FALSE) {
  .Deprecated("icd_children")
  icd_children.icd9(icd9, short_code = isShort, defined = onlyReal, billable = onlyBillable)
}

#' @rdname icd_children
#' @export
icd9ChildrenShort <- function(icd9Short, onlyReal = TRUE, onlyBillable = FALSE) {
  .Deprecated("icd_children")
  icd_children.icd9(icd9Short, short_code = TRUE, defined = onlyReal, billable = onlyBillable)
}

#' @rdname icd_children
#' @export
icd9ChildrenDecimal <- function(icd9Decimal, onlyReal = TRUE, onlyBillable = FALSE) {
  .Deprecated("icd_children")
  icd_children.icd9(icd9Decimal, short_code = FALSE, defined = onlyReal, billable = onlyBillable)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbidShort <- function(...) {
  .Deprecated("icd_comorbid")
  icd_comorbid.icd9(..., short_code = TRUE)
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
#' @export
icd9ComorbidQuanElixhauser <- function(...) {
  .Deprecated("icd_comorbid_quan_elix")
  icd9ComorbidQuanElix(...)
}

#' @rdname icd_comorbid
#' @param ... arguments passed to the corresponding function from the alias.
#'   E.g. all the arguments passed to \code{icd9ComorbiditiesAhrq} are passed on
#'   to \code{icd9ComorbidAhrq}
#' @export
icd9Comorbid <- function(icd9df,
                         icd9Mapping,
                         visitId = NULL,
                         icd9Field = NULL,
                         isShort = icd_guess_short.icd9(icd9df),
                         isShortMapping = icd_guess_short.icd9(icd9Mapping),
                         return.df = FALSE, ...) {
  .Deprecated("icd_comorbid")
  icd_comorbid.icd9(x = icd9df, map = icd9Mapping, visit_name = visitId, icd_name = icd9Field,
                    short_code = isShort, short_map = isShortMapping, return_df = return.df, ...)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbidAhrq <- function(icd9df,
                             visitId = NULL,
                             icd9Field = NULL,
                             isShort = icd9GuessIsShort(icd9df[1:100, icd9Field]),
                             return.df = FALSE, abbrevNames = TRUE, applyHierarchy = TRUE, ...) {
  .Deprecated("icd_comorbid_ahrq")
  icd_comorbid_ahrq.icd9(icd9df, visit_name = visitId,
                         icd_name = icd9Field, short_code = isShort,
                         abbrev_names = abbrevNames,
                         hierarchy = applyHierarchy,
                         return_df = return.df, ...)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbidElix <- function(icd9df,
                             visitId = NULL,
                             icd9Field = NULL,
                             isShort = icd9GuessIsShort(icd9df[1:100, icd9Field]),
                             return.df = FALSE, abbrevNames = TRUE, applyHierarchy = TRUE, ...) {
  .Deprecated("icd_comorbid_elix")
  icd_comorbid_elix.icd9(icd9df,visit_name = visitId,
                         icd_name = icd9Field, short_code = isShort,
                         abbrev_names = abbrevNames,
                         hierarchy = applyHierarchy, return_df = return.df,...)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbidQuanElix <- function(icd9df,
                                 visitId = NULL,
                                 icd9Field = NULL,
                                 isShort = icd9GuessIsShort(icd9df[1:100, icd9Field]),
                                 return.df = FALSE, abbrevNames = TRUE, applyHierarchy = TRUE, ...) {
  .Deprecated("icd_comorbid_quan_elix")
  icd_comorbid_quan_elix.icd9(icd9(icd9df), visit_name = visitId,
                              icd_name = icd9Field, short_code = isShort,
                              abbrev_names = abbrevNames,
                              hierarchy = applyHierarchy, return_df = return.df, ...)
}

#' @rdname icd_comorbid
#' @export
icd9ComorbidQuanDeyo <- function(icd9df,
                                 visitId = NULL,
                                 icd9Field = NULL,
                                 isShort = icd9GuessIsShort(icd9df[1:100, icd9Field]),
                                 return.df = FALSE, abbrevNames = TRUE, applyHierarchy = TRUE, ...) {
  .Deprecated("icd_comorbid_quan_deyo")
  icd_comorbid_quan_deyo.icd9(icd9df, visit_name = visitId,
                              icd_name = icd9Field, short_code = isShort,
                              abbrev_names = abbrevNames,
                              hierarchy = applyHierarchy, return_df = return.df, ...)
}

#' @rdname icd_condense
#' @export
icd9Condense <- function(icd9, isShort = icd_guess_short(icd9),
                         onlyReal = NULL, warn = TRUE) {
  .Deprecated("icd_condense.icd9")
  icd_condense.icd9(x = icd9, short_code = isShort, defined = onlyReal, warn = warn)
}

#' @rdname icd_condense
#' @export
icd9CondenseDecimal <- function(icd9Decimal, onlyReal = NULL, warn = TRUE, keepFactorLevels = FALSE) {
  .Deprecated("icd_condense.icd9")
  icd_condense.icd9(x = icd9Decimal, short_code = FALSE, defined = onlyReal,
                    warn = warn, keepFactorLevels = keepFactorLevels)
}

#' @rdname icd_condense
#' @export
icd9CondenseShort <- function(icd9Short, onlyReal = NULL, warn = TRUE, keepFactorLevels = FALSE) {
  .Deprecated("icd_condense.icd9")
  icd_condense.icd9(x = icd9Short, short_code = TRUE, defined = onlyReal,
                    warn = warn, keepFactorLevels = keepFactorLevels)
}


#' @rdname icd_comorbid_mat_to_df
#'
#' @template visitId
#' @export
icd9ComorbidMatToDf <- function(x, visitId = "visit_name",
                                stringsAsFactors = getOption("stringsAsFactors")) {
  .Deprecated("icd_comorbid_mat_to_df")
  icd_comorbid_mat_to_df(x, visitId, stringsAsFactors)
}

#' @rdname icd_comorbid_df_to_mat
#' @template visitId
#' @export
icd9ComorbidDfToMat <- function(x, visitId = get_visit_name(x),
                                stringsAsFactors = getOption("stringsAsFactors")) {
  .Deprecated("icd_comorbid_df_to_mat")
  icd_comorbid_df_to_mat(x, visitId, stringsAsFactors)
}

#' @rdname icd_explain
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @param doCondense single logical value which indicates whether to condense
#'   the given set of ICD-9 codes by replacing subsets of codes with 'parent'
#'   codes which exactly encompass certain subsets. E.g. If all cholera
#'   diagnoses are provided, only '001 - Cholera' needs to be displayed, not all
#'   subtypes.
#' @export
icd9Explain <- function(icd9, isShort = icd_guess_short.icd9(icd9), doCondense = TRUE, brief = FALSE, warn = TRUE) {
  .Deprecated("icd_explain")
  icd_explain(x = icd9, short_code = isShort, condense = doCondense, brief = brief, warn = warn)
}

#' @rdname icd_explain
#' @export
icd9Explain.numeric <- function(icd9, isShort = icd_guess_short(icd9),
                                doCondense = TRUE, brief = FALSE, warn = FALSE) {
  .Deprecated("icd_explain.numeric")
  icd_explain.numeric(icd9, short_code = isShort, condense = doCondense, brief = brief, warn = warn)
}

#' @rdname icd_explain
#' @export
icd9Explain.list <- function(icd9, isShort = icd_guess_short(icd9),
                             doCondense = TRUE, brief = FALSE, warn = FALSE) {
  .Deprecated("icd_explain.list")
  icd_explain.list(icd9, short_code = isShort, condense = doCondense, brief = brief, warn = warn)

}

#' @rdname icd_explain
#' @export
icd9ExplainShort <- function(icd9Short, doCondense = TRUE, brief = FALSE, warn = TRUE) {
  .Deprecated("icd_explain")
  icd_explain(icd9Short, short_code = TRUE, condense = doCondense, brief = brief, warn = warn)
}

#' @rdname icd_explain
#' @export
icd9ExplainDecimal <- function(icd9Decimal, doCondense = TRUE, brief = FALSE, warn = TRUE) {
  .Deprecated("icd_explain")
  icd_explain(icd9Decimal, short_code = FALSE, condense = doCondense, brief = brief, warn = warn)
}

#' @rdname icd_guess_short
icd9GuessIsShort <- function(icd9) {
  .Deprecated("icd_guess_short")
  icd_guess_short.icd9(icd9)
}

#' @rdname icd_filter_valid
#' @template icd9df
#' @template icd9field
#' @template isShort
#' @export
icd9FilterValid <- function(icd9df, icd9Field = NULL,
                            isShort =  NULL, invert = FALSE) {
  .Deprecated("icd_filter_valid")
  icd_filter_valid.icd9(x = icd9df, icd_name = icd9Field, short_code = isShort, invert = invert)
}

#' @rdname icd_filter_valid
#' @export
icd9FilterInvalid <- function(icd9df, icd9Field = NULL, isShort = NULL, invert = FALSE) {
  .Deprecated("icd_filter_valid")
  icd_filter_valid.icd9(x = icd9df, icd_name = icd9Field, short_code = isShort, invert = !invert)
}

#' @rdname icd_filter_poa
#' @template icd9df
#' @template poaField
#' @export
icd9FilterPoa <- function(icd9df, poaField = "poa", poa = icd9PoaChoices) {
  .Deprecated("icd_filter_poa")
  icd_filter_poa(x = icd9df, poa_name = poaField, poa = poa)
}

#' @rdname icd_filter_poa
#' @export
icd9FilterPoaYes <- function(icd9df, poaField = "poa") {
  .Deprecated("icd_filter_poa_yes")
  icd_filter_poa_yes(x = icd9df, poa_name = poaField)
}

#' @rdname icd_filter_poa
#' @export
icd9FilterPoaNo <- function(icd9df, poaField = "poa") {
  .Deprecated("icd_filter_poa_no")
  icd_filter_poa_no(x = icd9df, poa_name = poaField)
}

#' @rdname icd_filter_poa
#' @export
icd9FilterPoaNotNo <- function(icd9df, poaField = "poa") {
  .Deprecated("icd_filter_poa_not_no")
  icd_filter_poa_not_no(x = icd9df, poa_name = poaField)
}

#' @rdname icd_filter_poa
#' @export
icd9FilterPoaNotYes <- function(icd9df, poaField = "poa") {
  .Deprecated("icd_filter_poa_not_yes")
  icd_filter_poa_not_yes(x = icd9df, poa_name = poaField)
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
                        defined = onlyReal,
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
                        defined = onlyReal,
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
                        defined = onlyReal,
                        excludeAmbiguousStart = excludeAmbiguousStart,
                        excludeAmbiguousEnd = excludeAmbiguousEnd)
}

#' @rdname icd_expand_range
#' @export
icd9ExpandRangeMajor <- function(start, end, onlyReal = TRUE) {
  .Deprecated("icd_expand_range_major")
  icd_expand_range_major.icd9(start = start, end = end, defined = onlyReal)
}
#' @rdname icd_sort
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
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

#' @rdname icd_is_valid
#' @template mapping-icd9
#' @template isShort
#' @export
icd9IsValidMapping <- function(icd9Mapping, isShort) {
  .Deprecated("icd_is_valid")
  icd_is_valid.icd_comorbidity_map(icd9Mapping, short_code = isShort)
}

#' @rdname icd_is_valid
#' @export
icd9IsValidMappingShort <- function(icd9Mapping) {
  .Deprecated("icd_is_valid")
  icd_is_valid.icd_comorbidity_map(icd9Mapping, short_code = TRUE)
}

#' @rdname icd_is_valid
#' @export
icd9IsValidMappingDecimal <- function(icd9Mapping) {
  .Deprecated("icd_is_valid")
  icd_is_valid.icd_comorbidity_map(icd9Mapping, short_code = FALSE)
}

#' @rdname icd_is_valid
#' @export
icd9GetInvalidMappingShort <- function(icd9Mapping) {
  .Deprecated("icd_is_valid")
  x <- lapply(icd9Mapping, FUN = icd9GetInvalidShort)
  x[lapply(x, length) > 0]
}

#' @rdname icd_is_valid
#' @export
icd9GetInvalidMappingDecimal <- function(icd9Mapping) {
  .Deprecated("icd_is_valid")
  x <- lapply(icd9Mapping, FUN = icd9GetInvalidDecimal)
  x[lapply(x, length) > 0]
}

#' @rdname icd_is_valid
#' @template isShort
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

#' @rdname icd_is_valid_major
#' @export
icd9IsValidMajorN <- function(major) {
  .Deprecated("icd_is_valid_major")
  icd9_is_valid_major_n(major)
}


#' @rdname icd_is_valid_major
#' @export
icd9IsValidMajorV <- function(major) {
  .Deprecated("icd_is_valid_major")
  icd9_is_valid_major_v(major)
}


#' @rdname icd_is_valid_major
#' @export
icd9IsValidMajorE <- function(major) {
  .Deprecated("icd_is_valid_major")
  icd9_is_valid_major_e(major)
}

#' @rdname icd_is_valid
#' @export
icd9ValidShort <- function(icd9) {
  .Deprecated("icd_is_valid")
  icd9_is_valid_short(icd9)
}

#' @rdname icd_is_valid
#' @export
icd9IsValidShort <- function(icd9) {
  .Deprecated("icd_is_valid")
  icd9_is_valid_short(icd9)
}

#' @rdname icd_is_valid
#' @details do the icd9 valid subtypes need to be exported?
#' @export
icd9IsValidShortN <- function(icd9) {
  .Deprecated("icd_is_valid")
  icd9_is_valid_short_n(icd9)
}

#' @rdname icd_is_valid
#' @export
icd9IsValidShortV <- function(icd9) {
  .Deprecated("icd_is_valid")
  icd9_is_valid_short_v(icd9)
}

#' @rdname icd_is_valid
#' @export
icd9IsValidShortE <- function(icd9) {
  .Deprecated("icd_is_valid")
  icd9_is_valid_short_e(icd9)
}

#' @rdname icd_is_valid
#' @export
icd9ValidDecimal <- function(icd9Decimal) {
  .Deprecated("icd_is_valid")
  icd9_is_valid_decimal(icd9Decimal)
}

#' @rdname icd_is_valid
#' @export
icd9IsValidDecimal <- function(icd9Decimal) {
  .Deprecated("icd_is_valid")
  icd9_is_valid_decimal(icd9Decimal)
}

#' @rdname icd_is_valid
#' @export
icd9IsValidDecimalN <- function(icd9Decimal) {
  .Deprecated("icd_is_valid")
  icd9_is_valid_decimal_n(icd9Decimal)
}

#' @rdname icd_is_valid
#' @export
icd9IsValidDecimalV <- function(icd9Decimal) {
  .Deprecated("icd_is_valid")
  icd9_is_valid_decimal_v(icd9Decimal)
}

#' @rdname icd_is_valid
#' @export
icd9IsValidDecimalE <- function(icd9Decimal) {
  .Deprecated("icd_is_valid")
  icd9_is_valid_decimal_e(icd9Decimal)
}

#' @rdname icd_get_valid
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @export
icd9GetValid <- function(icd9, isShort = icd_guess_short(icd9)) {
  .Deprecated("icd_get_valid")
  icd_get_valid.icd9(icd9, isShort)
}

#' @rdname icd_get_valid
#' @export
icd9GetValidDecimal <- function(icd9Decimal) {
  .Deprecated("icd_get_valid")
  icd9Decimal[icd_is_valid.icd9(icd9Decimal, short_code = FALSE)]
}

#' @rdname icd_get_valid
#' @export
icd9GetValidShort <- function(icd9Short) {
  .Deprecated("icd_get_valid")
  icd9Short[icd_is_valid.icd9(icd9Short, short_code = TRUE)]
}

#' @rdname icd_get_valid
#' @export
icd9GetInvalid <- function(icd9, isShort = icd_guess_short(icd9)) {
  .Deprecated("icd_get_invalid")
  icd_get_invalid.icd9(icd9, short_code = isShort)
}

#' @rdname icd_get_major
#' @template isShort
#' @export
icd9GetMajor <- function(x, isShort) {
  .Deprecated("icd_get_major")
  icd_get_major.icd9(x, short_code = isShort)
}

#' @rdname icd_get_major
#' @export
icd9IsMajor <- function(x) {
  .Deprecated("icd_is_major.icd9")
  icd_is_major.icd9(x)
}

#' @rdname icd_is_valid_major
#' @export
icd9IsValidMajor <- function(icd) {
  .Deprecated("icd_is_valid_major.icd9")
  icd_is_valid_major.icd9(icd)
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

#' @rdname icd_get_invalid
#' @export
icd9GetInvalidDecimal <- function(icd9Decimal) {
  .Deprecated("icd_get_valid")
  icd9Decimal[!icd_is_valid.icd9(icd9Decimal, short_code = FALSE)]
}

#' @rdname icd_get_invalid
#' @export
icd9GetInvalidShort <- function(icd9Short) {
  .Deprecated("icd_get_valid")
  icd9Short[!icd_is_valid.icd9(icd9Short, short_code = TRUE)]
}

#' @rdname icd_charlson
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
#' @export
icd9Charlson <- function(x, visitId = NULL,
                         scoringSystem = c("original", "charlson", "quan"),
                         return.df = FALSE,
                         stringsAsFactors = getOption("stringsAsFactors"),
                         ...) {
  .Deprecated("icd_charlson")
  icd_charlson(x, visit_name = visitId, scoring_system = scoringSystem,
               return_df = return.df, stringsAsFactors = stringsAsFactors, ...)
}

#' @rdname icd_charlson
#' @export
icd9Charlson.data.frame <- function(x, visitId = NULL,
                                    scoringSystem = c("original", "charlson", "quan"),
                                    return.df = FALSE,
                                    stringsAsFactors = getOption("stringsAsFactors"),
                                    ...) {
  .Deprecated("icd_charlson.data.frame")
  icd_charlson.data.frame(x, visit_name = visitId, scoring_system = scoringSystem,
                          return_df = return.df, stringsAsFactors = stringsAsFactors, ...)
}

#' @rdname icd_charlson
#' @param hierarchy single logical value, default is FALSE. If TRUE, will
#'   drop DM if DMcx is present, etc.
#' @export
icd9CharlsonComorbid <- function(x, visitId = NULL, applyHierarchy = FALSE,
                                 scoringSystem = c("original", "charlson", "quan")) {
  .Deprecated("icd_charlson_from_comorbid")
  icd_charlson_from_comorbid(x = x, visit_name = visitId, hierarchy = applyHierarchy, scoring_system = scoringSystem)
}

#' @rdname icd_van_walraven
#' @export
icd9VanWalraven <- function(x, visitId = NULL,
                            return.df = FALSE,
                            stringsAsFactors = getOption("stringsAsFactors"),
                            ...) {
  .Deprecated("icd_van_walraven")
  icd_van_walraven(x, visitId, return.df, stringsAsFactors, ...)
}

#' @rdname icd_van_walraven
#' @template visitId
#' @template return.df
#' @export
icd9VanWalraven.data.frame <-
  function(x, visitId = NULL,
           return.df = FALSE,
           stringsAsFactors = getOption("stringsAsFactors"),
           ...) {
    # TODO: what about icd9 vs icd10 classes?
    .Deprecated("icd_van_walraven.data.frame")
    icd_van_walraven.data.frame(x = x, visit_name = visitId,
                                return_df = return.df,
                                stringsAsFactors = stringsAsFactors, ...)
  }

#' @rdname icd_van_walraven
#' @param applyHierarchy single logical value that defaults to \code{TRUE}, in
#'   which case the hierarchy defined for the mapping is applied. E.g. in
#'   Elixhauser, you can't have uncomplicated and complicated diabetes both
#'   flagged.
#' @export
icd9VanWalravenComorbid <- function(x, visitId = NULL, applyHierarchy = FALSE) {
  .Deprecated("icd_van_walraven_from_comorbid")
  icd_van_walraven_from_comorbid(x = x, visit_name = visitId,
                                 hierarchy = applyHierarchy)
}

#' @rdname icd_diff_comorbid
#' @param names character vector of the comorbidity names
#' @param x.names character vector of the comorbidity names from \code{x} to
#'   compare
#' @param y.names character vector of the comorbidity names from \code{y} to
#'   compare
#' @export
icd9DiffComorbid <- function(x, y, names = NULL, x.names = NULL, y.names = NULL,
                             show = TRUE, explain = TRUE) {
  .Deprecated("icd_diff_comorbid")
  icd_diff_comorbid(x = x, y = y,
                    all_names = names, x_names = x.names, y_names = y.names,
                    show = show, explain = explain)
}

#' @rdname icd9_get_chapters
#' @export
icd9GetChapters <- function(icd9, isShort = icd_guess_short(icd9), verbose = FALSE) {
  icd9_get_chapters(icd9, isShort, verbose)
}

#' @rdname icd_long_to_wide
#' @param icd9df data.frame of long-form data, one column for visitId and one
#'   for ICD code
#' @template visitid
#' @template icd9field
#' @param min.width, single integer, if specified, writes out this many columns
#'   even if no patients have that many codes. Must be greater than or equal to
#'   the maximum number of codes per patient.
#' @param aggregate single logical value, if TRUE (the default) will take more
#'   time to find out-of-order visitIds, and combine all the codes for each
#'   unique visitId. If \code{FALSE}, then out-of-order visitIds will result in
#'   a row in the output data per contiguous block of identical visitIds.
#' @param return.df single logical value, if \code{TRUE}, return a data frame
#'   with a field for the visitId. This may be more convenient, but the default
#'   of \code{FALSE} gives the more natural return data of a matrix with
#'   rownames being the visitIds.
#' @export
icd9LongToWide <- function(icd9df,
                           visitId =  get_visit_name(icd9df),
                           icd9Field = get_icd_name(icd9df),
                           prefix = "icd_",
                           min.width = 0,
                           aggregate = TRUE,
                           return.df = FALSE) {
  .Deprecated("icd_long_to_wide")
  icd_long_to_wide(x = icd9df, visit_name = visitId, icd_name = icd9Field,
                   prefix = prefix, min_width = min.width,
                   aggr = aggregate, return_df = return.df)
}

#' @rdname icd_wide_to_long
#' @template visitId
#' @param icdLabels Deprecated. Character vector of column names in which codes
#'   are found. If NULL, all columns matching icd or ICD will be included.
#' @param icdName Deprecated. Character vector length one containing the new
#'   column name for the ICD codes, defaults to "icdCode"
#' @param icdRegex Deprecated. Vector of character strings containg a regex to
#'   identify ICD-9 diagnosis columns to try (case-insensitive) in order.
#'   Default is \code{c("icd", "diag", "dx_", "dx")}
#' @template verbose
#' @export
icd9WideToLong <- function(x,
                           visitId = get_visit_name(x),
                           icdLabels = NULL,
                           icdName = "icdCode",
                           icdRegex = c("icd", "diag", "dx_", "dx"),
                           verbose = FALSE) {
  .Deprecated("icd_wide_to_long")
  icd_wide_to_long(x = x, visit_name = visitId, icd_labels = icdLabels,
                   icd_name = icdName, icd_regex = icdRegex)
}

#' @rdname icd_short_to_decimal
#' @template icd9-decimal
#' @param minorEmpty vector of length one, to be used in place of minor part of
#'   zero. Defaults to ""
#' @export
icd9DecimalToParts <- function(icd9Decimal, minorEmpty = "") {
  .Deprecated("icd_decimal_to_parts")
  icd_decimal_to_parts(icd9Decimal, minorEmpty)
}

#' @rdname icd_short_to_decimal
#' @template icd9-short
#' @export
icd9DecimalToShort <- function(icd9Short) {
  .Deprecated("icd_decimal_to_short")
  icd_decimal_to_short.icd9(icd9Short)
}

#' @rdname icd_short_to_decimal
#' @template icd9-short
#' @export
icd9ShortToDecimal <- function(icd9Short) {
  .Deprecated("icd_short_to_decimal")
  icd_short_to_decimal.icd9(icd9Short)
}

#' @rdname icd_short_to_decimal
#' @export
icd9ShortToParts <- function(icd9Short) {
  .Deprecated("icd_short_to_parts")
  icd_short_to_parts.icd9(icd9Short)
}

#' @rdname icd_in_reference_code
#' @keywords internal
icd9InReferenceCode <- function(icd9, icd9Reference,
                                isShort, isShortReference = TRUE) {
  .Deprecated("icd_in_reference_code")
  icd_in_reference_code(icd = icd9, icd_reference = icd9Reference,
                        short_code = isShort,
                        short_reference = isShortReference)
}

expandRangeWorker <- function(start, end, lookup, onlyReal,
                              excludeAmbiguousStart, excludeAmbiguousEnd){
  .Deprecated("expand_range_worker")
  expand_range_worker(start, end, lookup, defined = onlyReal,
                      excludeAmbiguousStart, excludeAmbiguousEnd)
}

icd9ExpandMinor <- function(minor, isE = FALSE) {
  .Deprecated("icd_expand_minor")
  icd_expand_minor.icd9(minor, is_e = isE)
}

parseQuanElix <- function(...) {
  .Deprecated("parse_quan_elix")
  icd9_generate_map_quan_elix(...)
}

parseElix <- function(...) {
  .Deprecated("parse_quan_elix")
  icd9_generate_map_elix(...)
}

logicalToBinary <- function(...) {
  .Deprecated("logical_to_binary")
  logical_to_binary(...)
}

#' @rdname icd_count_codes
#' @template visitId
#' @template return.df
#' @export
icd9Count <- function(x, visitId = get_visit_name(x), return.df = FALSE) {
  .Deprecated("icd_count_codes")
  icd_count_codes(x = x, visit_name = visitId, return_df = return.df)
}

#' @rdname icd_count_codes_wide
#' @template visitId
#' @template return.df
#' @param aggregate, single logical, default is FALSE. If TRUE, the length (or
#'   rows) of the output will no longer match the input, but duplicate visitIds
#'   will be counted together.
#' @export
icd9CountWide <- function(x,
                          visitId = get_visit_name(x),
                          return.df = FALSE,
                          aggregate = FALSE) {
  .Deprecated("icd_count_wide")
  icd_count_codes_wide(x = x, visit_name = visitId,
                       return_df = return.df, aggr = aggregate)
}

#' @rdname icd_count_comorbid
#' @template visitId
#' @template return.df
#' @export
icd9CountComorbidBin <- function(x, visitId = get_visit_name(x),
                                 return.df = FALSE) {
  .Deprecated("icd_count_comorbid")
  icd_count_comorbid(x, visit_name = visitId, return_df = return.df)
}

#' @rdname icd9cm_latest_edition
#' @keywords internal
getLatestBillableVersion <- function() {
  .Deprecated("icd9cm_latest_edition")
  icd9cm_latest_edition()
}

#' @rdname icd_is_billable
#' @export
icd9IsBillable <- function(icd9, isShort = icd9GuessIsShort(icd9),
                           version = getLatestBillableVersion()) {
  .Deprecated("icd_is_billable")
  icd_is_billable.icd9(x = icd9, short_code = isShort, version = version)
}

#' @rdname icd_is_billable
#' @export
icd9IsBillableShort <- function(icd9Short, version = icd9cm_latest_edition())
  icd_is_billable.icd9(icd9Short, short_code = TRUE, version)

#' @rdname icd_is_billable
#' @export
icd9IsBillableDecimal <- function(icd9Decimal,
                                  version = icd9cm_latest_edition())
  icd_is_billable.icd9(icd9Decimal, short_code = FALSE, version)

#' @rdname icd_get_billable
#' @export
icd9GetBillable <- function(icd9, isShort = icd9GuessIsShort(icd9),
                            invert = FALSE, version = icd9cm_latest_edition()) {
  assertVector(icd9)
  assertFlag(isShort)
  assertFlag(invert)
  assertString(version)
  if (isShort)
    return(icd9[icd9IsBillableShort(icd9, version = version) != invert])
  icd9[icd9IsBillableDecimal(icd9, version = version) != invert]
}

#' @rdname icd_get_billable
#' @export
icd9GetBillableShort <- function(icd9Short, version = getLatestBillableVersion()) {
  .Deprecated("icd_get_billable")
  icd_get_billable.icd9(icd9Short, short_code = TRUE, version = version)
}


#' @rdname icd_get_billable
#' @export
icd9GetBillableDecimal <- function(icd9Decimal, version = getLatestBillableVersion()) {
  .Deprecated("icd_get_billable")
  icd_get_billable.icd9(icd9Decimal, short_code = FALSE, version = version)
}

#' @rdname icd_get_billable
#' @export
icd9GetNonBillableShort <- function(icd9Short, version = getLatestBillableVersion()) {
  .Deprecated("icd_get_billable")
  icd_get_billable.icd9(icd9Short, short_code = TRUE,
                        invert = TRUE, version = version)
}


#' @rdname icd_get_billable
#' @export
icd9GetNonBillableDecimal <- function(icd9Decimal, version = getLatestBillableVersion()) {
  .Deprecated("icd_get_billable")
  icd_get_billable.icd9(icd9Decimal, short_code = FALSE,
                        invert = TRUE, version = version)
}

#' @rdname icd_get_billable
#' @export
icd9GetNonBillable <- function(icd9, isShort = icd9GuessIsShort(icd9),
                               version = getLatestBillableVersion()) {
  .Deprecated("icd_get_billable")
  icd_get_billable.icd9(icd9, short_code = isShort,
                        invert = TRUE, version = version)
}

#' @rdname icd_extract_alpha_numeric
#' @keywords internal
icd9ExtractAlphaNumeric <- function(icd9) {
  .Deprecated("icd_extract_alpha_numeric")
  icd_extract_alpha_numeric(icd9)
}

parseQuanDeyoSas <- function(...) {
  .Deprecated("parse_quan_deyo_sas")
  parse_quan_deyo_sas(...)
}

parseAhrqSas <- function(...) {
  .Deprecated("parse_ahrq_sas")
  parse_ahrq_sas(...)
}

generateSysData <- function(...) {
  .Deprecated("generate_sys")
  generate_sysdata(...)
}

icd9DropLeadingZeroes <- function(x, isShort) {
  .Deprecated("icd9_drop_leading_zeroes")
  unclass(icd9_drop_leading_zeroes.default(x, short_code = isShort))
}

icd9DropLeadingZeroesShort <- function(x) {
  .Deprecated("icd9_drop_leading_zeroes")
  icd9_drop_leading_zeroes.icd_short_code(x)
}

icd9DropLeadingZeroesDecimal <- function(x) {
  .Deprecated("icd9_drop_leading_zeroes")
  icd9_drop_leading_zeroes.icd_decimal_code(x)
}

icd9DropLeadingZeroesMajor <- function(major)  {
  .Deprecated("icd9_drop_leading_zeroes_major")
  icd9_drop_leading_zeroes_major(major)
}

icd9AddLeadingZeroesShort <- function(x) {
  .Deprecated("icd9_add_leading_zeroes")
  icd9_add_leading_zeroes.icd_short_code(x)
}

icd9AddLeadingZeroesDecimal <- function(x) {
  .Deprecated("icd9_add_leading_zeroes")
  icd9_add_leading_zeroes.icd_decimal_code(x)
}

stripRtf <- function(x) {
  .Deprecated("strip_rtf")
  strip_rtf(x)
}

#' @rdname icd_is_defined
#' @template icd9-any
#' @template isShort
#' @template onlyBillable
#' @export
icd9IsReal <- function(icd9, isShort = icd_guess_short(icd9),
                       onlyBillable = FALSE) {
  .Deprecated("icd_is_defined")
  icd_is_defined.icd9(icd9, short_code = isShort, billable = onlyBillable)
}

#' @rdname icd_is_defined
#' @template icd9-short
#' @export
icd9IsRealShort <- function(icd9Short, onlyBillable = FALSE) {
  .Deprecated("icd_is_defined")
  icd_is_defined.icd9(icd9Short, short_code = TRUE, billable = onlyBillable)
}

#' @rdname icd_is_defined
#' @template icd9-decimal
#' @export
icd9IsRealDecimal <- function(icd9Decimal, onlyBillable = FALSE) {
  .Deprecated("icd_is_defined")
  icd_is_defined.icd9(icd9Decimal, short_code = FALSE, billable = onlyBillable)
}

#' @rdname icd_is_defined
#' @export
icd9GetReal <- function(icd9, isShort = icd_guess_short.icd9(icd9),
                        onlyBillable = FALSE) {
  .Deprecated("icd_get_defined")
  icd_get_defined.icd9(icd9, short_code = isShort, billable = onlyBillable)
}

#' @rdname icd_is_defined
#' @export
icd9GetRealShort <- function(icd9Short, onlyBillable = FALSE) {
  .Deprecated("icd_get_defined")
  icd_get_defined.icd9(icd9Short, short_code = TRUE, billable = onlyBillable)
}

#' @rdname icd_is_defined
#' @export
icd9GetRealDecimal <- function(icd9Decimal, onlyBillable = FALSE) {
  .Deprecated("icd_get_defined")
  icd_get_defined.icd9(icd9Decimal, short_code = FALSE, billable = onlyBillable)
}

