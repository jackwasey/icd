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
  icd_comorbid_quan_elix.icd9(icd9df, visit_name = visitId,
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
  icd_condense.icd9(x = icd9, short_code = isShort, real = onlyReal, warn = warn)
}

#' @rdname icd_condense
#' @export
icd9CondenseDecimal <- function(icd9Decimal, onlyReal = NULL, warn = TRUE, keepFactorLevels = FALSE) {
  .Deprecated("icd_condense.icd9")
  icd_condense.icd9(x = icd9Decimal, short_code = FALSE, real = onlyReal, warn = warn, keepFactorLevels = keepFactorLevels)
}

#' @rdname icd_condense
#' @export
icd9CondenseShort <- function(icd9Short, onlyReal = NULL, warn = TRUE, keepFactorLevels = FALSE) {
  .Deprecated("icd_condense.icd9")
  icd_condense.icd9(x = icd9Short, short_code = TRUE, real = onlyReal, warn = warn, keepFactorLevels = keepFactorLevels)
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
icd9Explain <- function(icd9, isShort = icd_guess_short(icd9), doCondense = TRUE, brief = FALSE, warn = TRUE) {
  .Deprecated("icd_explain")
  icd_explain(icd9, short_code = isShort, condense = doCondense, brief, warn)
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

#' @rdname icd_filter
#' @export
icd9FilterValid <- function(icd9df, icd9Field = NULL,
                            isShort =  NULL, invert = FALSE) {
  .Deprecated("icd_filter_valid")
  icd_filter_valid.icd9(x = icd9df, icd_name = icd9Field, short_code = isShort, invert = invert)
}

#' @rdname icd_filter
#' @export
icd9FilterInvalid <- function(icd9df, icd9Field = NULL, isShort = NULL, invert = FALSE) {
  .Deprecated("icd_filter_valid")
  icd_filter_valid.icd9(x = icd9df, icd_name = icd9Field, short_code = isShort, invert = !invert)
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


#' @rdname icd_is_valid
#' @template mapping-icd9
#' @template short_code
#' @export
icd9IsValidMapping <- function(icd9Mapping, isShort) {
  .Deprecated("icd_is_valid.map")
  icd_is_valid.map(icd9Mapping, short_code = isShort)
}

#' @rdname icd_is_valid
#' @template mapping-icd9
#' @export
icd9IsValidMappingShort <- function(icd9Mapping) {
  .Deprecated("icd_is_valid.map")
  icd_is_valid.map(icd9Mapping, short_code = TRUE)
}

#' @rdname icd_is_valid
#' @template mapping-icd9
#' @export
icd9IsValidMappingDecimal <- function(icd9Mapping) {
  .Deprecated("icd_is_valid.map")
  icd_is_valid.map(icd9Mapping, short_code = FALSE)
}

#' @rdname icd_is_valid
#' @template mapping-icd9
#' @export
icd9GetInvalidMappingShort <- function(icd9Mapping) {
  .Deprecated("icd_is_valid.map")
  x <- lapply(icd9Mapping, FUN = icd9GetInvalidShort)
  x[lapply(x, length) > 0]
}

#' @rdname icd_is_valid
#' @template mapping-icd9
#' @export
icd9GetInvalidMappingDecimal <- function(icd9Mapping) {
  .Deprecated("icd_is_valid.map")
  x <- lapply(icd9Mapping, FUN = icd9GetInvalidDecimal)
  x[lapply(x, length) > 0]
}

#' @rdname icd_is_valid
#' @template short_code
#' @export
icd9IsValid <- function(icd9, isShort) {
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
icd9IsValidShort <- function(icd9) {
  .Deprecated("icd_is_valid")
  icd9_is_valid_short(icd9)
}

#' @rdname icd_is_valid
#' @export
icd9IsValidDecimal <- function(icd9Decimal) {
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

#Rcpp::CharacterVector icd9GetMajor(const Rcpp::CharacterVector x, const bool short_code)

#' @rdname icd_get_major
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
#' @export
icd9VanWalraven.data.frame <- function(x, visitId = NULL,
                                       return.df = FALSE,
                                       stringsAsFactors = getOption("stringsAsFactors"),
                                       ...) {
  .Deprecated("icd_van_walraven.data.frame") # todo: what about icd9 vs icd10 classes?
  icd_van_walraven.data.frame(x = x, visit_name = visitId, return_df = return.df, stringsAsFactors = stringsAsFactors, ...)
}

#' @rdname icd_van_walraven
#' @export
icd9VanWalravenComorbid <- function(x, visitId = NULL, applyHierarchy = FALSE) {
  .Deprecated("icd_van_walraven_from_comorbid")
  icd_van_walraven_from_comorbid(x = x, visit_name = visitId, hierarchy = applyHierarchy)
}

#' @rdname icd_diff_comorbid
#' @export
icd9DiffComorbid <- function(x, y, names = NULL, x.names = NULL, y.names = NULL,
                             show = TRUE, explain = TRUE) {
  .Deprecated("icd_diff_comorbid")
  icd_diff_comorbid(x = x, y =y, all_names = names, x_names = x.names, y_names = y.names, show = show, explain = explain)
}

#' @rdname icd9_get_chapters
#' @export
icd9GetChapters <- function(icd9, isShort = icd_guess_short(icd9), verbose = FALSE) {
  icd9_get_chapters(icd9, isShort, verbose)
}

#' @rdname icd_long_to_wide
#' @export
icd9LongToWide <- function(icd9df,
                           visitId =  get_visit_name(icd9df),
                           icd9Field = get_icd_name(icd9df),
                           prefix = "icd_",
                           min.width = 0,
                           aggregate = TRUE,
                           return.df = FALSE) {
  .Deprecated("icd_long_to_wide")
  icd_long_to_wide(x = icd9df, visit_name = visitId, icd_name = icd9Field, prefix = prefix,
    min_width = min.width, agg = aggregate, return_df = return.df)
}

#' @rdname icd_wide_to_long
#' @export
icd9WideToLong <- function(x,
                           visitId = get_visit_name(x),
                           icdLabels = NULL,
                           icdName = "icdCode",
                           icdRegex = c("icd", "diag", "dx_", "dx"),
                           verbose = FALSE) {
  .Deprecated("icd_wide_to_long")
  icd_wide_to_long(x = x, visit_name = visitId, icd_labels = icdLabels, icd_name = icdName, icd_regex = icdRegex)
}

icd9DecimalToParts <- function(icd9Decimal, minorEmpty = "") {
  .Deprecated("icd_decimal_to_parts")
  icd9DecimalToParts(icd9Decimal, minorEmpty)
}

#' @rdname icd_short_to_decimal
#' @export
icd9DecimalToShort <- function(icd9Short) {
  .Deprecated("icd_decimal_to_short")
  icd_decimal_to_short.icd9(icd9Short)
}

#' @rdname icd_short_to_decimal
#' @export
icd9ShortToDecimal <- function(icd9Short) {
  .Deprecated("icd_short_to_decimal")
  icd_short_to_decimal.icd9(icd9Short)
}

