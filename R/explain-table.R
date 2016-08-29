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

#' Explain ICD-9 and ICD-10 codes in English from decimal  (123.45 style),
#' Tabulates the decimal format alongside converted non-decimal format.
#'
#' Output is ordered in the same order as the input. A boolean column ismajor
#' indicates if the code is a parent Category.
#'
#' If the code is both a valid ICD9 and a ICD10 the output will default the
#' descriptions to ICD10.  The code would otherwise have to be explicitly casted
#' to get ICD9 descriptions.
#'
#' A column for source year may be added in the future. Other changes may occur
#' this new feature gets testing and use.
#'
#' @param x vector or other structure of ICD codes to explain in human language
#' @template short_code
#' @param condense single logical value which indicates whether to condense the
#'   given set of ICD-9 codes by replacing subsets of codes with 'parent' codes
#'   which exactly encompass certain subsets. E.g. If all cholera diagnoses are
#'   provided, only '001 - Cholera' needs to be displayed, not all sub-types.
#' @param brief single logical value, default is \code{FALSE}. If \code{TRUE},
#'   the short description from the canonical CMS descriptions (included in
#'   \code{data-raw}) will be used, otherwise the long description is used.
#' @param warn single logical value, default is \code{TRUE}, meaning that codes
#'   which do not correspond to diagnoses, or to three-digit codes, will trigger
#'   a warning.
#' @template dotdotdot
#' @return data frame with fields for ICD-9 code, name and description. The
#'   ordering is in the same order as input, including rows filled with NA for
#'   invalid input codes
#' @export
icd_explain_table <- function(...)
  UseMethod("icd_explain_table")

#' @describeIn icd_explain_table explaining ICD codes from a character vector, guessing ICD version
#' @details If the input x is of mixed type it will choose to convert by
#' @details majority of icd9 vs. icd19
#' @export
#' @keywords internal
icd_explain_table.default <- function(x, short_code = icd_guess_short(x), condense = FALSE,
                                      brief = TRUE, warn = TRUE, ...) {
  switch(
    icd_guess_version(x, short_code = short_code),
    "icd9" = icd_explain_table.icd9cm(x, short_code = short_code, condense = condense,
                                      brief = brief, warn = warn, ...),
    "icd10" = icd_explain_table.icd10cm(x, short_code = short_code, condense = condense,
                                        brief = brief, warn = warn, ...),
    stop("Unknown ICD version.")
  )
}

#' @describeIn icd_explain_table from vector of ICD-9 codes.
#' @export
#' @keywords internal
icd_explain_table.icd9 <- function(...) {
  icd_explain_table.icd9cm(...)
}

#' @describeIn icd_explain_table from vector of ICD-10 codes.
#' @export
#' @keywords internal
icd_explain_table.icd10 <- function(...) {
  icd_explain_table.icd10cm(...)
}

# TODO: these highlight the need for as.icd_short_code to convert if not already
# short. icd_short_code(x) can just set the attr
shortcode_icd9 <- function(x, short_code = icd_guess_short(x)) {
  if (!short_code) icd_decimal_to_short.icd9(x) else x
}

shortcode_icd10 <- function(x, short_code = icd_guess_short(x)) {
  if (!short_code) icd_decimal_to_short.icd10(x) else x
}

#' generate table of ICD code explanations
#'
#' common code for generating full table of explanations of ICD codes
#' @author Ed Lee
#' @keywords internal
icd_explain_table_worker <- function(x, hierarchy, short_code, condense,
                                     brief, warn, ...) {
  assert(check_character(x), check_factor(x))
  assert(check_data_frame(hierarchy))
  assert_flag(short_code)
  assert_flag(condense)
  assert_flag(brief)
  assert_flag(warn)

  xs <- if (!short_code) icd_decimal_to_short.icd9(x) else x

  exptable <- merge(data.frame(code = xs, stringsAsFactors = FALSE), hierarchy, all.x = TRUE)
  # merge has reordered...
  exptable[["is_major"]] <- exptable[["three_digit"]] == exptable[["code"]]
  exptable[["valid_icd9"]] <- icd_is_valid.icd9(xs, short_code = TRUE)
  exptable[["valid_icd10"]] <- icd_is_valid.icd10(xs, short_code = TRUE)

  if (condense)
    condense_explain_table(exptable)
  else
    exptable[match(xs, exptable[["code"]]), ]
}

#' @describeIn icd_explain_table explain character vector of ICD1-10-CM codes
#' @author Ed Lee
#' @export
#' @keywords internal
icd_explain_table.icd9cm <- function(x, short_code = icd_guess_short(x),
                                     condense = FALSE, brief = TRUE, warn = TRUE, ...) {
  icd_explain_table_worker(x = x, hierarchy = icd::icd9cm_hierarchy,
                           short_code = short_code, condense = condense,
                           brief = brief, warn = warn, ...)
}

#' @describeIn icd_explain_table explain character vector of ICD1-10-CM codes
#' @author Ed Lee
#' @export
#' @keywords internal
icd_explain_table.icd10cm <- function(x, short_code = icd_guess_short(x),
                                      condense = FALSE, brief = TRUE, warn = TRUE, ...) {
  icd_explain_table_worker(x = x, hierarchy = icd::icd10cm2016,
                           short_code = short_code, condense = condense,
                           brief = brief, warn = warn, ...)
}

#' condense icd_explain_table output down to major codes
#'
#' if a major code appears in the code column, and any children of that major
#' code, the children are aggregated to a list and added to the major code row.
#' This does currently not 'condense' e.g. middle-order codes
#'
#' Unlike icd_explain_table, preserving order doesn't make sense, since rows
#' anywhere in the list can be aggregated, thus altering order compared to the
#' input. Size of the output will also be different if any condensing was done.
#' @keywords internal
condense_explain_table <- function(x) {
  condensed_majors <- condense_explain_table_worker(x)
  if (nrow(condensed_majors) == 0) {
    x[["condensed_codes"]] <- x[["code"]]
    x[["condensed_num"]] <- 1L
    return(x)
  }
  # drop rows where major is present in the input
  x <- x[x[["three_digit"]] %nin% x[["code"]] | x$is_major, ]
  # add condensed merge existing major rows
  out <- merge(x, condensed_majors, by.x = "code", by.y = "three_digit", all.x = TRUE)
  # NA values are un-condensed, so just fill out:
  out[is.na(out$condensed_codes), "condensed_codes"] <- out[is.na(out$condensed_codes), "code"]
  out[is.na(out$condensed_num), "condensed_num"] <- 1L
  out
}

#' generate condensed code and condensed number columns
#'
#' @return details for condesable rows
#' @keywords internal
condense_explain_table_worker <- function(x) {
  # we can only condense when we have three_digit major
  x <- x[!is.na(x[["three_digit"]]), ]
  if (nrow(x) == 0) return(data.frame())
  condensed <- aggregate(x["code"], by = list(x[["three_digit"]]), paste, sep = ", ", collapse = ", ")
  # code column in result is now a factor, by default
  names(condensed) <- c("three_digit", "condensed_codes")
  condensed[["condensed_num"]] <- aggregate(x["code"], by = list(x[["three_digit"]]), length)[[2]]
  condensed[condensed[["condensed_num"]] != 1L, ]
}
