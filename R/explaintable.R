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
#' Tabulates the decimal format alongside converted short form (cms format)
#' with a boolean column to indicate if the code is parent.
#' A column for source year may be added in the future.
#'
#' Convert 'decimal' format (123.45 style) ICD-9 codes into the name and
#' description for human review there are official ICD9-CM data tables, not with
#' conversion to decimal notation, but to the textual format.
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
#' @import checkmate
#' @return data frame, or list of data frames, with fields for ICD-9 code, name
#'   and description. The ordering is in the same order as input.
#' @export
icd_explain_table <- function(...)
    UseMethod("icd_explain_table")

#' @describeIn icd_explain_table explaining ICD codes from a character vector, guessing ICD version
#' @export
#' @keywords internal
icd_explain_table.default <- function(x, short_code = icd_guess_short(x), condense = FALSE, brief = TRUE, warn = TRUE, ...) {

  switch(
    icd_guess_version.character(as_char_no_warn(x), short_code = short_code),
    "icd9" = icd_explain_table.icd9(x, short_code = short_code, condense = condense, brief = brief, warn = warn, ...),
    "icd10" = icd_explain_table.icd10(x, short_code = short_code, brief = brief, ...),
    stop("Unknown ICD version.")
  )
}

#' @describeIn icd_explain explain character vector of ICD-9 codes.
#' @export
#' @keywords internal
icd_explain_table.icd9 <- function(...) {
  icd_explain_table.icd9cm(...)
}

shortcode_icd9 <- function(x, short_code = icd:::icd_guess_short(x)) {
  if (!short_code){  # short code is false
    icd:::icd_decimal_to_short.icd9(x)
  }else{
    x
  }
}

#' @importFrom magrittr %>%
#' @import dplyr
lookup_icd9 <- function(x) {

  # must have major code.
  data.frame(input = x, shortcode = shortcode_icd9(x), stringsAsFactors = F) %>%
    left_join(., icd::icd9cm_hierarchy, by = c("shortcode" = "code") ) %>%
    rename(major_desc = major ) %>%
    mutate(major_desc = as.character(major_desc))
}

#' @describeIn icd_explain explain character vector of ICD-9-CM codes
#' @author Ed Lee
#' @export
#' @keywords internal
icd_explain_table.icd9cm <- function(x, short_code = icd:::icd_guess_short(x),
                                condense = FALSE, brief = TRUE, warn = TRUE, ...) {
  assert(checkCharacter(x), checkFactor(x))
  assert_flag(short_code)
  assert_flag(condense)
  assert_flag(brief)
  assert_flag(warn)

  # build desired columns
  outcols <- c("input", "shortcode", "three_digit", "majorcode", "ismajor",
                "major_desc", "long_desc", "short_desc") %>%
    (function(x) if (!brief) c(x,  "chapter", "sub_chapter") else x) %>%
    (function(x) if (condense) c(x, "numcondensed") else x)

  exptable <- lookup_icd9(x) %>%
    mutate(majorcode = icd:::icd_get_major.icd9(shortcode, short_code = TRUE)) %>%
    mutate(ismajor = input == majorcode)

  exptable %>%
      (function(x) if (condense) condense_explain_table(x) else x) %>%
      select_(., .dots = outcols)
}

#' @importFrom magrittr %>%
#' @import dplyr
condense_explain_table <- function(lookup) {

  # must have major code.
  # add description column of the major codes to lookup df
  add_majordesc_cols <- function(lookup) {
    majorlookup <- lookup_icd9(lookup$majorcode)
    lookup$major_shortdesc <-  majorlookup$short_desc
    lookup$major_longdesc <-  majorlookup$long_desc
    lookup
  }

  out_condensed <- lookup %>% group_by(majorcode) %>%
    mutate(condensedcodes = paste(input, collapse = ", ") ) %>% # concatenation of codes
    mutate(numcondensed = n()) %>% # number of condensed rows
    mutate(input = ifelse(numcondensed > 1, majorcode, input)) %>%
    mutate(shortcode = ifelse(numcondensed > 1, majorcode, shortcode)) %>%
    mutate(ismajor = ifelse(numcondensed > 1, T, F)) %>%
    data.frame %>% # convert to data.frame because next transformation don't want to mutate on group level.
    add_majordesc_cols %>%
    mutate(short_desc = ifelse(numcondensed > 1, major_shortdesc, short_desc) ) %>%
    mutate(long_desc = ifelse(numcondensed > 1, major_longdesc, long_desc) ) %>%
    select(-major_shortdesc, -major_longdesc) %>%
    filter(!duplicated(shortcode))
  out_condensed
}

#' @describeIn icd_explain ICD-10 explanation, current a minimal implementation
#' @export
#' @keywords internal
icd_explain_table.icd10 <- function(x, short_code = icd_guess_short(x),
                                    condense = FALSE, brief = TRUE, warn = TRUE, ...) {
  assert_vector(x)
  assert_flag(short_code)
  assert_flag(brief)

  if (!missing(condense))
    .NotYetUsed("condense", error = FALSE)
  if (!missing(warn))
    .NotYetUsed("warn", error = FALSE)

  if (!short_code)
    x <- icd_decimal_to_short.icd10(x)

  icd::icd10cm2016[icd::icd10cm2016[["code"]] %in% unique(as_char_no_warn(x)),
                   ifelse(brief, "short_desc", "long_desc")]
}
