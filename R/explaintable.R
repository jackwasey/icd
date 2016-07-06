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
# globalVariables

globalVariables(c("major_desc",
                  "validicd9",
                  "validicd10",
                  "ismajor",
                  "major",
                  "majorcode",
                  "numcondensed",
                  "shortcode",
                  "major_shortdesc",
                  "major_longdesc",
                  "shortcodeicd9",
                  "shortcodeicd10",
                  "majorcode9",
                  "majorcode10",
                  "input",
                  "short_desc",
                  "long_desc",
                  "."
                  ),
                add = TRUE)


#' Explain ICD-9 and ICD-10 codes in English from decimal  (123.45 style),
#' Tabulates the decimal format alongside converted non-decimal format.
#'
#' Output is ordered in the same order as the input. A boolean column ismajor
#' indicates if the code is a parent Category.
#'
#' A column for source year may be added in the future.
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
#' @param warn single logical value, default is \code{TRUE}, meaning that codesÏ
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
#' @details If the input x is of mixed type it will choose to convert by
#' @details majority of icd9 vs. icd19
#' @export
#' @keywords internal
icd_explain_table.default <- function(x, short_code = icd_guess_short(x), condense = FALSE, brief = TRUE, warn = TRUE, ...) {

  if (!(is.icd9(x) | is.icd10(x))) {
    icd_explain_table.character(x,
                                condense = condense,
                                brief = brief,
                                warn = warn)
  } else {
    switch(
      icd_guess_version.character(as_char_no_warn(x), short_code = short_code),
      "icd9" = icd_explain_table.icd9(x, short_code = short_code, condense = condense, brief = brief, warn = warn, ...),
      "icd10" = icd_explain_table.icd10(x, short_code = short_code, brief = brief, ...),
      stop("Unknown ICD version.")
    )
  }
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

shortcode_icd9 <- function(x, short_code = icd_guess_short(x)) {
  if (!short_code) icd_decimal_to_short.icd9(x) else x
}

shortcode_icd10 <- function(x, short_code = icd_guess_short(x)) {
  if (!short_code) icd_decimal_to_short.icd10(x) else x
}

#' Lookup ICD9 decimal from source data in icd9cm_hierarchy
#' @param x a vector of icd9 codes
#' @import magrittr
#' @import dplyr
lookup_icd9 <- function(x) {

  # must have major code.
  data.frame(input = x, shortcode = shortcode_icd9(x), stringsAsFactors = F) %>%
    left_join( y = icd::icd9cm_hierarchy, by = c("shortcode" = "code") ) %>%
    rename(major_desc = major ) %>%
    mutate(major_desc = as.character(major_desc))
}

#' Lookup ICD10 decimal from source data in icd10cm2016
#' @param x a vector of icd9 codes
#' @import magrittr
#' @import dplyr
lookup_icd10 <- function(x) {

  # must have major code.
  data.frame(input = x, shortcode = shortcode_icd10(x), stringsAsFactors = F) %>%
    left_join(y = icd::icd10cm2016, by = c("shortcode" = "code") ) %>%
    rename(major_desc = major ) %>%
    mutate(major_desc = as.character(major_desc))
}

#' @describeIn icd_explain explain character vector of ICD-9-CM codes
#' @param tiebreak_icd9 boolean.. If a code is both valid icd9 and icd10 it will choose icd9 if T.
#' @details will automatically detect if shortcode or not
#' @author Ed Lee
#' @export
#' @keywords internal
#' @import dplyr
#' @import magrittr
icd_explain_table.character <- function(x,
                                     condense = FALSE,
                                     brief = TRUE,
                                     warn = TRUE,
                                     tiebreak_icd10 = T,
                                     ...) {
  assert(checkCharacter(x), checkFactor(x))
  assert_flag(condense)
  assert_flag(brief)
  assert_flag(warn)

  st_codes <- data.frame(input = x,
           shortcodeicd9 = shortcode_icd9(x),
           shortcodeicd10 = shortcode_icd10(x),
           stringsAsFactors = F) %>%
    mutate(validicd9 = icd_is_valid.icd9(shortcodeicd9)) %>%
    mutate(validicd10 = icd_is_valid.icd10(shortcodeicd10)) %>%
    mutate(majorcode9 = icd_get_major.icd9(shortcodeicd9, short_code = TRUE)) %>%
    mutate(majorcode10 = icd_get_major.icd10(shortcodeicd10))

   st_codes9 <- st_codes %>% dplyr::filter(validicd9 == T & validicd10 == F)
   st_codes10 <- st_codes %>% dplyr::filter(validicd10 == T)  # if also a validicd9 will return icd10

   out9 <- icd_explain_table.icd9(st_codes9$input,
                                  brief = brief,
                                  condense = condense)

   out10 <- icd_explain_table.icd10(st_codes10$input,
                                   brief = brief,
                                   condense = condense)

   if (!condense) {
     st_codes %>%
       select(input) %>%
       left_join(y = rbind(out9, out10), by = c("input"))
   } else {
     # collapse short codes and left_join on explanation table
     st_codes %>% group_by(majorcode9) %>%
       mutate(numcondensed = n()) %>%
       data.frame %>%
       filter(!duplicated(majorcode9)) %>% # majorcode9 regardless if icd10
       mutate(input = ifelse(numcondensed > 1, majorcode9, input)) %>%
       select(input) %>%
       left_join(y = rbind(out9, out10), by = c("input"))
   }
}

#' @describeIn icd_explain_table explain character vector of ICD-9-CM codes
#' @author Ed Lee
#' @import dplyr
#' @import magrittr
#' @export
#' @keywords internal
icd_explain_table.icd9cm <- function(x,
                                short_code = icd_guess_short(x),
                                condense = FALSE,
                                brief = TRUE,
                                warn = TRUE, ...) {
  assert(checkCharacter(x), checkFactor(x))
  assert_flag(short_code)
  assert_flag(condense)
  assert_flag(brief)
  assert_flag(warn)

  # build desired columns
  outcols <- c("input", "shortcode", "three_digit", "majorcode", "ismajor",
               "major_desc", "long_desc", "short_desc", "validicd9", "validicd10") %>%
    (function(x) if (!brief) c(x,  "chapter", "sub_chapter") else x) %>%
    (function(x) if (condense) c(x, "condensedcodes", "numcondensed") else x)

  exptable <- lookup_icd9(x) %>%
    mutate( majorcode = icd_get_major.icd9(shortcode, short_code = TRUE)) %>%
    mutate( ismajor = input == majorcode) %>%
    mutate( validicd9 = icd_is_valid.icd9(shortcode)) %>%
    mutate( validicd10 = icd_is_valid.icd10(shortcode))

  exptable %>%
      (function(x) if (condense) condense_explain_table(x) else x) %>%
      select_(.dots = outcols)
}

#' @describeIn icd_explain_table explain character vector of ICD1-10-CM codes
#' @author Ed Lee
#' @import dplyr
#' @import magrittr
#' @export
#' @keywords internal
icd_explain_table.icd10cm <- function(x, short_code = icd_guess_short(x),
                                     condense = FALSE, brief = TRUE, warn = TRUE, ...) {
  assert(checkCharacter(x), checkFactor(x))
  assert_flag(short_code)
  assert_flag(condense)
  assert_flag(brief)
  assert_flag(warn)

  # build desired columns
  outcols <- c("input", "shortcode", "three_digit", "majorcode", "ismajor",
               "major_desc", "long_desc", "short_desc", "validicd9", "validicd10") %>%
    (function(x) if (!brief) c(x,  "chapter", "sub_chapter") else x) %>%
    (function(x) if (condense) c(x, "condensedcodes", "numcondensed") else x)

  exptable <- lookup_icd10(x) %>%
    mutate( majorcode = icd_get_major.icd10(shortcode)) %>%
    mutate( ismajor = input == majorcode) %>%
    mutate( validicd9 = icd_is_valid.icd9(shortcode)) %>%
    mutate( validicd10 = icd_is_valid.icd10(shortcode))

  exptable %>%
    (function(x) if (condense) condense_explain_table(x) else x) %>%
    select_(.dots = outcols)
}

#' @import magrittr
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

  # Groups by major code then concatenates them into condensedcodes column.
  # For those condensed rows will overwrite descriptions with major descriptions
  out_condensed <- lookup %>% group_by( majorcode) %>%
  mutate( condensedcodes = paste(input, collapse = ", ")) %>%
    mutate( numcondensed = n()) %>% # number of condensed rows
    mutate( input = ifelse(numcondensed > 1, majorcode, input)) %>%
    mutate( shortcode = ifelse(numcondensed > 1, majorcode, shortcode)) %>%
    mutate( ismajor = ifelse(numcondensed > 1, T, F)) %>%
    data.frame %>% # convert to data.frame to ungroup
    add_majordesc_cols %>%
    mutate( short_desc = ifelse(numcondensed > 1, major_shortdesc, short_desc)) %>%
    mutate( long_desc = ifelse(numcondensed > 1, major_longdesc, long_desc)) %>%
    select( c(-major_shortdesc, -major_longdesc)) %>%
    filter( !duplicated(shortcode))
  out_condensed
}
