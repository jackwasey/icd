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

#' check whether a code is major
#' @description move to C++ or own R file:
#' @param icd10 character vector
#' @import checkmate stringr
icd_is_major <- function(icd10) {
  assertCharacter(icd10)
  # this is easier than ICD-9
  stringr::str_detect(icd10, "^[[:space:]]*[[:alpha:]][[:digit:]][[:alnum:]][[:space:]]*$")
}


#' Test whether ICD-9 or ICD-10 codes are valid
#'
#' TODO: add default (when there is no class) which detected icd9 vs 10 if
#' possible. TODO: use "short" or "long" attribute if available to tighten
#' validation, or guess if missing.
#' @param icd An ICD-9 or 10 code. If the class is set to \code{'icd9'},
#'   \code{'icd10'}, \code{'icd10cm'} etc then perform appropriate validation.
icd_is_valid <- function(x)
  UseMethod("icd_is_valid")

#' @describeIn icd_is_valid Test whether generic ICD-10 code is valid
#' @import checkmate stringr
icd_is_valid.icd10 <- function(icd) {
  assertCharacter(icd)
  # SOMEDAY: check whether code has 'year' attribute. This is maybe more for testing 'realness'
  # start with a broad regex
  icd %>% str_trim %>% str_detect("^[[:space:]]*[[:alpha:]][[:digit:]][[:alnum:]]\\.?[[:alnum:]]{0,4}[[:space:]]*$")
}

#' @details From WHO ICD-10 manual: "The basic ICD is a single coded list of
#' three-character categories, each of which can be further divided into up to
#' 10 four-character subcategories. In place of the purely numeric coding system
#' of previous revisions, the 10th revision uses an alphanumeric code with a
#' letter in the first position and a number in the second, third and fourth
#' positions. The fourth character follows a decimal point. Possible code
#' numbers therefore range from A00.0 to Z99.9. The letter U is not used (see
#' Section 2.4.7)."
#'
#' "Although not mandatory for reporting at the international level, most of the
#' three-character categories are subdivided by means of a fourth, numeric
#' character after a decimal point, allowing up to 10 subcategories. Where a
#' three-character category is not subdivided, it is recommended that the letter
#' ‘X’ be used to fill the fourth position, so that the codes are of a standard
#' length for data-processing."
#'
#' officially, WHO standard goes to 3+1 digits, but there are officially
#' sanctioned extensions which use additional digits, e.g. neurology,
#' pediatrics, and of course ICD-10-CM.
icd_is_valid.icd10who <- function(icd, strict = FALSE) {
  assertCharacter(icd)
  # SOMEDAY: check whether code has 'year' attribute. This is maybe more for testing 'realness'
  # start with a broad regex
  if (strict) {
    icd %>%
      str_trim %>%
      str_detect("^[[:space:]]*[[:alpha:]][[:digit:]][[:digit:]]\\.?(X|[[:digit:]])[[:space:]]*$")
  } else {
    icd %>%
      str_trim %>%
      str_detect("^[[:space:]]*[[:alpha:]][[:digit:]][[:digit:]]\\.?(X|[[:digit:]]*)[[:space:]]*$")
  }
}

icd_is_valid.icd10cm <- function(icd) {
  stop("need to implement. WHO or ICD-10-CM as default? WHO ICD-10 is not actually a subset of ICD-10-CM.")
}
