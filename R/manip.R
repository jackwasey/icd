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

#' @title extract alphabetic, and numeric part of icd9 code prefix
#' @description removes whitespace and separates V or E if present.
#' @template icd9-any
#' @return vector or matrix, with first item of each row (or whole vector)
#'   containing V, E or "". The second part contains the numeric parts of the
#'   code, which may include a decimal point.
#' @keywords internal manip
icd_extract_alpha_numeric <- function(x) {
  assert(checkFactor(x), checkCharacter(x))
  # generate list, then flip into a matrix with a row for each code, and the
  # alpha part in first column, and numeric part in the second
  asCharacterNoWarn(x) %>% 
    str_match_all(pattern = "([VvEe]?)([[:digit:].]+)") %>%
    vapply(FUN = function(y) matrix(data = y[2:3], nrow = 1, ncol = 2),
           FUN.VALUE = rep(NA_character_, times = 2)) %>% t
}

#' @title drop zero padding from decimal ICD-9 code.
#' @description decimal form ICD-9 codes are not ambiguous if the leading zeroes
#'   are dropped. Some short-form ICD-9 codes would not be ambiguous, e.g. "1"
#'   but many would be problematic. This is the inverse of
#'   \code{icd9AddLeadingZeroesDecimal}.
#'
#'   Invalid codes have no guaranteed result, and may give NA, or a (possibly
#'   valid) code in repsonse.
#' @template icd9-any
#' @return character vector of ICD-9 codes with extra zeroes dropped from major
#'   part
#' @family ICD-9 convert
#' @keywords internal manip
icd9_drop_leading_zeroes <- function(x, short_code) {
  UseMethod("icd9_drop_leading_zeroes")
}

#' @describeIn icd9_drop_leading_zeroes Drop leading zeroes from a decimal format ICD-9 code
icd9_drop_leading_zeroes.decimal_code <- function(x) {
  assert(checkFactor(x), checkCharacter(x))
  
  x %>% asCharacterNoWarn %>% 
    str_match_all( pattern = "[[:space:]]*([EeVv]?)(0*)([\\.[:digit:]]+)[[:space:]]*") %>%
    vapply(FUN = function(y) if (length(y) > 0) sprintf("%s%s", x[2], x[4]) else NA_character_ ,
           FUN.VALUE = character(1))
}

#' @describeIn icd9_drop_leading_zeroes Drop leading zeroes from a short format ICD-9 code
icd_drop_leading_zeroes.short_code <- function(x) {
  assert(checkFactor(x), checkCharacter(x))
  parts <- icd_short_to_parts.icd9(x = x, minor_empty = "")
  # very important: only drop the zero in V codes if the minor part is empty.
  areEmpty <- parts[["minor"]] == ""

  x[areEmpty] <- icd9DropLeadingZeroesMajor(parts[areEmpty, "major"])
  icd9Short
}

#' @rdname icd9DropLeadingZeroes
icd9DropLeadingZeroesMajor <- function(major) {
  # (valid) E codes don't ever have leading zeroes
  # major can legitimately be an integer
  major <- trim(major) # my trim loses encoding, but this shouldn't matter
  isV <- icd9IsV(major) # not checking validity, necessarily, just quick check
  isN <- icd9_is_valid_major_n(major)
  major[isV] <- sub(pattern = "^([Vv])0([[:digit:]]{1})$",
                    replacement = "\\1\\2",
                    x = major[isV])
  #just replace the FIRST string of zeros everything else is passed through
  major[isN] <- sub(pattern = "^[[:space:]]*0{1,2}",
                    replacement = "",
                    x = major[isN])
  major
}
