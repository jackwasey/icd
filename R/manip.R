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

#' extract alphabetic, and numeric part of ICD-9 code prefix
#'
#' removes white space and separates V or E if present.
#' @template icd9-any
#' @return vector or matrix, with first item of each row (or whole vector)
#'   containing V, E or "". The second part contains the numeric parts of the
#'   code, which may include a decimal point.
#' @keywords internal manip
icd9_extract_alpha_numeric <- function(x) {
  assert(checkmate::checkFactor(x), checkmate::checkCharacter(x))
  # generate list, then flip into a matrix with a row for each code, and the
  # alpha part in first column, and numeric part in the second
  as_char_no_warn(x) %>%
    str_match_all(pattern = "([VvEe]?)([[:digit:].]+)") %>%
    vapply(FUN = function(y) matrix(data = y[2:3], nrow = 1, ncol = 2),
           FUN.VALUE = c(NA_character_, NA_character_)) %>% t
}

#' drop zero padding from decimal ICD-9 code.
#'
#' decimal form ICD-9 codes are not ambiguous if the leading zeroes are dropped.
#' Some short-form ICD-9 codes would not be ambiguous, e.g. "1" but many would
#' be problematic. This is the inverse of \code{icd9_add_leading_zeroes}.
#'
#' Invalid codes have no guaranteed result, and may give NA, or a (possibly
#' valid) code in response.
#' @examples
#' \dontrun{
#' library(microbenchmark)
#' library(stringr)
#' x <- icd:::generate_random_decimal_icd9(1e6)
#' microbenchmark(
#'   x %>% as_char_no_warn %>%
#'   str_replace("[[:space:]]*([EeVv]?)(0*)([\\.[:digit:]]*)[[:space:]]*", "\\1\\3"),
#'
#'   str_replace(as_char_no_warn(x),
#'               "[[:space:]]*([EeVv]?)(0*)([\\.[:digit:]]*)[[:space:]]*", "\\1\\3"),
#'
#'   gsub("[[:space:]]*([EeVv]?)(0*)([\\.[:digit:]]*)[[:space:]]*", "\\1\\3", x),
#'   times = 1000
#'   )
#' }
#' @template icd9-any
#' @return character vector of ICD-9 codes with extra zeroes dropped from major
#'   part
#' @family ICD-9 convert
#' @keywords internal manip
icd9_drop_leading_zeroes <- function(x, short_code = icd_guess_short(x)) {
  assert(checkmate::checkFactor(x), checkmate::checkCharacter(x))
  assert(checkmate::checkNull(short_code), checkmate::checkFlag(short_code))

  if (short_code) {
    parts <- icd_short_to_parts.icd9(x = x, minor_empty = "")
    # very important: only drop the zero in V codes if the minor part is empty.
    are_empty <- parts[["minor"]] == ""
    x[are_empty] <- icd9_drop_leading_zeroes_major(parts[are_empty, "major"])
    x
  } else {
    gsub("[[:space:]]*([EeVv]?)(0*)([\\.[:digit:]]*)[[:space:]]*", "\\1\\3", x)
  }
}

#' @keywords internal manip
icd9_add_leading_zeroes <- function(x, short_code = icd_guess_short(x)) {
  assert(checkmate::checkFactor(x), checkmate::checkCharacter(x))
  assert_flag(short_code)
  if (is.factor(x)) {
    levels(x) <- icd9_add_leading_zeroes_cpp(levels(x), short_code)
    x
  } else icd9_add_leading_zeroes_cpp(x, short_code)
}

#' @rdname icd9_drop_leading_zeroes
#' @keywords internal manip
icd9_drop_leading_zeroes_major <- function(major) {
  # (valid) E codes from 000 exist. Dropping zeroes from E000 would require a
  # lot of logic for no current benefit. Defer this until it is a problem.
  major %<>% str_trim
  # not checking validity, necessarily, just quick check
  is_v <- icd9_is_v(major)
  is_n <- icd9_is_valid_major_n(major)
  major[is_v] %<>% str_replace("^[[:space:]]*([Vv])0([[:digit:]])[[:space:]]*$",
                               replacement = "\\1\\2")
  #just replace the FIRST string of zeros everything else is passed through
  major[is_n] %<>% str_replace("^[[:space:]]*0{1,2}", replacement = "")
  major
}
