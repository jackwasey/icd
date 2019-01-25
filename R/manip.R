#' extract alphabetic, and numeric part of ICD-9 code prefix
#'
#' removes white space and separates V or E if present.
#' @template icd9-any
#' @return vector or matrix, with first item of each row (or whole vector)
#'   containing V, E or "". The second part contains the numeric parts of the
#'   code, which may include a decimal point.
#' @keywords internal manip
icd9_extract_alpha_numeric <- function(x) {
  assert(check_factor(x), check_character(x))
  # generate list, then flip into a matrix with a row for each code, and the
  # alpha part in first column, and numeric part in the second
  t(
    vapply(
      str_match_all(as_char_no_warn(x),
                    pattern = "([VvEe]?)([[:digit:].]+)"),
      FUN = function(y) matrix(data = y[2:3], nrow = 1, ncol = 2),
      FUN.VALUE = c(NA_character_, NA_character_)))
}

#' drop zero padding from decimal ICD-9 code.
#'
#' decimal form ICD-9 codes are not ambiguous if the leading zeroes are dropped.
#' Some short-form ICD-9 codes would not be ambiguous, e.g. "1" but many would
#' be problematic. This is the inverse of \code{icd9_add_leading_zeroes}.
#'
#' Invalid codes have no guaranteed result, and may give NA, or a (possibly
#' valid) code in response.
#' @template icd9-any
#' @return character vector of ICD-9 codes with extra zeroes dropped from major
#'   part
#' @keywords internal manip
icd9_drop_leading_zeroes <- function(x, short_code = guess_short(x)) {
  assert(check_factor(x), check_character(x))
  assert(check_null(short_code), check_flag(short_code))

  if (short_code) {
    parts <- short_to_parts.icd9(x = x, mnr_empty = "")
    # very important: only drop the zero in V codes if the minor part is empty.
    are_empty <- parts[["mnr"]] == ""
    x[are_empty] <- icd9_drop_leading_zeroes_major(parts[are_empty, "mjr"])
    x
  } else {
    gsub("[[:space:]]*([EeVv]?)(0*)([\\.[:digit:]]*)[[:space:]]*", "\\1\\3", x)
  }
}

icd9_add_leading_zeroes <- function(x, short_code = guess_short(x)) {
  assert(check_factor(x), check_character(x))
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
  major <- trim(major)
  # not checking validity, necessarily, just quick check
  is_v <- icd9_is_v(major)
  is_n <- icd9_is_valid_major_n(major)
  major[is_v] <- sub(
    pattern = "^[[:space:]]*([Vv])0([[:digit:]])[[:space:]]*$",
    replacement = "\\1\\2",
    x = major[is_v])
  #just replace the FIRST string of zeros everything else is passed through
  major[is_n] <- sub(pattern = "^[[:space:]]*0{1,2}",
                     replacement = "",
                     x = major[is_n])
  major
}
