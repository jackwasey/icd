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

#' @title take two ICD-9 codes and expand range to include all child codes
#' @description this is cumbersome code, covering a whole load of edge cases
#'   relating to the fact that icd9 codes are \strong{not} in numeric order. An
#'   alternative strategy would be to list all the ICD9 codes, then a range
#'   would just pick out start and finish positions, and return subset of the
#'   list. Not all ICD-9 codes are valid, including some parent codes which have
#'   valid children. However, I expect at least some of these have been used in
#'   some billing databases.
#'
#'   As with \code{link{icd9ExpandRangeShort}} great care is taken not to
#'   include codes which have children not in the range. E.g. "100.9" to "101.1"
#'   would _not_ include code "101".
#'
#'   \code{onlyReal} default is \code{TRUE} (a change from previous versions)
#'   since this is far more likely to be useful to the end user.
#' @examples
#' "4280 " %i9s% "4289 "
#' "4280 " %i9s% "42821"
#' "42799 " %i9sa% "42802" # doesn't include 428 or 4280
#' "427.99 " %i9da% "428.02"
#' "V80 " %i9s% " V810 "
#' @templateVar icd9ShortName start,end
#' @template icd9-short
#' @template onlyReal
#' @template short_code
#' @param excludeAmbiguousStart single logical value, if \code{TRUE} the range
#'   returned will not include codes which are explicitly listed in the range,
#'   but would imply a broader range than specified. E.g. \code{V10 %%i9sa%%
#'   V1009} would by default (\code{FALSE}) include \code{V10} even though
#'   \code{V10} itself is parent to everything up to \code{V11}.
#' @param excludeAmbiguousEnd single logical, same as
#'   \code{excludeAmbiguousStart} but affects codes at the end of the range.
#'   E.g. 99.99 to 101.01 would by default exclude 101 and 101.0
#' @family ICD-9 ranges
#' @export
icd_expand_range <- function(start, end, ...) {
  assertScalar(start) # i'll permit numeric but prefer char
  assertScalar(end)
  UseMethod("icd_expand_range")
}

#' @describeIn icd_expand_range Expand a range of ICD-9 or ICD-10 codes when the
#'   class is not known
#' @details When the class is not known, it must be guessed from the start and
#'   end codes. If this guessing fails, e.g. start is ICD-9 whereas end is
#'   ICD-10, then an error is thrown. Otherwise, the appropriate S3 method is
#'   called.
#' @export
icd_expand_range.character <- function(start, end, short_code = NULL, defined = TRUE, ...) {

  start_guess <- icd_guess_version.character(start, short_code = short_code)
  end_guess <- icd_guess_version.character(start, short_code = short_code)
  if (start_guess != end_guess)
    stop("Cannot expand range because ICD code version cannot be guessed from ", start,
         " and ", end, ". Either specify the classes, e.g. icd9(\"100.4\"), or call the
       S3 method directly, e.g. icd_expand_range.icd9")
  if (start_guess == "icd9") {
    if (is.null(short_code)) short_code <- icd_guess_short.icd9(c(start, end))
    icd_expand_range.icd9(start, end, short_code = short_code, defined = defined, ...)
  } else if (start_guess == "icd10") {
    if (is.null(short_code)) short_code <- icd_guess_short.icd10(c(start, end))
    icd_expand_range.icd10cm(start, end, short_code = short_code, defined = defined, ...)
  } else {
    stop("Unknown ICD type")
  }
}

#' expand range of ICD-10 codes returning only defined codes in ICD-10-CM
#'
#' This will need generalizing to any list of 'defined' codes, e.g. WHO or other
#' @param start character vector of length one containing an ICD code
#' @param end  character vector of length one containing an ICD code
#' @keywords internal
icd_expand_range.icd10cm <- function(start, end, short_code = icd_guess_short.icd10(c(start, end)),
                                     defined = TRUE, ...) {
  if (!defined)
    stop("expanding ranges of possible (versus defined) ICD-10-CM codes is not yet implemented.
         It will produce a very large number of codes because of permutations.")

  # check whether valid?
  # check whether defined?
  # minimal check for type:
  stopifnot(grepl("[^.]", c(start, end)))

  # for ranges, we can semi-legitimately strip any ".x" part ( but other X values CAN appear later). Quan uses x in position 4, but I'm not aware of any ICD-10 code that does this.

  # deliberately not case sensitive, as this is a Quan quirk:
  if (substr(start, 4, 100) == "x")
    start <- substr(start, 1, 3)

  if (substr(end, 4, 100) == "x")
    end <- substr(end, 1, 3)

  # TODO: either search down supposedly well ordered list until substring of end
  # changes, or find all children, and get pos of last one.

  end_kids <- icd_children_defined.icd10cm(end, short_code = TRUE)
  new_end <- end_kids[length(end_kids)]

  # find the start and end code positions in the master list
  pos <- match(c(start, new_end), icd9::icd10cm2016[["code"]])
  if (is.na(pos[1])) stop(sprintf("start code '%s' not found", pos[1]))
  if (is.na(pos[2])) stop(sprintf("calculated end code '%s' not found", pos[2]))
  stopifnot(pos[2] >= pos[1])

  icd9::icd10cm2016[pos[1]:pos[2], "code"]
}

# WIP
icd10_expand_range_possible_short <- function(start, end) {
  assertString(start)
  assertString(end)

  stopifnot(all(icd_is_valid.icd10(c(start, end))))

  # check whether defined? No: this is specifically for undefined codes?

  start <- str_trim(start)
  end <- str_trim(end)

  start_char <- substr(start, 1, 1)
  end_char <- substr(end, 1, 1)
  stopifnot(start_char <= end_char)

  start_two_digits <- as.integer(substr(start, 2, 3))
  end_two_digits <- as.integer(substr(end, 2, 3))
  if (start_char == end_char)
    stopifnot(start_two_digits <= end_two_digits)

  start_other_chars <- substr(start, 4, 10)
  end_other_chars <- substr(end, 4, 10)
  stop("Not implemented fully", start_other_chars, end_other_chars)
}

#' @title Expand major codes to range
#' @description Expand a pair of major codes into a range of major codes.
#' @keywords internal
icd_expand_range_major <- function(start, end)
  UseMethod("icd_expand_range_major")

#' @describeIn icd_expand_range_major Expand range of top-level ICD-10 codes
#' @keywords internal
icd_expand_range_major.icd10 <- function(start, end) {
  assertString(start)
  assertString(end)

  start <- str_trim(start)
  end <- str_trim(end)

  stopifnot(icd_is_major(start), icd_is_major(end))
  stopifnot(start <= end)
  start_first <- str_sub(start, 1, 1) %>% str_to_upper()
  end_first <- str_sub(end, 1, 1) %>% str_to_upper()

  stop("not finished implementing")
  if (start_first == end_first)
    paste

}

#' @describeIn icd_expand_range Expand a range of ICD-9 codes
#' @export
icd_expand_range.icd9 <- function(start, end,
                                  short_code = icd_guess_short.icd9(c(start, end)),
                                  defined = TRUE,
                                  excludeAmbiguousStart = TRUE,
                                  excludeAmbiguousEnd = TRUE,
                                  ...) {
  if (short_code)
    icd9_expand_range_short(start, end, defined,
                            excludeAmbiguousStart,
                            excludeAmbiguousEnd)
  else
    icd9_expand_range_decimal(start, end, defined,
                              excludeAmbiguousStart,
                              excludeAmbiguousEnd)
}

#' expand range worker function
#' @keywords internal
expand_range_worker <- function(start, end, lookup, defined,
                                excludeAmbiguousStart, excludeAmbiguousEnd) {
  assertString(start)
  assertString(end)
  assertCharacter(lookup, any.missing = FALSE, min.chars = 3)
  assertFlag(excludeAmbiguousStart)
  assertFlag(excludeAmbiguousEnd)

  start_index <- match(start, lookup)
  end_index <- match(end, lookup)
  assertInteger(start_index, len = 1)
  if (is.na(start_index[1]))
    stop(sprintf("start value '%s' not found in look-up table of ICD-9 codes.", start))
  assertInteger(end_index, len = 1)
  if (is.na(end_index[1]))
    stop(sprintf("end value '%s' not found in look-up table of ICD-9 codes.", end))
  if (end_index < start_index)
    stop("end code must be greater than or equal to start code")

  if (start == end) return(icd_children.icd9(start, short_code = TRUE, defined = defined))

  out <- lookup[start_index:end_index]
  if (excludeAmbiguousStart) {
    # just remove those codes at the beginning which have children not in the output
    # let's take the first 5, to cover cases like 100, 101, 102.1, 102.11, 102.2
    starts <- utils::tail(out, 5)
    for (s in starts) {
      if (any(icd_children.icd9(s, short_code = TRUE, defined = defined) %nin% out))
        out <- out[-which(out == s)]
    }
  }
  if (excludeAmbiguousEnd) {
    # at the end, we don't want any higher-level codes at the end which would
    # have children beyond the range. There could be lots of lower level codes
    # at the end, so we actually have to search the whole list. This means that
    # even if trying to preserve the ambig start, setting ambig end will have to
    # kill it, if it spills over.
    out_cp <- out
    for (o in out_cp) {
      if (any(icd_children.icd9(o, short_code = TRUE, defined = defined) %nin% out))
        out <- out[-which(out == o)]
    }
  }
  icd_sort.icd9(
    unique(
      c(out,
        icd_children.icd9(end, short_code = TRUE, defined = defined)
      )
    ),
    short_code = TRUE
  )
}

#' @rdname icd_expand_range
#' @details  Expand range of short_code-form ICD-9 codes
#' @keywords internal
icd9_expand_range_short <- function(start, end, defined = TRUE,
                                    excludeAmbiguousStart = TRUE,
                                    excludeAmbiguousEnd = TRUE) {
  assertScalar(start) # i'll permit numeric but prefer char
  assertScalar(end)
  assertFlag(defined)
  assertFlag(excludeAmbiguousStart)
  assertFlag(excludeAmbiguousEnd)

  start <- icd9_add_leading_zeroes.icd_short_code(trim(start))
  end <- icd9_add_leading_zeroes.icd_short_code(trim(end))

  # potentially do some checks on start and end, e.g. start <= end.
  # determine whether we are doing N, V or E
  # then lookup start and end indices in sysdata.rda lookup tables

  if (defined) {
    stopifnot(icd_is_defined(start, short_code = TRUE), icd_is_defined(end, short_code = TRUE))
    if (icd9_is_n(start) && icd9_is_n(end))
      res <- expand_range_worker(start, end, icd9NShortReal, defined = TRUE,
                                 excludeAmbiguousStart, excludeAmbiguousEnd)
    else if (icd9_is_v(start) && icd9_is_v(end))
      res <- expand_range_worker(start, end, icd9VShortReal, defined = TRUE,
                                 excludeAmbiguousStart, excludeAmbiguousEnd)
    else if (icd9_is_e(start) && icd9_is_e(end))
      res <- expand_range_worker(start, end, icd9EShortReal, defined = TRUE,
                                 excludeAmbiguousStart, excludeAmbiguousEnd)
    else
      stop("mismatch between numeric, V and E types in start and end")
  } else {

    if (icd9_is_n(start) && icd9_is_n(end))
      res <- expand_range_worker(start, end, icd9NShort, defined = FALSE,
                                 excludeAmbiguousStart, excludeAmbiguousEnd)
    else if (icd9_is_v(start) && icd9_is_v(end))
      res <- expand_range_worker(start, end, icd9VShort, defined = FALSE,
                                 excludeAmbiguousStart, excludeAmbiguousEnd)
    else if (icd9_is_e(start) && icd9_is_e(end))
      res <- expand_range_worker(start, end, icd9EShort, defined = FALSE,
                                 excludeAmbiguousStart, excludeAmbiguousEnd)
    else
      stop("mismatch between numeric, V and E types in start and end")
  }
  res
}

#' @describeIn icd_expand_range_major Expand a range of ICD-9 major codes into major codes
#' @export
icd_expand_range_major.icd9 <- function(start, end, defined = TRUE) {
  assertScalar(start) # i'll permit numeric but prefer char
  assertScalar(end)
  assertFlag(defined)
  c <- icd_extract_alpha_numeric(start)
  d <- icd_extract_alpha_numeric(end)
  # cannot range between numeric, V and E codes, so ensure same type.
  stopifnot(toupper(c[1]) == toupper(d[1]))
  if (icd9IsV(start)) fmt <- "%02d" else fmt <- "%03d"
  majors <- paste(c[,1], sprintf(fmt = fmt, c[,2]:d[,2]), sep  = "")
  if (defined)
    icd_get_defined(majors, short_code = TRUE, billable = FALSE)
  else
    majors
}

#' @rdname icd_expand_range
#' @keywords internal
icd9_expand_range_decimal <- function(start, end, defined = TRUE,
                                      excludeAmbiguousStart = TRUE,
                                      excludeAmbiguousEnd = TRUE) {
  icd9(
    icd_short_code(
      icd_short_to_decimal.icd9(
        icd_expand_range.icd9(
          icd_decimal_to_short.icd9(start), icd_decimal_to_short.icd9(end),
          short_code = TRUE, defined = defined,
          excludeAmbiguousStart = excludeAmbiguousStart,
          excludeAmbiguousEnd = excludeAmbiguousEnd
        )
      )
    )
  )
}

#' @rdname icd_expand_range
#' @export
"%i9da%" <- function(start, end) {
  icd_expand_range.icd9(start, end, short_code = FALSE, defined = FALSE)
}

#' @rdname icd_expand_range
#' @export
"%i9sa%" <- function(start, end) {
  icd_expand_range.icd9(start, end, short_code = TRUE, defined = FALSE)
}

#' @rdname icd_expand_range
#' @export
"%i9d%" <- function(start, end) {
  icd_expand_range.icd9(start, end, short_code = FALSE, defined = TRUE)
}

#' @rdname icd_expand_range
#' @export
"%i9mj%" <- function(start, end) {
  icd_expand_range_major.icd9(start, end, defined = TRUE)
}

#' @rdname icd_expand_range
#' @export
"%i9s%" <- function(start, end) {
  icd_expand_range.icd9(start, end, short_code = TRUE, defined = TRUE)
}

#' @title expand decimal part of ICD-9 code to cover all possible sub-codes
#' @description Accepts a single number or character input starting point for
#'   generation of all possible decimal parts of ICD9 code. e.g. giving an empty
#'   input will fill out 111 combinations, e..g .1 .11 .12 .... .2 ....
#' @template minor
#' @param isE single logical, which if TRUE, treats the minor as part of an E
#'   code (which is one character), as opposed to a V or numeric-only code,
#'   which is two character. Default is \code{FALSE}.
#' @examples
#' \dontrun{
#'   # return all possible decimal parts of ICD9 codes (111 in total)
#'   length(icd9:::icd_expand_minor(icd9(""), is_e = FALSE))
#'   icd9:::icd_expand_minor(icd9("1")) # "1"  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
#' }
#' @return NA for invalid minor, otherwise a vector of all possible (perhaps
#'   non-existent) sub-divisions.
#' @family ICD-9 ranges
#' @keywords internal manip

icd_expand_minor <- function(minor, ...) {
  UseMethod("icd_expand_minor")
}

icd_expand_minor.icd9 <- function(minor, is_e = FALSE) {
  # clang 3.6 with address sanitizer seems to fail if a number is passed instead
  # of string. It SHOULD fail with type error, and that might be an Rcpp
  # problem...
  assertString(minor) # or character vector?
  assertFlag(is_e)
  .Call("icd9_icd9ExpandMinorShim", PACKAGE = get_pkg_name(), minor, isE = is_e)
}

icd9_expand_minor.icd10 <- function(x) {
  stop("not implemented")
}
