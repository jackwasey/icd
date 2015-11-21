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
#' @template isShort
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
icd_expand_range <- function(start, end, ...)
  UseMethod("icd_expand_range")

#' expand range of ICD-10 codes returning only defined codes in ICD-10-CM
#'
#' This will need generalizing to any list of 'real' codes, e.g. WHO or other
#' @param start character vector of length one containing a real code
#' @param end  character vector of length one containing a real code
#' @keywords internal
icd_expand_range.icd10cm <- function(start, end, short = icd_guess_short.icd10(c(start, end)), real = TRUE) {
  assertScalar(start) # i'll permit numeric but prefer char
  assertScalar(end)

  if (!real)
    stop("expanding ranges of possible (versus real) ICD-10-CM codes is not yet implemented.
         It will produce a very large number of codes because of permutations.")

  # check whether valid?
  # check whether real?
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

  end_kids <- icd10_children_real_short(end)
  new_end <- end_kids[length(end_kids)]

  # find the start and end code positions in the master list
  pos <- match(c(start, new_end), icd9::icd10cm2016[["code"]])
  if (is.na(pos[1])) stop(sprintf("start code '%s' not found", pos[1]))
  if (is.na(pos[2])) stop(sprintf("calculated end code '%s' not found", pos[2]))
  stopifnot(pos[2] >= pos[1])

  icd9::icd10cm2016[pos[1]:pos[2], "code"]
}

# WIP
icd10ExpandRangePossibleShort <- function(start, end) {
  assertString(start)
  assertString(end)
  # check whether valid?
  # check whether real? Will error if not.

  start <- stringr::str_trim(start)
  end <- stringr::str_trim(end)

  start_char <- substr(start, 1, 1)
  end_char <- substr(end, 1, 1)
  stopifnot(start_char <= end_char)

  start_two_digits <- as.integer(substr(start, 2, 3))
  end_two_digits <- as.integer(substr(end, 2, 3))
  if (start_char == end_char)
    stopifnot(start_two_digits <= end_two_digits)

  start_other_chars <- substr(start, 4, 10)
  end_other_chars <- substr(end, 4, 10)

}

icd_expand_range_major <- function(start, end)
  UseMethod("icd_expand_range_major")

icd_expand_range_major.icd10 <- function(start, end) {
  assertString(start)
  assertString(end)

  start <- stringr::str_trim(start)
  end <- stringr::str_trim(end)


  stopifnot(icd10_is_major(start), icd10_is_major(end))
  stopifnot(start <= end)
  start_first <- stringr::str_sub(start, 1, 1) %>% stringr::str_to_upper()
  end_first <- stringr::str_sub(end, 1, 1) %>% stringr::str_to_upper()

  stop("not finished")
  if (start_first == end_first)
    paste

}

#' @rdname icd_expand_range
#' @export
icd9ExpandRange <- function(start, end,
                            isShort = icd_guess_short(c(start, end)),
                            onlyReal = TRUE,
                            excludeAmbiguousStart = TRUE,
                            excludeAmbiguousEnd = TRUE) {
  .Deprecated("icd_expand_range")
  icd_expand_range.icd9(start = start, end = end, short = isShort,
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
  icd_expand_range.icd9(start = start, end = end, short = TRUE,
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
  icd_expand_range.icd9(start = start, end = end, short = FALSE,
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

#' @describeIn icd_expand_range Expand a range of ICD-9 codes
icd_expand_range.icd9 <- function(start, end,
                                  short = icd_guess_short(c(start, end)),
                                  real = TRUE,
                                  excludeAmbiguousStart = TRUE,
                                  excludeAmbiguousEnd = TRUE) {
  if (short)
    icd9ExpandRangeShort(start, end, real,
                         excludeAmbiguousStart,
                         excludeAmbiguousEnd)
  else
    icd9ExpandRangeDecimal(start, end, real,
                           excludeAmbiguousStart,
                           excludeAmbiguousEnd)
}

#' expand range worker function
#' @keywords internal
#' @importFrom utils head tail
expand_range_worker <- function(start, end, lookup, onlyReal,
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

  if (start == end) return(icd9ChildrenShort(start, onlyReal = onlyReal))

  out <- lookup[start_index:end_index]
  if (excludeAmbiguousStart) {
    # just remove those codes at the beginning which have children not in the output
    # let's take the first 5, to cover cases like 100, 101, 102.1, 102.11, 102.2
    starts <- tail(out, 5)
    for (s in starts) {
      if (any(icd9ChildrenShort(s, onlyReal = onlyReal) %nin% out))
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
      if (any(icd9ChildrenShort(o, onlyReal = onlyReal) %nin% out))
        out <- out[-which(out == o)]
    }
  }
  icd_sort.icd9(unique(c(out, icd9ChildrenShort(end, onlyReal = onlyReal))), short = TRUE)
}

# Expand range of short-form ICD-9 codes
icd9_expand_range_short <- function(start, end, real = TRUE,
                                    excludeAmbiguousStart = TRUE,
                                    excludeAmbiguousEnd = TRUE) {
  assertScalar(start) # i'll permit numeric but prefer char
  assertScalar(end)
  assertFlag(real)
  assertFlag(excludeAmbiguousStart)
  assertFlag(excludeAmbiguousEnd)

  start <- icd9AddLeadingZeroesShort(trim(start))
  end <- icd9AddLeadingZeroesShort(trim(end))

  # potentially do some checks on start and end, e.g. start <= end.
  # determine whether we are doing N, V or E
  # then lookup start and end indices in sysdata.rda lookup tables

  if (real) {
    stopifnot(icd9IsRealShort(start), icd9IsRealShort(end))
    if (icd9IsN(start) && icd9IsN(end))
      res <- expand_range_worker(start, end, icd9::icd9NShortReal, onlyReal = TRUE,
                                 excludeAmbiguousStart, excludeAmbiguousEnd)
    else if (icd9IsV(start) && icd9IsV(end))
      res <- expand_range_worker(start, end, icd9::icd9VShortReal, onlyReal = TRUE,
                                 excludeAmbiguousStart, excludeAmbiguousEnd)
    else if (icd9IsE(start) && icd9IsE(end))
      res <- expand_range_worker(start, end, icd9::icd9EShortReal, onlyReal = TRUE,
                                 excludeAmbiguousStart, excludeAmbiguousEnd)
    else
      stop("mismatch between numeric, V and E types in start and end")
  } else {

    if (icd9IsN(start) && icd9IsN(end))
      res <- expand_range_worker(start, end, icd9::icd9NShort, onlyReal = FALSE,
                                 excludeAmbiguousStart, excludeAmbiguousEnd)
    else if (icd9IsV(start) && icd9IsV(end))
      res <- expand_range_worker(start, end, icd9::icd9VShort, onlyReal = FALSE,
                                 excludeAmbiguousStart, excludeAmbiguousEnd)
    else if (icd9IsE(start) && icd9IsE(end))
      res <- expand_range_worker(start, end, icd9::icd9EShort, onlyReal = FALSE,
                                 excludeAmbiguousStart, excludeAmbiguousEnd)
    else
      stop("mismatch between numeric, V and E types in start and end")
  }
  res
}

#' @describeIn icd_expand_range_major Expand a range of ICD-9 major codes into major codes
#' @export
icd_expand_range_major.icd9 <- function(start, end, real = TRUE) {
  assertScalar(start) # i'll permit numeric but prefer char
  assertScalar(end)
  assertFlag(real)
  c <- icd9ExtractAlphaNumeric(start)
  d <- icd9ExtractAlphaNumeric(end)
  # cannot range between numeric, V and E codes, so ensure same type.
  stopifnot(toupper(c[1]) == toupper(d[1]))
  if (icd9IsV(start)) fmt <- "%02d" else fmt <- "%03d"
  majors <- paste(c[,1], sprintf(fmt = fmt, c[,2]:d[,2]), sep  = "")
  if (real)
    icd9GetRealShort(majors, onlyBillable = FALSE)
  else
    majors
}

#' @rdname icd9ExpandRange
#' @export
icd9ExpandRangeDecimal <- function(start, end, onlyReal = TRUE,
                                   excludeAmbiguousStart = TRUE,
                                   excludeAmbiguousEnd = TRUE) {
  icd9ShortToDecimal(
    icd9ExpandRangeShort(
      icd9DecimalToShort(start), icd9DecimalToShort(end), onlyReal = onlyReal,
      excludeAmbiguousStart = excludeAmbiguousStart,
      excludeAmbiguousEnd = excludeAmbiguousEnd
    )
  )
}


#' @rdname icd9ExpandRange
#' @export
"%i9da%" <- function(start, end) {
  icd9ExpandRangeDecimal(start, end, onlyReal = FALSE)
}

#' @rdname icd9ExpandRange
#' @export
"%i9sa%" <- function(start, end) {
  icd9ExpandRangeShort(start, end, onlyReal = FALSE)
}

#' @rdname icd9ExpandRange
#' @export
"%i9d%" <- function(start, end) {
  icd9ExpandRangeDecimal(start, end, onlyReal = TRUE)
}

#' @rdname icd9ExpandRange
#' @export
"%i9mj%" <- function(start, end) {
  icd9ExpandRangeMajor(start, end, onlyReal = TRUE)
}

#' @rdname icd9ExpandRange
#' @export
"%i9s%" <- function(start, end) {
  icd9ExpandRangeShort(start, end, onlyReal = TRUE)
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
#'   # return all possible decimal parts of ICD9 codes (111 in total)
#'   length(icd9:::expand_minor("", isE = FALSE))
#'   icd9:::expand_minor("1") # "1"  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
#' @return NA for invalid minor, otherwise a vector of all possible (perhaps
#'   non-existent) sub-divisions.
#' @family ICD-9 ranges
#' @keywords internal manip
expand_minor <- function(minor, is_e = FALSE) {
  # clang 3.6 with address sanitizer seems to fail if a number is passed instead
  # of string. It SHOULD fail with type error, and that might be an Rcpp
  # problem...
  assertString(minor)
  assertFlag(is_e)
  .Call("icd9_expand_minorShim", PACKAGE = get_pkg_name(), minor, isE)
}
