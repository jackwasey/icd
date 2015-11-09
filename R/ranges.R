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

#' @title sort short-form icd9 codes
#' @description Sorts lists of numeric only, V or E codes. Note that a simple
#'   numeric sort does not work for ICD-9 codes, since "162" > "1620", and also
#'   V codes precede E codes.
#' @details Implementation used fast built-in sort, then shuffles the E codes to
#'   the end.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @return sorted vector of ICD-9 codes. Numeric, then E codes, then V codes.
#' @keywords manip
#' @export
icd9Sort <- function(icd9, isShort = icd9GuessIsShort(icd9)) {
  assertFlag(isShort)
  if (isShort) return(icd9SortShort(icd9))
  icd9SortDecimal(icd9)
}

#' @rdname icd9Sort
#' @export
icd9SortShort <- function(icd9Short) {
  assertFactorOrCharacter(icd9Short)
  icd9Short[sortOrderShort(icd9Short)]
}

#' @rdname icd9Sort
#' @export
icd9SortDecimal <- function(icd9Decimal)
  icd9Decimal[sortOrderShort(icd9DecimalToShort(icd9Decimal))]

sortOrderShort <- function(icd9Short) {
  x <- icd9Short[order(icd9AddLeadingZeroesShort(icd9Short))]
  match(
    x[c(which(icd9IsN(x)), which(icd9IsV(x)), which(icd9IsE(x)))],
    icd9Short)
}

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
icd9ExpandRange <- function(start, end, isShort = icd9GuessIsShort(c(start, end)), onlyReal = TRUE,
                            excludeAmbiguousStart = TRUE,
                            excludeAmbiguousEnd = TRUE) {
  if (isShort) return(icd9ExpandRangeShort(start, end, onlyReal,
                                           excludeAmbiguousStart,
                                           excludeAmbiguousEnd))
  icd9ExpandRangeDecimal(start, end, onlyReal,
                         excludeAmbiguousStart,
                         excludeAmbiguousEnd)
}

#' expand range worker function
#' @keywords internal
#' @importFrom utils head tail
expandRangeWorker <- function(start, end, lookup, onlyReal,
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
    starts <- tail(out, )
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
  icd9SortShort(unique(c(out, icd9ChildrenShort(end, onlyReal = onlyReal))))
}

# horrible kludge for difficult source data
icd9ExpandRangeForSas <- function(start, end) {
  if (end == "0449") end <- start # HIV codes changed
  reals <- icd9ExpandRangeShort(start, end, onlyReal = TRUE,
                                # hmmm, maybe get the diff and test all children of ambigs present later
                                excludeAmbiguousStart = FALSE,
                                excludeAmbiguousEnd = TRUE)
  real_parents <- icd9CondenseShort(reals, onlyReal = TRUE)
  merged <- unique(c(reals, real_parents))
  real_parents_of_merged <- icd9CondenseShort(merged, onlyReal = TRUE)
  halfway <- icd9ChildrenShort(real_parents_of_merged, onlyReal = FALSE)
  nonrealrange <- icd9ExpandRangeShort(start, end, onlyReal = FALSE,
                                       excludeAmbiguousStart = TRUE,
                                       excludeAmbiguousEnd = TRUE)
  icd9SortShort(unique(c(halfway, nonrealrange)))
}

#' @rdname icd9ExpandRange
#' @export
icd9ExpandRangeShort <- function(start, end, onlyReal = TRUE,
                                 excludeAmbiguousStart = TRUE,
                                 excludeAmbiguousEnd = TRUE) {
  assertScalar(start) # i'll permit numeric but prefer char
  assertScalar(end)
  assertFlag(onlyReal)
  assertFlag(excludeAmbiguousStart)
  assertFlag(excludeAmbiguousEnd)

  start <- icd9AddLeadingZeroesShort(trim(start))
  end <- icd9AddLeadingZeroesShort(trim(end))

  # potentially do some checks on start and end, e.g. start <= end.
  # determine whether we are doing N, V or E
  # then lookup start and end indices in sysdata.rda lookup tables

  if (onlyReal) {
    stopifnot(icd9IsRealShort(start), icd9IsRealShort(end))
    if (icd9IsN(start) && icd9IsN(end))
      res <- expandRangeWorker(start, end, icd9NShortReal, onlyReal = TRUE,
                               excludeAmbiguousStart, excludeAmbiguousEnd)
    else if (icd9IsV(start) && icd9IsV(end))
      res <- expandRangeWorker(start, end, icd9VShortReal, onlyReal = TRUE,
                               excludeAmbiguousStart, excludeAmbiguousEnd)
    else if (icd9IsE(start) && icd9IsE(end))
      res <- expandRangeWorker(start, end, icd9EShortReal, onlyReal = TRUE,
                               excludeAmbiguousStart, excludeAmbiguousEnd)
    else
      stop("mismatch between numeric, V and E types in start and end")
  } else {

    if (icd9IsN(start) && icd9IsN(end))
      res <- expandRangeWorker(start, end, icd9NShort, onlyReal = FALSE,
                               excludeAmbiguousStart, excludeAmbiguousEnd)
    else if (icd9IsV(start) && icd9IsV(end))
      res <- expandRangeWorker(start, end, icd9VShort, onlyReal = FALSE,
                               excludeAmbiguousStart, excludeAmbiguousEnd)
    else if (icd9IsE(start) && icd9IsE(end))
      res <- expandRangeWorker(start, end, icd9EShort, onlyReal = FALSE,
                               excludeAmbiguousStart, excludeAmbiguousEnd)
    else
      stop("mismatch between numeric, V and E types in start and end")
  }
  res
}

#' @rdname icd9ExpandRange
#' @export
icd9ExpandRangeMajor <- function(start, end, onlyReal = TRUE) {
  assertScalar(start) # i'll permit numeric but prefer char
  assertScalar(end)
  assertFlag(onlyReal)
  c <- icd9ExtractAlphaNumeric(start)
  d <- icd9ExtractAlphaNumeric(end)
  # cannot range between numeric, V and E codes, so ensure same type.
  stopifnot(toupper(c[1]) == toupper(d[1]))
  if (icd9IsV(start)) fmt <- "%02d" else fmt <- "%03d"
  majors <- paste(c[,1], sprintf(fmt = fmt, c[,2]:d[,2]), sep  = "")
  if (onlyReal) return(icd9GetRealShort(majors, onlyBillable = FALSE))
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

# icd9Children separate from C++ docs so that I can guess isShort
#' @name icd9Children
#' @title Expand ICD-9 codes to all possible sub-codes
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @template onlyReal
#' @template onlyBillable
#' @keywords manip
#' @family ICD-9 ranges
#' @examples
#' library(magrittr)
#' icd9ChildrenShort("10201", FALSE) # no children other than self
#' icd9Children("0032", FALSE) # guess it was a short, not decimal code
#' icd9ChildrenShort("10201", TRUE) # empty because 102.01 is not meaningful
#' icd9ChildrenShort("003", TRUE) %>% icd9ExplainShort(doCondense = FALSE)
#' icd9ChildrenDecimal("100.0")
#' icd9ChildrenDecimal("100.00")
#' icd9ChildrenDecimal("2.34")
#' @export
icd9Children <- function(icd9, isShort = icd9GuessIsShort(icd9),
                         onlyReal = TRUE, onlyBillable = FALSE) {
  assertFactorOrCharacter(icd9)
  assertFlag(isShort)
  assertFlag(onlyReal)
  res <- .Call("icd9_icd9ChildrenCpp", PACKAGE = "icd9", icd9, isShort, onlyReal)
  if (onlyBillable) return(icd9GetBillable(res, isShort))
  res
}

#' @rdname icd9Children
#' @name icd9Children
#' @export
icd9ChildrenShort <- function(icd9Short,
                              onlyReal = TRUE, onlyBillable = FALSE) {
  assertCharacter(icd9Short)
  assertFlag(onlyReal)
  res <- .Call("icd9_icd9ChildrenShortCpp", PACKAGE = "icd9", toupper(icd9Short), onlyReal)
  if (onlyBillable) return(icd9GetBillableShort(res))
  res
}

#' @rdname icd9Children
#' @export
icd9ChildrenDecimal <- function(icd9Decimal,
                                onlyReal = TRUE, onlyBillable = FALSE) {
  res <- .Call("icd9_icd9ChildrenDecimalCpp", PACKAGE = "icd9", icd9Decimal, onlyReal)
  if (onlyBillable) return(icd9GetBillableDecimal(res))
  res
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
#'   length(icd9:::icd9ExpandMinor("", isE = FALSE))
#'   icd9:::icd9ExpandMinor("1") # "1"  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
#' @return NA for invalid minor, otherwise a vector of all possible (perhaps
#'   non-existent) sub-divisions.
#' @family ICD-9 ranges
#' @keywords internal manip
icd9ExpandMinor <- function(minor, isE = FALSE) {
  # clang 3.6 with address sanitizer seems to fail if a number is passed instead
  # of string. It SHOULD fail with type error, and that might be an Rcpp
  # problem...
  assertString(minor)
  .Call("icd9_icd9ExpandMinorShim", PACKAGE = "icd9", minor, isE)
}
