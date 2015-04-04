#' @title sort short-form icd9 codes
#' @description Sorts lists of numeric only, V or E codes. Note that a simple
#'   numeric sort does not work for ICD-9 codes, since "162" > "1620" TODO:
#'   write tests. TODO: reply with the actual items given (not trimmed, etc.)
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @return sorted vector of ICD-9 codes. Numeric, then E codes, then V codes.
#' @keywords manip
#' @export
icd9Sort <- function(icd9, isShort = icd9GuessIsShort(icd9)) {
  checkmate::assertFlag(isShort)
  # TODO: need to be able to compare a pair of codes quickly, then use built-in
  # sort. This becomes easier when I move to S3 classes for ICD-9.
  if (isShort) return(icd9SortShort(icd9))
  icd9SortDecimal(icd9)
}

#' @rdname icd9Sort
#' @export
icd9SortShort <- function(icd9Short) {
  assertFactorOrCharacter(icd9Short)
  icd9Short[order(icd9AddLeadingZeroesShort(icd9Short))]
}

#' @rdname icd9Sort
#' @export
icd9SortDecimal <- function(icd9Decimal) {
  assertFactorOrCharacter(icd9Decimal)
  icd9Decimal[order(icd9DecimalToShort(icd9Decimal))]
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
#' "V80 " %i9s% " V8210 "
#'
#' # the following should give all codes in 428 EXCEPT "428",
#' # and all codes upto 43014 EXCEPT 430 and 4301
#' icd9ExpandRangeShort("4280 ", "43014")
#' stopifnot(any(c("430", "4301") %nin% icd9ExpandRange("4280 ", "43014")))
#' @templateVar icd9ShortName start,end
#' @template icd9-short
#' @template onlyReal
#' @template isShort
#' @param excludeAmbiguousParent single logical value, if \code{TRUE} (the default) the range returned will not include
#' @family ICD-9 ranges
#' @export
icd9ExpandRange <- function(start, end, isShort = icd9GuessIsShort(c(start, end)),
                            onlyReal = TRUE, excludeAmbiguousParent = TRUE) {
  if (isShort) return(icd9ExpandRangeShort(start, end, onlyReal, excludeAmbiguousParent))
  icd9ExpandRangeDecimal(start, end, onlyReal, excludeAmbiguousParent)
}

#' @rdname icd9ExpandRange
#' @export
icd9ExpandRangeShort <- function(start, end, onlyReal = TRUE,
                                 excludeAmbiguousParent = TRUE) {
  checkmate::assertScalar(start) # i'll permit numeric but prefer char
  checkmate::assertScalar(end)
  checkmate::assertFlag(onlyReal)
  checkmate::assertFlag(excludeAmbiguousParent)
  start <- icd9AddLeadingZeroesShort(trim(start))
  end <- icd9AddLeadingZeroesShort(trim(end))

  worker <- function(s, e, lookup) {
    # s & e should be length 1, so match will only give a single value
    si <- match(s, lookup)
    checkmate::assertInteger(si, len = 1)
    if (is.na(si[1])) stop(sprintf(
      "start value '%s' not found in look-up table of ICD-9 codes.", s))
    # now if end is not a full-length code (5 for V or N, 4 for E), we keep
    # going until we get to the last code before the same hierarchical level is
    # reached. E.g. 101 to 102 should include all of 102 subcodes, but not 103.
    # See the tests.
    ei <- match(e, lookup)
    checkmate::assertInteger(ei, len = 1)
    if (is.na(ei[1])) stop(sprintf(
      "end value '%s' not found in look-up table of ICD-9 codes.", e))
    if (ei < si) stop("end code must be greater than or equal to start code")
    if (nchar(e) != 5) {
      # calculate the number of codes to span, start with lookup table of nchar of each
      nlk <- nchar(lookup[seq(from = ei + 1, to = ei + 111)])
      lene <- nchar(e)
      # lookup the next code with the same hierarchical level
      mlen <- match(lene, nlk)
      # if the next code can't be found, e.g. after 999, just pick a big number.
      if (is.na(mlen)) mlen <- 111
      ei <- ei + mlen
      ei <- ei - (lene - 2)
      if (icd9IsE(e))
        ei <- ei + 1
      else if (icd9IsV(e) && lene < nchar(s) && substr(e, lene, lene) != "9")
        ei <- ei + 1
    }
    # except if 'end' is 4 char and ends in 9, because we don't want to catch
    # the preceding 3 digit.

    out <- lookup[si:ei]
    # finally, drop any higher-level codes which would describe broader ranges
    # than specified. E.g. 1019 to 1021 should omit 102, but 1059 to 1079 should
    # include 106. # github issue #14
    if (excludeAmbiguousParent) {
      tmp <- lookup[si:ei]
      # drop start and end range arguments because we know we will accept those and their children
      startkids <- icd9ChildrenShort(start, onlyReal = onlyReal)
      endkids <- icd9ChildrenShort(end, onlyReal = onlyReal)
      for (code in tmp) {
        # this now does a lot of tests, slowing things down significantly.
        if (!all(icd9ChildrenShort(code, onlyReal = onlyReal) %in% tmp))
          out <- out[-which(out == code)]
      }
    }
    out
  }

  # potentially do some checks on start and end, e.g. start <= end.
  # determine whether we are doing N, V or E
  # then lookup start and end indices in sysdata.rda lookup tables

  if (icd9IsN(start) && icd9IsN(end))
    res <- worker(start, end, icd9NShort)
  else if (icd9IsV(start) && icd9IsV(end))
    res <- worker(start, end, icd9VShort)
  else if (icd9IsE(start) && icd9IsE(end))
    res <- worker(start, end, icd9EShort)
  else
    stop("mismatch between numeric, V and E types in start and end")

  if (onlyReal)
    return(icd9GetRealShort(res))

  res
}

#' @rdname icd9ExpandRange
#' @export
icd9ExpandRangeMajor <- function(start, end, onlyReal = TRUE) {
  checkmate::assertScalar(start) # i'll permit numeric but prefer char
  checkmate::assertScalar(end)
  checkmate::assertFlag(onlyReal)
  c <- icd9ExtractAlphaNumeric(start)
  d <- icd9ExtractAlphaNumeric(end)
  # cannot range between numeric, V and E codes, so ensure same type.
  stopifnot(toupper(c[1]) == toupper(d[1]))
  if (icd9IsV(start)) fmt <- "%02d" else fmt <- "%03d"
  majors <- paste(c[,1], sprintf(fmt = fmt, c[,2]:d[,2]), sep  = "")
  if (onlyReal) return(icd9GetRealShort(majors, majorOk = TRUE))
  majors
}

#' @rdname icd9ExpandRange
#' @export
icd9ExpandRangeDecimal <- function(start, end, onlyReal = TRUE, excludeAmbiguousParent = TRUE) {
  icd9ShortToDecimal(
    icd9ExpandRangeShort(
      icd9DecimalToShort(start), icd9DecimalToShort(end),
      onlyReal = onlyReal
    ) # although still considering allowing numeric
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
icd9Children <- function(icd9, isShort = icd9GuessIsShort(icd9), onlyReal = TRUE) {
  assertFactorOrCharacter(icd9)
  checkmate::assertFlag(isShort)
  checkmate::assertFlag(onlyReal)
  .Call("icd9_icd9ChildrenCpp", PACKAGE = "icd9", icd9, isShort, onlyReal)
}

#' @rdname icd9Children
#' @name icd9Children
#' @export
icd9ChildrenShort <- function(icd9Short, onlyReal = TRUE) {
  .Call("icd9_icd9ChildrenShortCpp", PACKAGE = "icd9", icd9Short, onlyReal)
}

#' @rdname icd9Children
#' @export
icd9ChildrenDecimal <- function(icd9Decimal, onlyReal = TRUE) {
  .Call("icd9_icd9ChildrenDecimalCpp", PACKAGE = "icd9", icd9Decimal, onlyReal)
}

#' Generate sysdata.rda
#'
#' Generate correctly ordered look-up tables of numeric-only, V and E codes. This is
#' quick, but much too slow when it appears many times in a loop.
#' @keywords internal
icd9GenerateSysData <- function(sysdata.path = file.path("R", "sysdata.rda"), do.save = TRUE) {
  c() -> icd9NShort -> icd9VShort -> icd9EShort
  for ( i in 1:999)
    icd9NShort <- c(icd9NShort, sort(icd9ChildrenShort(i, onlyReal = FALSE)))
  for ( i in 0:99)
    icd9VShort <- c(icd9VShort, sort(icd9ChildrenShort(paste("V", i, sep = ""), onlyReal = FALSE)))
  for ( i in 0:999)
    icd9EShort <- c(icd9EShort, sort(icd9ChildrenShort(paste("E", i, sep = ""), onlyReal = FALSE)))

  # we can either use the icd9IsReal functions on these lists, or just grep the
  # canonical list directly to get the numeric, V and E codes.
  icd9NShortReal <- grep("^[^VE]+", icd9::icd9Hierarchy$icd9, value = TRUE) # nolint
  icd9VShortReal <- grep("V", icd9::icd9Hierarchy$icd9, value = TRUE) # nolint
  icd9EShortReal <- grep("E", icd9::icd9Hierarchy$icd9, value = TRUE) # nolint

  # some very quick sanity checks: (duplicate in a test in test-ranges.R)
  stopifnot(length(icd9NShortReal) < length(icd9NShort))
  stopifnot(length(icd9VShortReal) < length(icd9VShort))
  stopifnot(length(icd9EShortReal) < length(icd9EShort))
  stopifnot(all(icd9NShortReal %in% icd9NShort))
  stopifnot(all(icd9VShortReal %in% icd9VShort))
  stopifnot(all(icd9EShortReal %in% icd9EShort))

  # we assume we are in the root of the package directory. Save to sysdata.rda
  # because these are probably not of interest to a user and would clutter an
  # already busy namespace.
  lknames <- c("icd9NShort", "icd9VShort", "icd9EShort",
               "icd9NShortReal", "icd9VShortReal", "icd9EShortReal");
  if (do.save) save(list = lknames,
                    file = sysdata.path, compress = "xz")
  invisible(mget(lknames))
}
