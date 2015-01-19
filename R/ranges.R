

#' @title sort short-form icd9 codes
#' @description should work with numeric only, V or E codes. Note that a numeric
#'   sort does not work for ICD-9 codes, since "162" > "1620" TODO: write tests.
#'   TODO: reply with the actual items given (not trimmed, etc.) TODO: need to
#'   be able to compare a pair of codes quickly, then use built-in sort. This
#'   becomes easier when I move to S3 classes for ICD-9.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @return sorted vector of ICD-9 codes. Numeric, then E codes, then V codes.
#' @keywords manip
#' @export
icd9Sort <- function(icd9, isShort) {
  if (isShort) return(icd9SortShort(icd9))
  icd9SortDecimal(icd9)
}

#' @rdname icd9Sort
#' @export
icd9SortShort <- function(icd9Short)
  icd9Short[order(icd9AddLeadingZeroesShort(icd9Short))]

#' @rdname icd9Sort
#' @export
icd9SortDecimal <- function(icd9Decimal)
  icd9Decimal[order(icd9DecimalToShort(icd9Decimal))]

#' Generate sysdata.rda
#'
#' Generate correctly ordered look-up tables of numeric-only, V and E codes. This is
#' quick, but much to slow when it appears many times in a loop.
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
  icd9NShortReal <- grep("[^VE]*", icd9::icd9Hierarchy$icd9, value = TRUE)
  icd9VShortReal <- grep("V", icd9::icd9Hierarchy$icd9, value = TRUE)
  icd9EShortReal <- grep("E", icd9::icd9Hierarchy$icd9, value = TRUE)
  # we assume we are in the root of the package directory. Save to sysdata.rda
  # because these are probably not of interest to a user and would clutter an
  # already busy namespace.
  lknames <- c("icd9NShort", "icd9VShort", "icd9EShort",
               "icd9NShortReal", "icd9VShortReal", "icd9EShortReal");
  if (do.save) save(list = lknames,
       file = sysdata.path, compress = "xz")
  invisible(mget(lknames))
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
#'   As with \code{link{icd9ExpandRangeShort}} great care is taken
#'   not to include codes which have children not in the range. E.g. "100.9" to
#'   "101.1" would _not_ include code "101".
#' @examples
#' "4280 " %i9s% "4289 "
#' "V80 " %i9s% " V8210 "
#'
#' # the following should give all codes in 428 EXCEPT "428",
#' # and all codes upto 43014 EXCEPT 430 and 4301
#' icd9ExpandRangeShort("4280 ", "43014")
#' @templateVar icd9ShortName start,end
#' @template icd9-short
#' @template onlyReal
#' @template isShort
#' @import jwutil
#' @family ICD-9 ranges
#' @export
icd9ExpandRange <- function(start, end, isShort, onlyReal = FALSE) {
  if (isShort) return(icd9ExpandRangeShort(start, end, onlyReal))
  return(icd9ExpandRangeDecimal(start, end, onlyReal))
}

#' @rdname icd9ExpandRange
#' @export
icd9ExpandRangeShort <- function(start, end, onlyReal = FALSE, omitParents = FALSE) {
  start <- icd9AddLeadingZeroesShort(trim(start))
  end <- icd9AddLeadingZeroesShort(trim(end))

  worker <- function(s, e, lookup) {
    si <- match(s, lookup)
    if (is.na(si)) stop(sprintf(
      "start value '%s' not found in look-up table of ICD-9 codes.", s))
    # now if end is not a full-length code (5 for V or N, 4 for E), we keep
    # going until we get to the last code before the same hierarchical level is
    # reached. E.g. 101 to 102 should include all of 102 subcodes, but not 103.
    # See the tests.
    ei <- match(e, lookup)
    if (is.na(ei)) stop(sprintf(
      "end value '%s' not found in look-up table of ICD-9 codes.", e))
    if (ei < si) stop("end code must be greater than or equal to start code")
    if (nchar(e) != 5) {
      nlk = nchar(lookup[ei + 1 : ei + 112])
      ei <- ei + match(nchar(e), nlk) -
        (nchar(e) == 4 && substr(e, 4, 4) == "9") +
        icd9IsASingleE(e)
    }
    # except if 'end' is 4 char and ends in 9, because we don't want to catch
    # the preceding 3 digit.

    out <- lookup[si:ei]
    # finally, drop any higher-level codes which would describe broader ranges
    # than specified. E.g. 1019 to 1021 should omit 102, but 1059 to 1079 should
    # include 106.
    if (omitParents) stop("not implemented")
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
icd9ExpandRangeMajor <- function(start, end) {
  stopifnot(length(start) == 1 && length(end) == 1)
  c <- icd9ExtractAlphaNumeric(start)
  d <- icd9ExtractAlphaNumeric(end)
  # cannot range between numeric, V and E codes, so ensure same type.
  stopifnot(toupper(c[1]) == toupper(d[1]))
  if (icd9IsV(start)) fmt <- "%02d" else fmt <- "%03d"
  paste(c[,1], sprintf(fmt = fmt, c[,2]:d[,2]), sep  = "")
}

#' @rdname icd9ExpandRange
#' @export
icd9ExpandRangeDecimal <- function(start, end) {
  icd9ShortToDecimal(
    icd9ExpandRangeShort(
      icd9DecimalToShort(start), icd9DecimalToShort(end)
    )
  )
}

#' @rdname icd9ExpandRange
#' @export
"%i9d%" <- function(start, end) {
  icd9ExpandRangeDecimal(start, end)
}

#' @rdname icd9ExpandRange
#' @export
"%i9mj%" <- function(start, end) {
  icd9ExpandRangeMajor(start = start, end = end)
}

#' @rdname icd9ExpandRange
#' @export
"%i9s%" <- function(start, end) {
  icd9ExpandRangeShort(start, end)
}
