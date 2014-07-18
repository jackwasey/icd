#' @title generate all child codes for given decimal ICD9 codes
#' @description take ICD9 codes in decimal form and lists of all possible
#'   sub-classification codes: e.g. 1.1 returns 1.11, 1.12, 1.13 etc. There are
#'   no codes like 1.10 which are distinct from 1.1, so this can be purely
#'   numeric Also, note that expanding "100.0" (100.00 to 100.09) is different
#'   from expanding "100.00" (no expansion) \code{0.1 == .3/3} is a problem...
#'   \url{http://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f}
#'
#'
#' @template icd9-decimal
#' @template invalid
#' @examples
#' #icd9ChildrenDecimal("100.1")
#' #icd9ChildrenDecimal("2.34")
#' @return unsorted vector of ICD9 codes for all subsections of the provided
#'   code.
#' @export
#' @family ICD-9 ranges
#' @keywords manip
icd9ChildrenDecimal <- function(icd9Decimal, invalidAction = icd9InvalidActions) {

  if (!is.character(icd9Decimal)) stop('baseCode must be character only to avoid ambiguity')
  icd9Decimal <- icd9ValidNaWarnStopDecimal(icd9Decimal, invalidAction = invalidAction)

  parts <- icd9DecimalToParts(icd9Decimal, minorEmpty = "")
  out <- c()
  for (r in rownames(parts)) {
    out <- append(out,
                  paste(
                    parts[[r, "major"]],
                    icd9ExpandMinor(parts[[r, "minor"]], isE = icd9IsE(parts[[r, "major"]])),
                    sep = "."
                  )
    )
  }
  out
}

#' @title expand 5 character form 'short' ICD9 to all possible sub codes
#' @template icd9-short
#' @template invalid
#' @keywords manip
#' @family ICD-9 ranges
#' @export
icd9ChildrenShort <- function(icd9Short, invalidAction = icd9InvalidActions) {
  if (!is.character(icd9Short)) stop('must have character only input to expand a short basecode to avoid ambiguity')
  if (length(icd9Short) == 0) return(character())
  icd9Short <- icd9ValidNaWarnStopShort(icd9Short, invalidAction = match.arg(invalidAction))

  parts <- icd9ShortToParts(icd9Short, minorEmpty = "")
  out <- c()
  for (r in 1:nrow(parts)) {
    out <- c(out,
             icd9MajMinToShort(
               major = parts[[r, "major"]],
               minor = icd9ExpandMinor(parts[[r, "minor"]], isE = icd9IsE(parts[[r, "major"]])),
               invalidAction = "ignore")
    )
  }
  unique(out)
}

# icd9ChildrenShortFast <- function(icd9Short, invalidAction = icd9InvalidActions) {
#   if (!is.character(icd9Short)) stop('must have character only input to expand a short basecode to avoid ambiguity')
#
#   partsList <- icd9ShortToPartsNV(icd9Short) # ignoring E codes for now
#   unlist(
#     lapply(
#       partsList,
#       function(x) {
#         icd9PartsToShort(x[[1]], icd9ExpandMinor(x[[2]]))
#       }
#     )
#   )
# }

#' @rdname icd9ChildrenShort
#' @template icd9-any
#' @template isShort
#' @family ICD-9 ranges
#' @keywords internal
icd9Children <- function(icd9, isShort) {
  if (isShort) return(icd9ChildrenShort(icd9))
  icd9ChildrenDecimal(icd9)
}

#' @title List all child ICD-9 codes for a range definition
#' @description This is primarily useful for converting the icd9Chapters to
#'   ranges of children. E.g. "280-289" becomes "280" %i9s% "289". In this case,
#'   isShort determines the output, not the input.
#' @param range character vector length one e.g. "V10-V15"
#' @template isShort
icd9ChildrenRange <- function(range, isShort) {
  x <- unlist(strMultiMatch("([VvEe[:digit:]]*)-([VvEe[:digit:]]*)", range))
  if (isShort) {
    return(x[1] %i9s% x[2])
  } else {
    return(x[1] %i9d% x[2])
  }
}

icd9ChildrenRangeDecimal <- function(range) {
  icd9ChildrenRange(range, isShort = FALSE)
}

icd9ChildrenRangeShort <- function(range) {
  icd9ChildrenRange(range, isShort = TRUE)
}

#' @title sort short-form icd9 codes
#' @description should work with numeric only, V or E codes. Note that a numeric
#'   sort does not work for ICD-9 codes, since "162" > "1620" TODO: write tests.
#'   TODO: reply with the actual items given (not trimmed, etc.) TODO: need to
#'   be able to compare a pair of codes quickly, then use built-in sort. This
#'   becomes easier when I move to S3 classes for ICD-9.
#' @template icd9-short
#' @template invalid
#' @return sorted vector of ICD-9 codes
#' @keywords manip
#' @export
icd9SortShort <- function(icd9Short, invalidAction = icd9InvalidActions) {
  icd9Short <- icd9ValidNaWarnStopShort(icd9Short, invalidAction = invalidAction)

  tmp <- strsplit(icd9Short, "") # split into characters
  # convert to matrix and pad out to five characters, starting from the left
  xmatrix <- do.call(rbind, lapply(tmp, '[', 1:5))
  # then order by column, starting from the left:
  xmatrix <- xmatrix[order(xmatrix[,1], xmatrix[,2], xmatrix[,3], xmatrix[,4], xmatrix[,5], na.last = FALSE),]
  # and piece it togehter again, replacing NA with ""
  apply(xmatrix, MARGIN = 1,
        function(x) { x[is.na(x)] <- ""; paste(x, collapse = "") }
  )
}

#' @title take two ICD-9 codes and expand range to include all child codes
#' @description this is cumbersome code, covering a whole load of edge cases
#'   relating to the fact that icd9 codes are \strong{not} in numeric order. An
#'   alternative strategy would be to list all the ICD9 codes, then a range
#'   would just pick out start and finish positions, and return subset of the
#'   list. Not all ICD-9 codes are valid, including some parent codes which have
#'   valid children. However, I expect at least some of these have been used in
#'   some billing databases.
#' @examples
#' "4280 " %i9s% "4289 "
#' "V80 " %i9s% " V8210 "
#'
#' # the following should give all codes in 428 EXCEPT "428",
#' # and all codes upto 43014 EXCEPT 430 and 4301
#' icd9ExpandRangeShort("4280 ", "43014")
#' @templateVar icd9ShortName start,end
#' @template icd9-short
#' @param inferParents single logical value, if TRUE, will infer and include a
#'   parent code if a range ends with a value which terminates a higher-level
#'   code. E.g. "043" %i9s% "0449" . "0449" expands out to 04499, and thus all
#'   the codes from 0440 to 04499, are covered, but "044" is not explicit. If
#'   \code{inferParents} is TRUE, "044" would be added, otherwise omitted.
#' @template invalid
#' @export
#' @family ICD-9 ranges
icd9ExpandRangeShort <- function(start, end, inferParents = TRUE, invalidAction = icd9InvalidActions) {
  invalidAction <- match.arg(invalidAction)
  # minimal quick validation checks
  stopifnot(is.character(start), is.character(end))
  stopifnot(length(start) == 1, length(end) == 1)
  stopifnot(is.logical(inferParents), length(inferParents) == 1)

  isE = icd9IsE(start)

  start <- icd9ValidNaWarnStopShort(start, invalidAction = invalidAction)
  end <- icd9ValidNaWarnStopShort(end, invalidAction = invalidAction)

  start <- trim(start)
  end <- trim(end)
  # TODO: this is not a very elegant or complete test, and done more
  # comprehensively a few lines later.
  if (nchar(start) == nchar(end) && start > end) stop("start is after end")

  sdf <- icd9ShortToParts(start)
  edf <- icd9ShortToParts(end)

  startMajor <- icd9AddLeadingZeroesMajor(sdf[["major"]]) # zero pad to tolerate entering "1" instead of "001"
  endMajor <- icd9AddLeadingZeroesMajor(edf[["major"]])
  startMinor <- sdf[["minor"]]
  endMinor <- edf[["minor"]]

  # use icd9ExtractAlphaNumeric to get just the numbers (ignore V or E by taking
  # the second element returned), and compare those:
  startMajorInt <- as.integer(icd9ExtractAlphaNumeric(startMajor)[, 2])
  endMajorInt <- as.integer(icd9ExtractAlphaNumeric(endMajor)[, 2])
  if (endMajorInt < startMajorInt)  {
    stop("start is after end")
  } else if (startMajorInt == endMajorInt && icd9SortShort(c(start, end)) != c(start, end)) {
    stop("start is after end in minor part")
  }

  #if (!all(icd9SortShort(c(startMajor, endMajor)) == c(startMajor, endMajor)))
  #stop("start is after end (major part)")

  # If start and end majors are the same, we just need to calculate the minor range.
  if (startMajor == endMajor) {
    # the following initial \code{result} works when minors have same length,
    # but incomplete for start minor shorter.
    result <- icd9MajMinToShort(major = startMajor,
                                minor = intersect(
                                  icd9SubsequentMinors(startMinor, isE = isE),
                                  icd9PrecedingMinors(endMinor, isE = isE)
                                )
    )


    # case where startMinor lengths are 0,0 1,1 or 2,2: no corner cases
    if (nchar(startMinor) == nchar(endMinor)) return(icd9AddLeadingZeroesShort(result))

    # startMinor length is one, but endMinor could be zero or two chars.
    if (nchar(startMinor) == 1) return(unique(icd9AddLeadingZeroesShort(c(start, result))))
    if (nchar(startMinor) == 0) {
      # cover edge case where minor is "0x" so "0" is not included, and yet
      # should be if the start minor is ""
      if (substr(endMinor,1,1) == "0") result <- c(icd9MajMinToShort(startMajor, "0"), result)
      return(icd9AddLeadingZeroesShort(
        c( # zero pad the major for consistency.
          icd9MajMinToShort(startMajor, startMinor),
          result
        )
      ))
    } # end nchar(startMinor) == 0
    # if here, then startMinor was 2 characters long
  } else { # startMajor != endMajor
    result = c() # vector of complete short codes

    # corner case: the end minor terminates with a '9', so we can infer the parent:
    if (inferParents) {
      # do this twice, because we care about XXX99 and XXX9
      if (substr(endMinor, length(endMinor), length(endMinor)) == "9") endMinor <- icd9ParentMinor(endMinor)
      if (substr(endMinor, length(endMinor), length(endMinor)) == "9") endMinor <- icd9ParentMinor(endMinor)
    }

    for (major in startMajor %i9mj% endMajor) {
      #if no minor spec for current major, then expand all
      if ((major %nin% c(startMajor,endMajor)) # current major is not first or last, so expand all children
          || (major == startMajor & startMinor == "") # starting major has no minor, so expand all children
          || (major == endMajor & endMinor == "")) { #   ending major has no minor, so expand all children
        result <- c(result, icd9ChildrenShort(as.character(major), invalidAction = "ignore"))
      } else { # loop minors from start minor, or until end minor
        # at this point we definitely have a minor code, whether for start or end
        # (but not both at same time)
        # if minor is a start minor, then get subsequent minors
        if (major == startMajor) {
          result <- c(result,
                      icd9MajMinToShort(
                        major = major,
                        minor = icd9SubsequentMinors(startMinor, isE)
                      )
          )
        } else if (major == edf[["major"]]) { # otherwise get preceeding minors
          result <- c(result,
                      icd9MajMinToShort(
                        major = major,
                        minor = icd9PrecedingMinors(endMinor, isE)
                      )
          )
        }
      } # end if - whether to expand all children
    } #end loop through majors
  } # end startMajor != endMajor
  icd9AddLeadingZeroesShort(result, addZeroV = TRUE, invalidAction = "ignore")
}

#' @title create range of icd9 major parts
#' @description accepts V, E or numeric codes. Does not validate codes beyond
#'   ensuring that the start and end of the range are of the same type. Will add
#'   leading zeroes when appropriate. User can strip them out with
#'   icd9DropLeadingZeroes if they wish.
#' @templateVar icd9AnyName start,end
#' @template icd9-any
#' @template invalid
#' @return character vector with range inclusive of start and end
#' @family ICD-9 ranges
#' @export
icd9ExpandRangeMajor <- function(start, end, invalidAction = icd9InvalidActions) {
  invalidAction <- match.arg(invalidAction)
  start <- icd9ValidNaWarnStopMajor(start, invalidAction = invalidAction)
  end <- icd9ValidNaWarnStopMajor(end, invalidAction = invalidAction)
  stopifnot(length(start) == 1 && length(end) == 1)
  c <- icd9ExtractAlphaNumeric(start)
  d <- icd9ExtractAlphaNumeric(end)
  # cannot range between numeric, V and E codes, so ensure same type.
  stopifnot(toupper(c[1]) == toupper(d[1]))
  if (icd9IsV(start)) fmt <- "%02d" else fmt <- "%03d"
  paste(c[,1], sprintf(fmt = fmt, c[,2]:d[,2]), sep  = "")
}

#' @rdname icd9ExpandRangeMajor
#' @export
"%i9mj%" <- function(start, end) {
  icd9ExpandRangeMajor(start = start, end = end, invalidAction = "warn")
}

#' @rdname icd9ExpandRangeShort
#' @export
"%i9s%" <- function(start, end) {
  icd9ExpandRangeShort(start = start, end = end, invalidAction = "warn")
}

#' @title expand range of ICD-9 decimal codes to all possible intermediate and
#'   sub-codes
#' @description As with \code{link{icd9ExpandRangeShort}} great care is taken
#'   not to include codes which have children not in the range. E.g. "100.9" to
#'   "101.1" would _not_ include code "101".
#' @template invalid
#' @keywords internal manip
#' @family ICD-9 ranges
icd9ExpandRangeDecimal <- function(start, end, invalidAction = c("stop", "ignore", "silent", "warn")) {

  invalidAction <- match.arg(invalidAction)
  # invalidAction may only need to be specified in the innermost call here,
  # however, this function is internal and reserved for expanding the contents
  # of co-morbidity mappings, which is a one-off event. Therefore, extra
  # validation after each transformation is helpful, if a little slower. For
  # this reason, \code{stop} is moved to the default position in
  # \code{invalidAction}
  icd9ShortToDecimal(
    icd9ExpandRangeShort(
      icd9DecimalToShort(start, invalidAction = invalidAction),
      icd9DecimalToShort(end, invalidAction = invalidAction),
      invalidAction = invalidAction
    ),
    invalidAction = invalidAction
  )
}

#' @rdname icd9ExpandRangeDecimal
#' @export
"%i9d%" <- function(start, end) {
  icd9ExpandRangeDecimal(start = start, end = end, invalidAction = "warn")
}

#' @title condense list of short ICD-9 code into minimal set of parent codes
#' @description This can be thought of as the inverse operation to expanding a
#'   range. The list given must already contain the parents, because this
#'   function will never add a parent ICD-9 which, although may have all
#'   children present, may itself have an additional clinical meaning.
#' @template icd9-short
#' @template invalid
#' @family ICD-9 ranges
#' @export
icd9CondenseShort <- function(icd9Short, invalidAction = c("stop", "ignore", "silent", "warn")) {

  invalidAction <- match.arg(invalidAction)
  icd9Short <- icd9ValidNaWarnStopShort(icd9Short, invalidAction = invalidAction)

  # make homogeneous
  icd9Short <- sort(icd9AddLeadingZeroesShort(icd9Short)) # sort so we will hit the parents first, kids later.
  out <- icd9Short

  # for every entry, search for all possible child entries in the list, and if we find ALL the children, then delete them from the output list.
  for (i in icd9Short) {
    kids <- icd9ChildrenShort(i)
    if (all(kids %in% out)) {
      out <- c(i, out[!out %in% kids]) # keep self!
    }
  }
  out
}

#' @title condense list of short ICD-9 code into minimal set of parent codes
#'   which have descriptions.
#' @description This can be thought of as the inverse operation to expanding a
#'   range. The list given must already contain the parents, because this
#'   function will never add a parent ICD-9 which, although may have all
#'   children present, may itself have an additional clinical meaning. In
#'   addition, in contrast to \code{icd9CondenseShort}, this function only walks
#'   back up to parents which have descriptions in \code{icd9CmDesc}, so it is
#'   useful for generating a minimal textual description of a set of ICD-9
#'   codes.
#' @template icd9-short
#' @template invalid
#' @family ICD-9 ranges
#' @export
icd9CondenseToExplainShort <- function(icd9Short, invalidAction = c("stop", "ignore", "silent", "warn")) {
  icd9Short <- icd9ValidNaWarnStopShort(icd9Short, invalidAction = match.arg(invalidAction))
  # make homogeneous
  out <- icd9Short <- sort(icd9AddLeadingZeroesShort(icd9Short)) # sort so we will hit the parents first, kids later.
  # for every entry, search for all possible child entries in the list, and if we find ALL the children, then delete them from the output list.
  for (i in icd9Short) {
    kids <- icd9ChildrenShort(i)
    if (i %in% icd9CmDesc[["icd9"]] && all(kids %in% out)) {
      out <- c(i, out[!out %in% kids]) # keep self!
    }
  }
  out
}

#' @title determine preceding or subsequent post-decimal parts of ICD9 codes
#' @description this is not simply numeric, since "4" is after "39" and "0" !=
#'   "00". Frustrating to have to do so much string manipulation, but, as I have
#'   learned, it is not possible to treat ICD-9 codes as numbers without risking
#'   ambiguity and subtle mistakes.
#' @template minor
#' @family ICD-9 ranges
#' @keywords internal manip
icd9SubsequentMinors <- function(minor, isE) {

  stopifnot(!isE | nchar(minor) < 2)

  # these validations are done downstream
  #if (!is.character(minor)) stop("must have character input for minor")
  #if (nchar(minor) > 2) stop("minor provided with length > 2")

  # if no minor, then provide all 111 minor codes. (Noting again that there are
  # 111 codes between each integer ICD-9 top level code.)
  if (nchar(minor) == 0) return(icd9ExpandMinor(isE = isE))

  # simple case where minor is a single character, so we can legitimately include all child codes
  if (nchar(minor) == 1) {
    if (isE) return(as.character(seq(as.integer(minor), 9)))
    return(unlist(lapply(as.character(seq(as.integer(minor),9)), icd9ExpandMinorNV)))
  }


  # now working purely with two-digit minor parts
  minorBig <- as.integer(substr(minor,1,1)) # this is the first digit after the decimal

  # treat 0x as an edge cases 0x .... 09, then 1 to 99.
  if (minorBig == 0) {
    return(c(
      appendZeroToNine("0")[(as.integer(minor)+1):10],
      as.character(seq(1,99))))
  }

  # now we have minors from 10 to 99 to handle.
  minorSmalls <- as.character(seq(as.integer(minor),99)) # faulty for "0x" minor codes.

  if (minorBig == "9") return(minorSmalls)
  minorBigs <- unlist(lapply(as.character(seq(minorBig + 1, 9)), icd9ExpandMinor, isE))
  unique(c(minorBigs, minorSmalls))
}

#' @rdname icd9SubsequentMinors
icd9PrecedingMinors <- function(minor, isE) {

  stopifnot(!isE | nchar(minor) < 2)

  if (nchar(minor) == 0) return(icd9ExpandMinor(isE = isE))

  # take care of single digit minor codes.
  if (nchar(minor) == 1) {
    if (minor == "0") return(minor)
    return(unlist(lapply(as.character(seq(0, as.integer(minor))), icd9ExpandMinor, isE)))
  }

  minorBig <- as.integer(substr(minor,1,1))
  minorSmall <- as.integer(substr(minor,2,2))
  if (minorBig == 0) return(paste("0", seq(0, minorSmall),sep  = "")) #fill out 00 to 0x
  # remaining possibilities are between 10 and 99, two digits
  minorSmalls <- c(
    icd9ExpandMinor("0", isE = FALSE),
    as.character(seq(10, as.integer(minor)))
  )

  if (minorBig == 1) return(minorSmalls)
  minorBigs <- unlist(lapply(as.character(seq(0, minorBig-1)), icd9ExpandMinor, isE = FALSE))

  unique(c(minorBigs, minorSmalls))
}

#' @title expand decimal part of ICD-9 code to cover all possible sub-codes
#' @description Accepts a single number or character input starting point for
#'   generation of all possible decimal parts of ICD9 code. e.g. giving an empty
#'   input will fill out 111 combinations, e..g .1 .11 .12 .... .2 ....
#'   #examples #icd9ExpandMinor(isE = FALSE) # return all possible decimal parts of ICD9
#'   codes icd9ExpandMinor(1) # "1"  "10" "11" "12" "13" "14" "15" "16" "17"
#'   "18" "19" icd9ExpandMinor("1") # same
#' @template minor
#' @param isE single logical, which if TRUE, treats the minor as part of an E
#'   code (which is one character), as opposed to a V or numeric-only code,
#'   which is two character. No default.
#' @return NA for invalid minor, otherwise a vector of all possible (perhaps
#'   non-existent) sub-divisions.
#' @family ICD-9 ranges
#' @keywords internal manip
icd9ExpandMinor <- function(minor = "", isE, invalidAction = icd9InvalidActions) {

  invalidAction <- match.arg(invalidAction)
  # this is an error, not just invalidity. Could easily allow multiple values,
  # but I would then have to return a list and post-process that, so I think
  # this keeps things simpler, but maybe slower.
  # TODO: why not allow multiple?
  if (length(minor) > 1) stop("icd9ExpandMinor received more than one code to expand")

  if (!is.character(minor))
    stop("icd9ExpandMinor expects character input only. Minor class is ", class(minor))

  if (invalidAction != "ignore") {
    valid <- allIsNumeric(minor) # single value, but this takes care of characters, factors
    if (!valid) icd9WarnStopMessage("minor validation failed: non numeric: ", minor, invalidAction = invalidAction)
    if (!valid) minor = NA # again, limiting ourselves to single values
  }

  if (isE) return(icd9ExpandMinorE(minor = minor))
  icd9ExpandMinorNV(minor = minor)
}

#' @rdname icd9ExpandMinor
icd9ExpandMinorNV <- function(minor = "") {

  stopifnot(is.character(minor))

  # minor should be 0-2 character, digits only
  if (nchar(minor) > 2)
    stop("icd9ExpandMinor: starting length already too long! minor is: ", minor)

  # iterate through minors to generate all possible child codes.
  while (max(nchar(minor)) < 2) {
    newStrings <- appendZeroToNine(minor)
    minor <- unique(append(minor, newStrings)) # and add the new ones of any length
  }
  minor
}

#' @rdname icd9ExpandMinor
icd9ExpandMinorE <- function(minor = "") {
  if (nchar(minor) == 0) return(c("", as.character(seq(0,9))))
  if (nchar(minor) == 1) return(minor)
  if (nchar(minor) > 1) stop("icd9ExpandMinorE: starting length already too long! minor is: ", minor)
  stop("other invalid E minor condition for minor = ", minor)
}

#' @title append zero to nine
#' @description appends the characters "0" to "9" to elements of character
#'   vector Used for extrapolating all the possible ICD9 codes in order to
#'   match. The alternative approach is the way done in the 'comorbidities'
#'   package, but this has a load of messy seq() functions in the middle of the
#'   comorbid groups specification, and as far as I can tell, some errors
#'   arising from the non-numeric nature of ICD-9 codes. My way allows simple
#'   specification of, e.g. code "100" and all possible child codes are
#'   captured. #examples #appendZeroToNine("1") #appendZeroToNine(1:3)
#' @param str vector of numbers (or character representation of numbers)
#' @return vector of characters with 0 to 9 appended to each input value
#' @keywords internal
appendZeroToNine <- function(str) {
  if (!allIsNumeric(str)) stop("appendZeroToNine expects number input, or character input representing numbers")
  apply(expand.grid(str, as.character(0:9),""), 1, paste, collapse="")
}
