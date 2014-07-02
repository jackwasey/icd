#' @title generate all child codes for given decimal ICD9 codes
#' @description take ICD9 codes in decimal form and lists of all possible
#'   sub-classification codes: e.g. 1.1 returns 1.11, 1.12, 1.13 etc. There are
#'   no codes like 1.10 which are distinct from 1.1, so this can be purely
#'   numeric Also, note that expanding "100.0" (100.00 to 100.09) is different
#'   from expanding "100.00" (no expansion) \code{0.1 == .3/3} is a problem...
#'   \url{http://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f}
#'
#' @template icd9-decimal
#' @template invalid
#' @examples
#' #icd9ChildrenDecimal("100.1")
#' #icd9ChildrenDecimal("2.34")
#' @return unsorted vector of ICD9 codes for all subsections of the provided
#'   code.
#' @keywords internal manip
icd9ChildrenDecimal <- function(icd9Decimal, invalidAction = c("ignore", "silent", "warn", "stop")) {

  if (!is.character(icd9Decimal)) stop('baseCode must be character only to avoid ambiguity')
  icd9Decimal <- icd9ValidNaWarnStopDecimal(icd9Decimal, invalidAction = invalidAction)

  #vOrE <- grepl(pattern="[VvEe]", x=baseCode)
  parts <- icd9ExtractPartsDecimal(icd9Decimal, minorEmpty = "")
  out <- c()
  for (r in rownames(parts)) {
    out <- append(out,
                  paste(
                    parts[[r, "major"]],
                    icd9ExpandMinor(parts[[r, "minor"]]), sep="."
                  )
    )
  }
  out
}

#' @title expand 5 character form 'short' ICD9 to all possible sub codes
#' @description this is so the raw info from SAS code provided by AHRQ can be
#'   interpreted without manually reformatting.
#' @template icd9-short
#' @template invalid
#' @template short
#' @keywords internal
icd9ChildrenShort <- function(icd9Short, invalidAction = c("ignore", "silent", "warn", "stop")) {
  if (!is.character(icd9Short)) stop('must have character only input to expand a short basecode to avoid ambiguity')
  icd9Short <- icd9ValidNaWarnStopShort(icd9Short, invalidAction = invalidAction)

  # split into major and minor parts
  parts <- icd9ExtractPartsShort(icd9Short, minorEmpty = "")
  out <- c()
  for (r in 1:nrow(parts)) {
    out <- c(out, icd9PartsToShort(parts[[r, "major"]], icd9ExpandMinor(parts[[r, "minor"]])))
  }
  unique(out)
}

# icd9ChildrenShortFast <- function(icd9Short, invalidAction = c("ignore", "silent", "warn", "stop")) {
#   if (!is.character(icd9Short)) stop('must have character only input to expand a short basecode to avoid ambiguity')
#
#   partsList <- icd9ExtractPartsShortNV(icd9Short) # ignoring E codes for now
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
#' @template short
#' @keywords internal
icd9Children <- function(icd9, short) {
  if (short) return(icd9ChildrenShort(icd9))
  icd9ChildrenDecimal(icd9)
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
#' @export
icd9SortShort <- function(icd9Short, invalidAction = c("ignore", "silent", "warn", "stop")) {
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

# TODO: make this suffix N?
#icd9GenerateShort <- function() icd9ChildrenShort(as.character(1:999))
# TODO: this is not quite right...
#icd9GenerateShortV <- function() paste("V", icd9ChildrenShort(as.character(1:99)),sep  = "")
# TODO: this is not quite right, since E99 is invalid.
#icd9GenerateShortE <- function() sub("0", "", paste("E", icd9ChildrenShort(as.character(80:99)),sep  = ""))

#' @title take SAS icd9 format for character range and expand to a list of codes
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
#' @template invalid
#' @export
#'
icd9ExpandRangeShort <- function(start, end, invalidAction = c("ignore", "silent", "warn", "stop")) {
  # minimal quick validation checks
  stopifnot(is.character(start), is.character(end))
  stopifnot(length(start) == 1, length(end) == 1)
  stopifnot(all(grepl(pattern = "^[^E]*$", c(start, end)))) # cannot handle E ranges yet (this is not a quick test with regex... make 'fixed' TODO)

  start <- icd9ValidNaWarnStopShort(start, invalidAction = invalidAction)
  end <- icd9ValidNaWarnStopShort(end, invalidAction = invalidAction)

  start <- strip(start)
  end <- strip(end)
  if (nchar(start) == nchar(end) && start>end) stop("start is after end time")
  sdf <- icd9ExtractPartsShort(start)
  edf <- icd9ExtractPartsShort(end)

  # should have single digit minors for E codes.

  startMajor <- icd9AddLeadingZeroesMajor(sdf[["major"]]) # zero pad to tolerate entering "1" instead of "001"

  endMajor <- icd9AddLeadingZeroesMajor(edf[["major"]])
  startMinor <- sdf[["minor"]]
  endMinor <- edf[["minor"]]

  # use icd9ExtractAlphaNumeric to get just the numbers (ignore V or E by taking
  # the second element returned), and compare those:
  startMajorInt <- as.integer(icd9ExtractAlphaNumeric(startMajor)[,2])
  endMajorInt <- as.integer(icd9ExtractAlphaNumeric(endMajor)[,2])
  if (endMajorInt < startMajorInt)  {
    stop("start is after end")
  } else if (startMajorInt == endMajorInt && icd9SortShort(c(start, end)) != c(start, end)) {
    stop("start is after end in minor part")
  }

  #if (!all(icd9SortShort(c(startMajor, endMajor)) == c(startMajor, endMajor)))
  #stop("start is after end (major part)")

  # deal with special case where start major = start minor
  if (startMajor == endMajor) {
    #if (startMinor > endMinor) # not obvious, because "1" is greater than "02"
    #  stop("majors match and end minor has broader scope than start minor. startMinor=",
    #       startMinor, " and endMinor=", endMinor)
    # the following result works when minors have same length, but incomplete for start minor shorter.
    result <- icd9PartsToShort(startMajor,
                               intersect(
                                 icd9SubsequentMinors(startMinor),
                                 icd9PrecedingMinors(endMinor)
                               )
    )

    # case where startMinor lengths are 0,0 1,1 or 2,2: no corner cases
    if (nchar(startMinor) == nchar(endMinor)) return(result)

    # startMinor length is one, but endMinor could be zero or two chars.
    if (nchar(startMinor) == 1) return(unique(c(start, result)))
    if (nchar(startMinor) == 0) {
      # cover edge case where minor is "0x" so "0" is not included, and yet
      # should be if the start minor is ""
      if (substr(endMinor,1,1) == "0") result <- c(icd9PartsToShort(startMajor, "0"), result)
      return(c(icd9PartsToShort(startMajor, startMinor), result))
    }
    return(result) # i.e. startMinor was 2 characters long.
  }

  out = c() # vector of complete short codes
  for (major in startMajor %i9mj% endMajor) {
    #if no minor spec for current major, then expand all
    if ((major %nin% c(startMajor,endMajor)) # current major is not first or last, so expand all children
        || (major == startMajor & startMinor == "") # starting major has no minor, so expand all children
        || (major == endMajor & endMinor == "") #   ending major has no minor, so expand all children
    ) {
      out <- c(out, icd9ChildrenShort(as.character(major)))
    } else { # loop minors from start minor, or until end minor

      # at this point we definitely have a minor code, whether for start or end
      # (but not both at same time)

      # if minor is a start minor, then get subsequent minors
      if (major == startMajor) {
        out <- c(out, icd9PartsToShort(major, icd9SubsequentMinors(startMinor)))
      } else if (major == edf[["major"]]) { # otherwise get preceeding minors
        out <- c(out, icd9PartsToShort(major, icd9PrecedingMinors(endMinor)))
      }
    }
  }
  out
}

#' @title find range of ICD-9 codes between two E-codes
#' @description E codes only have a single possible numeral after the decimal
#'   place. This makes processing very different from V and pure numeric codes.
#' @inheritParams icd9ExpandRangeShort

#'

#' @title create range of icd9 major parts
#' @description accepts V, E or numeric codes. Does not validate codes beyond
#'   ensuring that the start and end of the range are of the same type.
#' @templateVar icd9AnyName start,end
#' @template icd9-any
#' @return character vector with range inclusive of start and end
#' @family ICD-9
#' @family jack
#' @export
"%i9mj%" <- function(start, end) {
  stopifnot(length(start) == 1 && length(end) == 1)
  c <- icd9ExtractAlphaNumeric(start)
  d <- icd9ExtractAlphaNumeric(end)
  stopifnot(toupper(c[1]) == toupper(d[1])) # cannot range between numeric, V and E codes, so ensure same type.
  paste(c[,1], c[,2]:d[,2],sep  = "")
}

#' @rdname icd9ExpandRangeShort
#' @export
"%i9s%" <- function(start, end) icd9ExpandRangeShort(start = start, end = end, invalidAction = "warn")

#' @title expand range of ICD-9 decimal codes to all possible intermediate and sub-codes
#' @description As with \code{link{icd9ExpandRangeShort}} great care is taken not to include codes which have children not in the range. E.g. "100.9" to "101.1" would _not_ include code "101".
#' @template invalid
#' @keywords internal manip
#' @family ICD-9
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
    invalidAction = invalidAction,
    keepLoneDecimal = FALSE,
    leadingZeroes = TRUE
  )
}

#' @rdname icd9ExpandRangeDecimal
#' @export
"%i9d%" <- function(start, end) icd9ExpandRangeDecimal(start = start, end = end)

#' @title condense list of short ICD-9 code into minimal set of parent codes
#' @description This can be thought of as the inverse operation to expanding a range. The list given must already contain the parents, because this function will never add a parent ICD-9 which, although may have all children present, may itself have an additional clinical meaning.
#' @param icd9Short character vector of ICD-9 codes in short form
#' @template invalid
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

#' @title extract alphabetic, and numeric part of icd9 code
#'   prefix
#' @description removes whitespace and separates V or E if present.
#' @template icd9-any
#' @return vector or matrix, with first item of each row (or whole vector)
#'   containing V, E or "". The second part contains the numeric parts of the
#'   code, which may include a decimal point.
#' @keywords internal manip
icd9ExtractAlphaNumeric <- function(icd9) {
  icd9c <- as.character(icd9) # so we can accept numbers, if we must...
  iList <- regmatches(
    x = icd9c,
    m = regexec(
      pattern="([VvEe]?)([[:digit:].]+)",
      text = icd9c
    )
  )
  # flip the list into a matrix with a row for each code, and the alpha part in
  # first column, and numeric part in the second
  t(
    vapply(
      iList,
      function(x) matrix(data = x[2:3], nrow = 1, ncol = 2),
      FUN.VALUE = rep(NA_character_, times = 2)
    )
  )
}

#' @title determine preceding or subsequent post-decimal parts of ICD9 codes
#' @description this is not simply numeric, since "4" is after "39" and "0" !=
#'   "00". Frustrating to have to do so much string manipulation, but, as I have
#'   learned, it is not possible to treat ICD-9 codes as numbers without risking
#'   ambiguity and subtle mistakes.
#' @template minor
#' @keywords internal manip
icd9SubsequentMinors <- function(minor) {

  # these validations are done downstream
  #if (!is.character(minor)) stop("must have character input for minor")
  #if (nchar(minor) > 2) stop("minor provided with length > 2")

  # if no minor, then provide all 111 minor codes. (Noting again that there are
  # 111 codes between each integer ICD-9 top level code.)
  if (nchar(minor) == 0) return(icd9ExpandMinor())

  # simple case where minor is a single character, so we can legitimately include all child codes
  if (nchar(minor) == 1)
    return(unlist(lapply(as.character(seq(as.integer(minor),9)), icd9ExpandMinor)))

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
  minorBigs <- unlist(lapply(as.character(seq(minorBig + 1, 9)), icd9ExpandMinor))
  unique(c(minorBigs, minorSmalls))
}

#' @rdname icd9SubsequentMinors
icd9PrecedingMinors <- function(minor, invalidAction = c("ignore", "silent", "warn", "stop")) {

  if (nchar(minor)==0) return(icd9ExpandMinor())

  # take care of single digit minor codes.
  if (nchar(minor)==1) {
    if (minor == "0") return(minor)
    return(unlist(lapply(as.character(seq(0, as.integer(minor))), icd9ExpandMinor)))
  }

  minorBig <- as.integer(substr(minor,1,1))
  minorSmall <- as.integer(substr(minor,2,2))
  if (minorBig == 0) return(paste("0", seq(0, minorSmall),sep  = "")) #fill out 00 to 0x
  # remaining possibilities are between 10 and 99, two digits
  minorSmalls <- c(
    icd9ExpandMinor("0"),
    as.character(seq(10,as.integer(minor)))
  )

  if (minorBig == 1) return(minorSmalls)
  minorBigs <- unlist(lapply(as.character(seq(0, minorBig-1)), icd9ExpandMinor))

  unique(c(minorBigs, minorSmalls))
}

#' @title expand decimal part of ICD-9 code to cover all possible sub-codes
#' @description Accepts a single number or character input starting point for
#'   generation of all possible decimal parts of ICD9 code. e.g. giving an empty
#'   input will fill out 111 combinations, e..g .1 .11 .12 .... .2 ....
#' #examples #icd9ExpandMinor() # return all possible decimal parts of ICD9 codes
#' icd9ExpandMinor(1) # "1"  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
#' icd9ExpandMinor("1") # same
#' @template minor
#' @return NA for invalid minor, otherwise a vector of all possible (perhaps
#'   non-existent) sub-divisions.
#' @keywords internal manip
icd9ExpandMinor <- function(minor = "", invalidAction = c("ignore", "silent", "warn", "stop")) {

  invalidAction <- match.arg(invalidAction)
  # this is an error, not just invalidity. Could easily allow multiple values,
  # but I would then have to return a list and post-process that, so I think
  # this keeps things simpler, but maybe slower.
  if (length(minor) > 1) stop("icd9ExpandMinor received more than one code to expand") # TODO: why not allow multiple?

  if (!is.character(minor))
    stop("icd9ExpandMinor expects character input only. Minor class is ", class(minor))

  if (invalidAction != "ignore") {
    valid <- allIsNumeric(minor) # single value, but this takes care of characters, factors
    if (!valid) icd9WarnStopMessage("minor validation failed: non numeric: ", minor, invalidAction = invalidAction)
    if (!valid) minor = NA # again, limiting ourselves to single values
  }
  # TODO: switch here if E code, and call icd9ExpandMinorE
  return(icd9ExpandMinorNV(minor = minor))
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
  stop("not supported yet. Would require a load of changes to other parts of code
       which have hard-coded minor lengths of 2.")
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

#' @title convert between icd9 decimal and short formats
#' @description converted decimal ICD9 code, e.g. 123.45 to 'short' e.g. 12345
#'   non-decimal format
#' @template icd9-decimal
#' @template leadingZeroes
#' @template invalid
#' @return character vector of converted ICD-9 codes
#' @keywords manip
icd9DecimalToShort <- function(icd9Decimal,
                               leadingZeroes = TRUE,
                               invalidAction = c("ignore", "silent", "warn", "stop")) {

  if (!is.character(icd9Decimal)) stop("icd9DecimalToShort must be given character string, not a numeric type")

  icd9Decimal <- icd9ValidNaWarnStopDecimal(icd9Decimal, invalidAction)

  # todo: this defeats the purpose of optionally skipping validation...
  icd9Decimal[!icd9ValidDecimal(icd9Decimal)] <- NA

  x <- icd9ExtractPartsDecimal(icd9Decimal, leadingZeroes = TRUE) # returns everything zero-padded. Good default behaviour.
  x[is.na(x[["minor"]]), "minor"] <- "" # NA to ""
  # skipping zero padding only when asked, and only when minor is empty, otherwise it would simply give the wrong code.
  if (!leadingZeroes && any(x[["minor"]] == ""))
    x[x[["minor"]] == "", "major"] <- icd9DropZeroFromDecimal(x[x[["minor"]] == "", "major"])
  y <- paste(x$major, x$minor,sep  = "")
  y[is.na(x[["major"]])] <- NA
  y
}

#' @title icd9ShortToDecimal
#' @description converts ICD-9 'short' form to decimal form
#' @template icd9-short
#' @template leadingZeroes
#' @param keepLoneDecimal logical. Since the presence or absence of a trailing
#'   zero in an ICD-9 code can make a difference, when converting to decimal, we
#'   have a situation where we do not want to add trailing zeroes, but may wish
#'   to preserve the decimal point. If this is desired, then set this value to
#'   TRUE.
#' @template invalid
#' @export
#' @keywords manip
icd9ShortToDecimal <- function(icd9Short, leadingZeroes = FALSE, keepLoneDecimal = FALSE,
                               invalidAction = c("ignore", "silent", "warn", "stop")) {

  invalidAction <- match.arg(invalidAction)

  # prevalidate regardless of invalidAction - TODO: do this for every public entry point.
  if (class(icd9Short) != "character") stop("icd9Short must be a character: number values could be ambiguous if converted blindly to character")

  icd9Short <- icd9ValidNaWarnStopShort(icd9Short, invalidAction)

  # short icd9 codes are always zero-padded, since there is no other way to
  # unambiguously represent codes<100. Therefore, leadingZeroes = FALSE has to
  # strip the extra zeros.
  parts <- icd9ExtractPartsShort(icd9Short)

  out <- paste( parts[["major"]], ".", parts[["minor"]], sep = "") # should only be max of 6 chars...
  if (!keepLoneDecimal && (any(parts[["minor"]] == "") || is.na(parts[["minor"]])))
    out[parts[["minor"]] == "" | is.na(parts[["minor"]])] <- parts[parts[["minor"]] == "" | is.na(parts[["minor"]]), "major"]

  # paste inevitably makes <NA> into a string "NA", so if major was NA, then restore that:
  out[is.na(parts[["major"]])] <- NA_character_

  # todo: belt and braces re-validation in this call to icd9DropZeroFromDecimal
  # ? We have already validated, but done some transformations. Validation is
  # expected to be done routinely on one-time steps, but only intially on
  # repeated or very slow steps.
  if (!leadingZeroes) return (icd9DropZeroFromDecimal(out))
  out
}

#' @title extract major and minor parts of a decimal ICD-9 code
#' @description accepts Vxxxx Exxxx or xxxxx
#' @template icd9-short
#' @param minorEmpty vector of length one, to be used in place of
#'   minor part of zero. Defaults to ""
#' @param invalid
#' @return data.frame with two columns. At least the minor part must be
#'   character, because "03" is different to "3", but "30" is the same as "3" at
#'   least in ICD-9 if leadingZeroes is true, then the major part must also be
#'   character.
#' @keywords internal manip
icd9ExtractPartsShort <- function(icd9Short, minorEmpty = "", invalidAction = c("ignore", "silent", "warn", "stop")) {

  icd9Short <- icd9ValidNaWarnStopShort(icd9Short, invalidAction)

  eCodes <- grepl(pattern = "E", x = icd9Short, fixed = TRUE, useBytes = TRUE) # assume bytes not unicode, for speed

  icd9Short <- strip(icd9Short)
  x <- data.frame(
    major = substr(icd9Short[!eCodes], 0, 3),
    minor = substr(icd9Short[!eCodes], 4, 5),
    stringsAsFactors = FALSE)
  x[eCodes, "major"] <- substr(icd9Short[eCodes], 0, 4)
  x[eCodes, "minor"] <- substr(icd9Short[eCodes], 5, 5)

  if (minorEmpty != "")
    x[!is.na(x[["minor"]]) & x[["minor"]] == "", "minor"] <- minorEmpty

  x
}

## benchmark is slower than handling a data frame.
# icd9ExtractPartsShortList <- function(icd9Short, minorEmpty = "") {
#   eCodes <- grepl(pattern = "E", x = icd9Short, fixed = TRUE, useBytes = TRUE) # bytes not unicode...
#   out <- icd9ExtractPartsShortNV(icd9Short)
#   out[eCodes] <- icd9ExtractPartsShortE(icd9Short[eCodes])
#   out
# }

# icd9ExtractPartsShortSlow <- function(icd9Short, minorEmpty = "") {
#
#   x <- data.frame(
#     major = substr(, 0, 3),
#     minor = substr(trim(icd9Short), 4, 5),  # probably breaks for E codes
#     stringsAsFactors = FALSE
#   )
#   # this is not efficient!
#   xe <- data.frame(
#     major = substr(trim(icd9Short), 0, 4),
#     minor = substr(trim(icd9Short), 5, 5),  # probably breaks for E codes
#     stringsAsFactors=F
#   )
#
#   eCodes <- grepl(pattern = "E", icd9Short)
#   x[eCodes] <- xe[eCodes]
#   x[!is.na(x[["minor"]]) & x[["minor"]] == "", "minor"] <- minorEmpty  # equivalent to =="0"
#   x
# }

icd9ExtractPartsShortNV <- function(icd9Short) {
  vapply(X = icd9Short,
         FUN.VALUE = c("", ""),
         FUN = function(x) { c(substr(x, 0, 3), substr(x, 4, 5)) }
  )
}

icd9ExtractPartsShortE <- function(icd9Short) {
  vapply(X = icd9Short,
         FUN.VALUE = c("", ""),
         FUN = function(x) { c(substr(x, 0, 4), substr(x, 5, 5)) })
}

#' @title icd9ExtractPartsDecimal
#' @template icd9-decimal
#' @template leadingZeroes
#' @param minorEmpty vector of length one, to be used in place of
#'   minor part of zero. Defaults to ""
#' @template invalid
#' @keywords internal manip
icd9ExtractPartsDecimal <- function(icd9Decimal, leadingZeroes = TRUE, minorEmpty = "", invalidAction = c("ignore", "silent", "warn", "stop")) {
  invalidAction <- match.arg(invalidAction)
  icd9Decimal <- icd9ValidNaWarnStopDecimal(icd9Decimal, invalidAction)

  a <- strsplit(trim(icd9Decimal), "\\.")
  x <- as.data.frame(
    do.call(rbind, lapply(a, '[', 1:2)),
    stringsAsFactors = FALSE
  )  # this may be slow! (need to flip axes from list to data frame)
  names(x) <- c("major","minor")
  if (leadingZeroes) x[["major"]] <- icd9AddLeadingZeroesDecimal(x[["major"]])
  x[is.na(x[["minor"]]), "minor"] <- minorEmpty
  x
}

#' @title pad decimal ICD-9 codes with leading zeroes
#' @template icd9-decimal
#' @return character vector of icd9 codes
#' @keywords internal manip
icd9AddLeadingZeroesDecimal <- function(icd9Decimal, invalidAction = c("ignore", "silent", "warn", "stop")) {
  # zero pad the major part:
  parts <- icd9ExtractPartsDecimal(icd9Decimal, leadingZeroes = FALSE, invalidAction = invalidAction) # avoid infinite recursion by setting leadingZeroes to FALSE
  lzdf <- parts
  # now just pad the numeric-only icd9 major parts, and strip whitespace from V and E codes
  ivd <- icd9ValidDecimalN(icd9Decimal)
  lzdf[ivd, "major"] <- icd9AddLeadingZeroesMajor(parts[ivd, "major"])
  # TODO: strip whitespace from V & E?
  lzDecimal <- paste(lzdf$major, lzdf$minor, sep = ".")
  # get rid of NA errors, which sprintf insists on converting to "NA". Also "xxx" becomes "xxx.", so drop these, too
  lzDecimal[lzdf$minor == ""] <- lzdf[lzdf$minor == "", "major"]
  # if either part is 'NA', then return NA for that value
  lzDecimal[is.na(parts$major) | is.na(parts$minor)] <- NA
  lzDecimal
}

#' @title add leading zeroes to short-form ICD-9 code
#' @description Use with care. non-decimal ICD-9 codes with length<5 are often
#'   ambiguous. E.g. 100 could be 1.00 10.0 or 100
#' @template icd9-any
#' @keywords internal manip
icd9AddLeadingZeroesShort <- function(icd9Short, invalidAction = c("ignore", "silent", "warn", "stop")) {
  parts <- icd9ExtractPartsShort(icd9Short, invalidAction = invalidAction)
  parts[["major"]] <- icd9AddLeadingZeroesMajor(parts[["major"]])
  out <- icd9PartsToShort(parts = parts)
  #TODO Consider doing this by normal validation rules
  out[!icd9ValidShort(icd9Short)] <- NA # set NA for invalid inputs (may be done downstream?)
  out
}

#' @title drop zero padding from decimal ICD-9 code.
#' @description decimal form ICD-9 codes are not ambiguous if the leading zeroes
#'   are dropped. Some short-form ICD-9 codes would not be ambiguous, e.g. "1"
#'   but many would be problematic, so no function is provided to do this for
#'   short-form codes.
#' @template icd9-any
#' @template invalid
#' @return character vector of ICD-9 codes with extra zeroes dropped from major
#'   part
#' @keywords internal manip
icd9DropZeroFromDecimal <- function(icd9Decimal, invalidAction = c("ignore", "silent", "warn", "stop")) {

  icd9Decimal <- icd9ValidNaWarnStopDecimal(icd9Decimal, invalidAction = invalidAction)
  # may well fail here if not validating, but do what we're told
  vapply(
    X = strMultiMatch(
      pattern = "[[:space:]]*([VvEe]?)(0*)([\\.[:digit:]]+)[[:space:]]*",
      text = icd9Decimal),
    FUN = function(x) { if (length(x) > 0) paste(x[[1]], x[[3]], sep = "") else NA_character_ },
    FUN.VALUE = character(1) # template result PER vapply 'row'
  )
}

#' @title zero-pad major part of ICD9 code
#' @description three digit codes are returned unchanged, one and two digit
#'   codes are preceded by 00 or 0. V or E codes are trimmed for whitespace and
#'   returned without prefxing. NA values are generated where the code is
#'   non-numeric, and doesn't match a valid V or E code format.
#' @template major
#' @return character vector
#' @keywords internal manip
icd9AddLeadingZeroesMajor <- function(major, invalidAction = c("ignore", "silent", "warn", "stop")) {
  # major should be character or factor only, nchar between 1 and 4 (including E
  # code...) and no decimal point.

  invalidAction <- match.arg(invalidAction)
  if (invalidAction != "ignore") {
    #only do validation step if not in 'ignore' mode: faster this way
    valid <- icd9ValidMajor(major)
    if (invalidAction == "stop") stop("invalid major values: ", paste(major[!valid]))
    if (invalidAction == "warn") warning("invalid major values: ", paste(major[!valid]))
    major[!valid] <- NA
  }

  numMajor <- asNumericNoWarn(major) # make integers and characters into numeric (double) type
  # now quickly find V or E codes, without fully validating V or E codes again:
  vOrE <- grepl(pattern = "E", major, fixed = TRUE) | grepl(pattern = "e", major, fixed = TRUE) |
    grepl(pattern = "V", major, fixed = TRUE) | grepl(pattern = "v", major, fixed = TRUE)
  isIntMajor <- !vOrE & (numMajor %% 1 < 1e-6) # catch modulo rounding errors with some tolerance
  if (any(!vOrE & !isIntMajor, na.rm = TRUE)) stop("if numeric type is given, it must acutally be an integer, but I see a floating point")
  out <- rep(x = NA, times=length(major))
  # give NAs for V and E codes, which are unhelpfully converted to "NA"
  out[isIntMajor & !is.na(isIntMajor)] <- sprintf("%03d", numMajor[isIntMajor & !is.na(isIntMajor)])
  outve <- trim(major[vOrE])
  # if single digit V or E code, then slip in a zero.
  outve[nchar(outve) == 2] <- paste(substr(outve, 1, 1), "0", substr(outve, 2, 2),sep  = "")
  out[vOrE] <- outve
  out
}

#' @title recompose major and minor parts into icd9 codes
#' @aliases icd9PartsToShort icd9PartsToDecimal
#' @description internal function which checks vector lengths to avoid
#'   unintentional recycling of vectors when lengths differ. Length of one is
#'   fine for major or minor.
#' @template major
#' @template minor
#' @param parts data.frame with major and minor fields. This can be given
#'   instead of major and minor vectors
#' @template sep
#' @return character vector. Deliberately returns zero-padded major, because
#'   otherwise we are creating ambiguous codes (even if we know what we mean)
#' @keywords internal
icd9PartsRecompose <- function(major = NULL, minor = NULL, parts = NULL, sep) {
  if (!is.null(parts)) {
    stopifnot(is.null(major), is.null(minor)) # enforce parts OR major, minor
    return(paste(icd9AddLeadingZeroesMajor(parts[["major"]]), parts[["minor"]], sep = sep))
  }
  stopifnot(length(sep) == 1)
  if (length(major) != 1 && length(minor) != 1 && length(major) != length(minor))
    stop("icd9PartsRecompose requires major and minor vectors to be of the same length, or for one of them to be unit length")

  paste(icd9AddLeadingZeroesMajor(major), minor, sep = sep)
}

#' @rdname icd9PartsRecompose
#' @export
icd9PartsToShort <- function(major = NULL, minor = NULL, parts = NULL)
  icd9PartsRecompose(major = major, minor = minor, parts = parts, sep  = "")

#' @rdname icd9PartsRecompose
#' @export
icd9PartsToDecimal <- function(major = NULL, minor = NULL, parts = NULL)
  icd9PartsRecompose(major = major, minor = minor, parts = parts, sep = ".")

#' @title explain ICD9 codes
#' @description convert full format (123.45 style) ICD9 codes into the name and
#'   description for human review there are official ICD9-CM data tables, not
#'   with conversion to decimal notation, but to the textual format.
#' @note TODO: it will be useful to have S3 ICD-9 short and long classes for
#'   situations like this where we could easily dispatch on short or long type,
#'   or even use a print.icd9decimal or print.icd9short S3 method to display
#'   ICD-9 codes.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template short
#' @examples
#' icd9ExplainShort(ahrqComorbid[[1]][1:3])
#' @return data frame, or list of data frames, with fields for ICD9 code, name
#'   and description, derived from datamart lookup table
#' @seealso package comorbidities
#' @references \url{http://www.stata.com/help.cgi?icd9}
#' @export
icd9Explain <- function(icd9, short) UseMethod("icd9Explain")
#' @rdname icd9Explain
#' @export
icd9ExplainShort <- function(icd9Short) icd9Explain(icd9Short, short = TRUE)
#' @rdname icd9Explain
#' @export
icd9ExplainDecimal <- function(icd9Decimal) icd9Explain(icd9Decimal, short = FALSE)

#' @describeIn icd9Explain explain alll ICD-9 codes in a list of vectors
#' @export
icd9Explain.list <- function(icd9, short) lapply(icd9, icd9Explain, short = short)

#' @describeIn icd9Explain explain character vector of ICD-9 codes
#' @export
icd9Explain.character <- function(icd9, short) {

  if (!short) {
    # make sure there are preceding zeroes, in order to match the icd9CmDesc data.
    icd9 <- icd9AddLeadingZeroesDecimal(icd9)
    icd9 <- icd9DecimalToShort(icd9)
  }
  icd9 <- icd9CondenseShort(icd9, invalidAction = "warn")
  out <- icd9CmDesc[ icd9CmDesc[["icd9"]] %in% icd9, ]
  row.names(out) <- NULL
  names(out) <- c("ICD9", "Diagnosis", "Description")
  out
}

#' @describeIn icd9Explain explain numeric vector of ICD-9 codes, with warning
#' @export
icd9Explain.numeric <- function(icd9, short) {
  warning("Numeric ICD-9 codes are unable to accurately represent actual ICD-9 codes.
          Converting to character, but beware of inevitable errors.")
  icd9Explain.character(as.character(icd9), short = short)
}

#' @title read the ICD-9-CM description data as provided by the Center for
#'   Medicaid Services.
#' @description ICD9-CM data unfortunately has no comma separation, so have to
#'   pre-process. Note that this canonical data doesn't specify non-diagnostic
#'   higher-level codes, just the specific diagnostic 'child' codes.
#' @details ideally would get ICD9-CM data zip directly from CMS web page, and
#'   extract, but the built-in unzip only extracts the first file in a zip.
#' @param icd9path path of the source data which is in /extddata in the
#'   installed package, but would be in inst/extdata in development tree.
#' @param save logical whether to attempt to save output in package source tree
#'   data directory
#' @param path Absolute path in which to save parsed data
#' @return invisibly return the result
#' @export
parseIcd9Cm <- function(icd9path = system.file("extdata","CMS32_DESC_LONG_DX.txt",
                                               package = 'icd9'),
                        save = FALSE,
                        path = "~/icd9/data") {
  f <- file(icd9path, "r")
  r <- readLines(f, encoding="latin1")
  close(f)
  r <- strsplit(r, " ")
  icd9LongCode <- lapply(r, FUN=function(row) row[1])
  icd9LongDesc <- lapply(r, FUN=function(row) paste(row[-c(1,2)], collapse = " "))

  f <- file(system.file("extdata", "CMS32_DESC_SHORT_DX.txt", package='icd9'), "r")
  r <- readLines(f) # this is ascii
  close(f)
  r <- strsplit(r, " ")
  icd9ShortCode <- lapply(r, FUN=function(row) row[1])
  icd9ShortDesc <- lapply(r, FUN=function(row) paste(row[-c(1,2)], collapse = " "))
  icd9CmDesc <- data.frame(
    icd9 = unlist(icd9LongCode),
    descLong = unlist(icd9LongDesc),
    descShort = unlist(icd9ShortDesc),
    stringsAsFactors = FALSE)

  # attempt to write the date from the source file to RData in the package source tree.
  if (save) saveSourceTreeData("icd9CmDesc", path = path)

  message("The following long descriptions contain UTF-8 codes:")
  message(paste(icd9CmDesc[grep(pattern = "UTF", Encoding(icd9CmDesc$descLong)), ], sep = ", "))

  invisible(icd9CmDesc)
}

