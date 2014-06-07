#' @title generate all child codes for given decimal ICD9 codes
#' @description take ICD9 codes in decimal form and lists of all possible
#'   sub-classification codes: e.g. 1.1 returns 1.11, 1.12, 1.13 etc. There are
#'   no codes like 1.10 which are distinct from 1.1, so this can be purely
#'   numeric Also, note that expanding "100.0" (100.00 to 100.09) is different
#'   from expanding "100.00" (no expansion) \code{0.1 == .3/3} is a problem...
#'   \url{http://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f}
#'   
#' @template icd9-decimal
#' @template validate
#' @return unsorted vector of ICD9 codes for all subsections of the provided
#'   code.
icd9ExpandBaseCodeDecimal <- function(icd9Decimal, validate = F) {
  
  if (!is.character(icd9Decimal)) stop('baseCode must be character only to avoid ambiguity')
  
  if (validate) stopIfInvalidICD9(icd9Decimal, short = F)
  
  #vOrE <- grepl(pattern="[VvEe]", x=baseCode)
  parts <- icd9ExtractPartsDecimal(icd9Decimal, minorEmpty="")
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
#' @template validate
#' @template short
#' @keywords internal
icd9ExpandBaseCodeShort <- function(icd9Short, validate=F) {
  if (!is.character(icd9Short)) stop('must have character only input to expand a short basecode to avoid ambiguity')
  
  if (validate) stopIfInvalidICD9(icd9Short, short=T)
  
  # split into major and minor parts
  parts <- icd9ExtractPartsShort(icd9Short, minorEmpty="")
  out <- c()
  for (r in 1:nrow(parts)) {
    out <- c(out, icd9PartsToShort(parts[[r, "major"]], icd9ExpandMinor(parts[[r, "minor"]])))
  }
  out
}

#' @describeIn icd9ExpandBaseCodeShort
#' @template icd9-any
#' @template short
icd9ExpandBaseCode <- function(icd9, short) {
  if (short) return(icd9ExpandBaseCodeShort(icd9))
  icd9ExpandBaseCodeDecimal(icd9)
}

#' @title sort short-form icd9 codes
#' @description should work with numeric only, V or E codes. Note that a numeric
#'   sort does not work for ICD-9 codes, since "162" > "1620" TODO: write tests.
#'   TODO: reply with the actual items given (not trimmed, etc.) TODO: need to
#'   be able to compare a pair of codes quickly, then use built-in sort. This
#'   becomes easier when I move to S3 classes for ICD-9.
#' @template icd9-short
#' @return sorted vector of ICD-9 codes
#' @export
icd9SortShort <- function(icd9Short) {
  tmp <- strsplit(icd9Short, "") # split into characters
  # convert to matrix and pad out to five characters, starting from the left
  xmatrix <- do.call(rbind, lapply(tmp, '[', 1:5)) 
  # then order by column, starting from the left:
  xmatrix <- xmatrix[order(xmatrix[,1], xmatrix[,2], xmatrix[,3], xmatrix[,4], xmatrix[,5], na.last = FALSE),]
  # and piece it togehter again, replacing NA with ""
  apply(xmatrix, MARGIN=1, 
        function(x) { x[is.na(x)] <- ""; paste(x, collapse="") }
  )
}

# TODO: make this suffix N?
#icd9GenerateShort <- function() icd9ExpandBaseCodeShort(as.character(1:999))
# TODO: this is not quite right...
#icd9GenerateShortV <- function() paste("V", icd9ExpandBaseCodeShort(as.character(1:99)), sep="")
# TODO: this is not quite right, since E99 is invalid. 
#icd9GenerateShortE <- function() sub("0", "", paste("E", icd9ExpandBaseCodeShort(as.character(80:99)), sep=""))

#' @title take SAS icd9 format for character range and expand to a list of codes
#' @description this is cumbersome code, covering a whole load of edge 
#'   cases relating to the fact that icd9 codes are \strong{not} in numeric order. An 
#'   alternative strategy would be to list all the ICD9 codes, then a range 
#'   would just pick out start and finish positions, and return subset of the 
#'   list. Not all ICD-9 codes are valid, including some parent codes which have
#'   valid children. However, I expect at least some of these have been used in
#'   some billing databases.
#' @examples
#' "4280 " %icd9% "4289 "
#' "V80 " %icd9% " V8210 "
#' icd9ExpandRangeShort("4280 ", "43014") # should give all codes in 428 EXCEPT "428", and all codes upto 43014 EXCEPT 430 and 4301
#' @templateVar icd9ShortName start,end
#' @template icd9-short
#' @export
icd9ExpandRangeShort <- function(start, end) {
  stopifnot(is.character(start), is.character(end))
  stopifnot(length(start)==1 && length(end)==1)
  start <- trim(start)
  end <- trim(end)
  if (nchar(start) == nchar(end) && start>end) stop("start is after end time")
  sdf <- icd9ExtractPartsShort(start)
  edf <- icd9ExtractPartsShort(end)
  startMajor <- icd9leadingZeroesMajor(sdf[["major"]]) # zero pad to tolerate entering "1" instead of "001"
  endMajor <- icd9leadingZeroesMajor(edf[["major"]]) 
  startMinor <- sdf[["minor"]]
  endMinor <- edf[["minor"]]
  
  # use icd9ExtractAlphaNumeric to get just the numbers (ignore V or E), and compare those:
  majorNumsOnly <- as.integer(icd9ExtractAlphaNumeric(c(startMajor, endMajor))[,2])
  if (!all(icd9SortShort(c(start, end)) == trim(c(start, end)))) 
    stop("start is after end")
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
      out <- c(out, icd9ExpandBaseCodeShort(as.character(major)))
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

#' @title create range of icd9 major parts
#' @description accepts V, E or numeric codes. Does not validate codes beyond
#'   ensuring that the start and end of the range are of the same type.
#' @templateVar icd9AnyName start,end
#' @template icd9-any
#' @return character vector with range inclusive of start and end
#' @export
"%i9mj%" <- function(start, end) {
  stopifnot(length(start)==1 && length(end)==1)
  c <- icd9ExtractAlphaNumeric(start)
  d <- icd9ExtractAlphaNumeric(end)
  stopifnot(toupper(c[1]) == toupper(d[1])) # cannot range between numeric, V and E codes, so ensure same type.
  paste(c[,1], c[,2]:d[,2], sep="")
}

#' @describeIn icd9ExpandRangeShort
#' @export
"%icd9%" <- function(start, end) icd9ExpandRangeShort(start=start, end=end)

#' @title extract alphabetic, and numeric part of icd9 code
#'   prefix
#' @description removes whitespace and separates V or E if present.
#' @template icd9-any
#' @return vector or matrix, with first item of each row (or whole vector) 
#'   containing V, E or "". The second part contains the numeric parts of the 
#'   code, which may include a decimal point.
#' @export
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
      function(x) matrix(data=x[2:3], nrow=1, ncol=2),
      FUN.VALUE=rep(NA_character_, times=2)
    )
  )
}

#' @title determine subsequent post-decimal parts of ICD9 codes
#' @description this is not simply numeric, since "4" is after "39" and "0" !=
#'   "00". Frustrating to have to do so much string manipulation, but, as I have
#'   learned, it is not possible to treat ICD-9 codes as numbers without risking
#'   ambiguity and subtle mistakes.
#' @template minor
#' @export
icd9SubsequentMinors <- function(minor) {
  
  if (!is.character(minor)) stop("must have character input for minor")
  if (nchar(minor)>2) stop("minor provided with length > 2")
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

#' @describeIn icd9SubsequentMinors
icd9PrecedingMinors <- function(minor) {
  
  if (!is.character(minor)) stop("must have character input for minor")
  if (nchar(minor)>2) stop("minor provided with length > 2")
  if (nchar(minor)==0) return(icd9ExpandMinor())
  
  # take care of single digit minor codes.
  if (nchar(minor)==1) {
    if (minor=="0") return(minor)
    return(unlist(lapply(as.character(seq(0, as.integer(minor))), icd9ExpandMinor)))
  }
  
  minorBig <- as.integer(substr(minor,1,1))
  minorSmall <- as.integer(substr(minor,2,2))
  if (minorBig == 0) return(paste("0", seq(0, minorSmall), sep="")) #fill out 00 to 0x
  # remaining possibilities are between 10 and 99, two digits
  minorSmalls <- c(
    icd9ExpandMinor("0"),
    as.character(seq(10,as.integer(minor)))
  )
  
  if (minorBig == 1) return(minorSmalls)
  minorBigs <- unlist(lapply(as.character(seq(0, minorBig-1)), icd9ExpandMinor))
  
  unique(c(minorBigs, minorSmalls))
}

#' @title expand decimal part of ICD9 code to cover all possible sub-codes
#' @description Accepts a single number or character input starting point for
#'   generation of all possible decimal parts of ICD9 code. e.g. giving an empty
#'   input will fill out 111 combinations, e..g .1 .11 .12 .... .2 ....
#' #examples #icd9ExpandMinor() # return all possible decimal parts of ICD9 codes
#' icd9ExpandMinor(1) # "1"  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
#' icd9ExpandMinor("1") # same
#' @template minor
#' @return NA for invalid minor, otherwise a vector of all possible (perhaps
#'   non-existent) sub-divisions.
#' @keywords internal
icd9ExpandMinor <- function(minor="") {
  if (length(minor) > 1) stop("icd9ExpandMinor received more than one code to expand")
  if (!is.character(minor)) stop("icd9ExpandMinor expects character input only")
  
  # minor should be 0-2 character, digits only
  if (nchar(minor) > 2)
    stop("icd9ExpandMinor: starting length already too long!")
  
  # iterate through minors to generate all possible child codes.
  while (max(nchar(minor))<2) {
    newStrings <- appendZeroToNine(minor)
    minor <- unique(append(minor, newStrings)) # and add the new ones of any length
  }
  minor
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
#' @return character vector of converted ICD-9 codes
#' @export
icd9DecimalToShort <- function(icd9Decimal, leadingZeroes = T) {
  if (!is.character(icd9Decimal)) stop("icd9DecimalToShort must be given character string, not a numeric type")
  #if (!all(grepl(pattern="[VvEe0-9\\.]", icd9Decimal)) stop()
  
  #warnIfInvalidICD9(icd9Decimal, short=F)
  icd9Decimal[!icd9ValidDecimal(icd9Decimal)] <- NA
  
  x <- icd9ExtractPartsDecimal(icd9Decimal, leadingZeroes=T) # returns everything zero-padded. Good default behaviour.
  x[is.na(x[["minor"]]), "minor"] <- "" # NA to ""
  # skipping zero padding only when asked, and only when minor is empty, otherwise it would simply give the wrong code.
  if (!leadingZeroes && any(x[["minor"]] == "")) x[x[["minor"]] == "", "major"] <- icd9DropZeroFromDecimal(x[x[["minor"]] == "", "major"])
  y <- paste(x$major, x$minor, sep="")
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
#' @template validate
#' @export
icd9ShortToDecimal <- function(icd9Short, leadingZeroes = F, keepLoneDecimal = F, validate = F) {
  
  # unclear whether I should validate icd9 codes here at all.
  if (validate) stopifnot(icd9ValidShort(icd9Short))
  
  if (class(icd9Short) != 'character') stop('icd9Short must be a character: number values could be ambiguous if converted blindly to character')
  
  # short icd9 codes are always zero-padded, since there is no other way to
  # unambiguously represent codes<100. Therefore, leadingZeroes=F has to strip the
  # extra zeros.
  parts <- icd9ExtractPartsShort(icd9Short) 
  out <- paste( parts[["major"]], ".", parts[["minor"]], sep="") # should only be max of 6 chars...
  if (!keepLoneDecimal && any(parts[["minor"]] == "")) out[parts[["minor"]] == ""] <- parts[parts[["minor"]] == "", "major"]
  if (!leadingZeroes) return (icd9DropZeroFromDecimal(out))
  out
}

#' @title extract major and minor parts of a decimal ICD-9 code
#' @description accepts Vxxxx Exxxx or xxxxx
#' @template icd9-short
#' @param minorEmpty vector of length one, to be used in place of
#'   minor part of zero. Defaults to ""
#' @return data.frame with two columns. At least the minor part must be
#'   character, because "03" is different to "3", but "30" is the same as "3" at
#'   least in ICD-9 if leadingZeroes is true, then the major part must also be
#'   character.
#' @export
icd9ExtractPartsShort <- function(icd9Short, minorEmpty = "") {
  x <- data.frame(
    major=substr(trim(icd9Short), 0, 3),
    minor=substr(trim(icd9Short), 4, 5),  # probably breaks for E codes
    stringsAsFactors=F
  )
  x[!is.na(x[["minor"]]) & x[["minor"]]=="", "minor"] <- minorEmpty  # equivalent to =="0"
  x
}

#' @title icd9ExtractPartsDecimal
#' @template icd9-decimal
#' @template leadingZeroes
#' @param minorEmpty vector of length one, to be used in place of
#'   minor part of zero. Defaults to ""
#' @template validate
#' @export
icd9ExtractPartsDecimal <- function(icd9Decimal, leadingZeroes=T, minorEmpty="", validate=F) {
  if (validate) icd9Decimal[!icd9ValidDecimal(icd9Decimal)] <- NA
  a <- strsplit(trim(icd9Decimal), "\\.")
  x <- as.data.frame(
    do.call(rbind, lapply(a, '[', 1:2)),
    stringsAsFactors = F
  )  # this may be slow! (need to flip axes from list to data frame)
  names(x) <- c("major","minor")
  if (leadingZeroes) x[["major"]] <- icd9leadingZeroesDecimal(x[["major"]])
  x[is.na(x[["minor"]]), "minor"] <- minorEmpty
  x
}

#' @title pad decimal icd9 codes with leading zeroes
#' @template icd9-decimal
#' @return character vector of icd9 codes
#' @keywords internal
icd9leadingZeroesDecimal <- function(icd9Decimal) {
  # zero pad the major part:
  parts <- icd9ExtractPartsDecimal(icd9Decimal, leadingZeroes=F) # avoid infinite recursion!
  lzdf <- parts
  # now just pad the numeric-only icd9 major parts, and strip whitespace from V and E codes
  ivd <- icd9ValidDecimalN(icd9Decimal)
  lzdf[ivd, "major"] <- icd9leadingZeroesMajor(parts[ivd, "major"])
  # TODO: strip whitespace from V & E?
  lzDecimal <- paste(lzdf$major, lzdf$minor, sep=".")
  # get rid of NA errors, which sprintf insists on converting to "NA". Also "xxx" becomes "xxx.", so drop these, too
  lzDecimal[lzdf$minor==""] <- lzdf[lzdf$minor=="", "major"]
  # if either part is 'NA', then return NA for that value
  lzDecimal[is.na(parts$major) | is.na(parts$minor)] <- NA
  lzDecimal
}

#' @title add leading zeroes to short-form ICD-9 code
#' @description Use with care. non-decimal ICD-9 codes with length<5 are often 
#'   ambiguous. E.g. 100 could be 1.00 10.0 or 100
#' @template icd9-any
#' @export
icd9leadingZeroesShort <- function(icd9) {
  parts <- icd9ExtractPartsShort(icd9)
  parts[["major"]] <- icd9leadingZeroesMajor(parts[["major"]])
  out <- icd9PartsToShort(parts=parts)
  out[!icd9ValidShort(icd9)] <- NA # set NA for invalid inputs (may be done downstream?)
  out
}

#' @title drop zero padding from decimal ICD-9 code.
#' @description decimal form ICD-9 codes are not ambiguous if the leading zeroes
#'   are dropped. Some short-form ICD-9 codes would not be ambiguous, e.g. "1" 
#'   but many would be problematic, so no function is provided to do this for 
#'   short-form codes.
#' @template icd9-any
#' @export
#' @return character vector of ICD-9 codes with extra zeroes dropped from major
#'   part
#'   
icd9DropZeroFromDecimal <- function(icd9) {
  stopifnot(icd9ValidDecimal(icd9))
  vapply(
    X = strMultiMatch(
      pattern = "[[:space:]]*([VvEe]?)(0*)([\\.[:digit:]]+)[[:space:]]*", 
      text = icd9),
    FUN = function(x) paste(x[[1]], x[[3]],sep=""),
    FUN.VALUE = character(1) # template result PER vapply 'row'
  )
}

#' @rdname icd9leadingZeroesDecimal
#' @title zero-pad major part of ICD9 code
#' @description three digit codes are returned unchanged, one and two digit 
#'   codes are preceded by 00 or 0. V or E codes are trimmed for whitespace and 
#'   returned without prefxing. NA values are generated where the code is
#'   non-numeric, and doesn't match a valid V or E code format.
#' @template major
#' @return character vector
#' @keywords internal
icd9leadingZeroesMajor <- function(major) {
  # possible inputs here, for each element of vector, are: integer, non-integer
  # double, integer double, string of integer, string of double, V or E code,
  # other string, other crap.
  
  numMajor <- asNumericNoWarn(major) # make integers and characters into numeric (double) type
  vOrE <- icd9ValidShortV(major) | icd9ValidShortE(major)
  isIntMajor <- !vOrE & (numMajor %% 1 < 1e-6) # catch modulo rounding errors
  if (any(!vOrE & !isIntMajor, na.rm=T)) stop("if numeric type is given, it must acutally be an integer, but I see a floating point")
  out <- rep(x=NA, times=length(major))
  out[isIntMajor & !is.na(isIntMajor)] <- sprintf("%03d", numMajor[isIntMajor & !is.na(isIntMajor)]) # will give NAs for V and E codes, which are unhelpfully converted to "NA"
  outve <- trim(major[vOrE])
  outve[nchar(outve) == 2] <- paste(substr(outve, 1, 1), "0", substr(outve, 2, 2), sep="") # if single digit V or E code, then slip in a zero.
  out[vOrE] <- outve
  out
}

#' @title recompose major and minor parts into icd9 codes
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
icd9PartsRecompose <- function(major=NULL, minor=NULL, parts=NULL, sep) {
  if (!is.null(parts)) {
    stopifnot(is.null(major), is.null(minor)) # enforce parts OR major, minor
    return(paste(icd9leadingZeroesMajor(parts[["major"]]),parts[["minor"]], sep=sep))
  }
  #stopifnot(length(major) == length(minor), length(major) !=1, length(minor) !=1) 
  stopifnot(length(sep) ==1)
  paste(icd9leadingZeroesMajor(major), minor, sep=sep)
}

#' @describeIn icd9PartsRecompose
#' @export
icd9PartsToShort <- function(major=NULL, minor=NULL, parts=NULL) icd9PartsRecompose(major=major, minor=minor, parts=parts, sep="")

#' @describeIn icd9PartsRecompose
#' @export
icd9PartsToLong <- function(major=NULL, minor=NULL, parts=NULL) icd9PartsRecompose(major=major, minor=minor, parts=parts, sep=".")

#' @title explain ICD9 codes
#' @description convert full format (123.45 style) ICD9 codes into the name and description for human review
#' there are official ICD9-CM data tables, not with conversion to decimal notation, but to the textual format. May be better to use these over Hopkins?
#' TODO: could also lookup against the short codes to validate? The Hopkins ICD9 lookup table is a mess. 
#' @template icd9-any
#' @examples
#' \dontrun{
#' icd9Explain(ahrqComorbid)
#' }
#' @return data frame, or list of data frames, with fields for ICD9 code, name and description, derived from datamart lookup table
#' @seealso package \code{comorbidities}
#' @references \url{http://www.stata.com/help.cgi?icd9}
#' @export
icd9Explain <- function(icd9) UseMethod("icd9Explain")

icd9Explain.list <- function(icd9) lapply(icd9, icd9Explain)
icd9Explain.numeric <- function(icd9) icd9Explain.character(as.character(icd9))
icd9Explain.character <- function(icd9) {
  #todo: use s3 class to quickly differentiate short and decimal form icd9 codes.
  
  # lookup table directly from CMS is: icd9CmDesc
  
  out <- icd9CmDesc[ icd9CmDesc$icd9 %in% icd9leadingZeroesDecimal(icd9), ]
  row.names(out) <- NULL
  names(out) <- c("ICD9 Code", "Diagnosis", "Description")
  out
}

#icd9Simplify <- function(icd9) {
#  stop("to implement.")
#  #Will take a list of ICD9 codes,     and reply with the minimum number of ICD9
#  #codes    which represent the group.") this doesn't work for real ICD9 codes,
#  #because they rarely fill out all decimal child possibilities, if ever
#}

#' @title read the ICD-9-CM description data as provided by the Center for 
#'   Medicaid Services.
#' @description ICD9-CM data unfortunately has no comma separation, so have to 
#'   pre-process. Note that this canonical data doesn't specify non-diagnostic 
#'   higher-level codes, just the specific diagnostic 'child' codes.
#' @details ideally would get ICD9-CM data zip directly from CMS web page, and 
#'   extract, but the built-in unzip only extracts the first file in a zip.
#' @param save logical whether to attempt to save output in package source tree 
#'   data directory
#' @param path absolute path in which to save parsed data
#' @return invisibly return the result
#' @export
parseIcd9Cm <- function(save = F, path = "~/icd9/data") {
  f <- file(system.file("extdata","CMS32_DESC_LONG_DX.txt", package='icd9'), "r")
  r <- readLines(f, encoding="latin1")
  close(f)
  r<-strsplit(r, " ")
  icd9LongCode <- lapply(r, FUN=function(row) row[1])
  icd9LongDesc <- lapply(r, FUN=function(row) paste(row[-c(1,2)], collapse=" "))
  
  f <- file(system.file("extdata","CMS32_DESC_SHORT_DX.txt", package='icd9'), "r")
  r <- readLines(f, encoding="latin1")
  r<-strsplit(r, " ")
  icd9ShortCode <- lapply(r, FUN=function(row) row[1])
  icd9ShortDesc <- lapply(r, FUN=function(row) paste(row[-c(1,2)], collapse=" "))
  icd9CmDesc <- data.frame(icd9=unlist(icd9LongCode), descLong=unlist(icd9LongDesc), descShort=unlist(icd9ShortDesc))
  
  # attempt to write the date from the source file to RData in the package source tree.
  if (save) saveSourceTreeData("icd9CmDesc", path = path)
  
  invisible(icd9CmDesc)
}
