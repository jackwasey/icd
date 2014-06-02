#' @rdname icd9ToComorbidities
#' @title match ICD9 codes
#' @description This does the hard work of finding whether a given icd9 code
#'   falls under a group of reference ICD9 codes. baseCodes is expanded to cover
#'   all possible subgroups, then we look for matches where the given ICD9 codes
#'   appear in the baseCodes. 
#'   http://www.acep.org/Clinical---Practice-Management/V-and-E-Codes-FAQ/
#' @seealso comorbidities.
#' @param icd9codes vector, either decimal (string or floating point) or short
#'   form (must be character)
#' @param baseCodes vector, decimal (but may be string or floating point)
#' @param icd9codeShort true or false, default to accept short codes
#' @param baseCodeShort true or false, default to accept long codes
#' @return logical vector of which icd9codes match or are subcategory of
#'   baseCodes
#' @keywords internal
icd9ToComorbid <- function(icd9codes, baseCodes, icd9codeShort=TRUE, baseCodeShort=FALSE) {
  
  if (!class(icd9codes) %in% c("character","numeric","integer")) stop("icd9ToComorbid expects a character or number vector for the icd9codes to examine, but got: ", class(icd9codes))
  if (!class(baseCodes) %in% c("character","numeric","integer")) stop("icd9ToComorbid expects a character or number vector for the basecodes,to avoid ambiguity with trailing zeroes, but got: ", class(baseCodes))
  if (class(icd9codeShort)!='logical') stop("icd9ToComorbid expects logical value for icd9codeShort")
  if (class(baseCodeShort)!='logical') stop("icd9ToComorbid expects logical value for baseCodeShort")
  
  if (length(icd9codeShort)>1 )  stop("icd9ToComorbid got vector for icd9CodeShort, expected single TRUE or FALSE value")
  if (length(baseCodeShort)>1 )  stop("icd9ToComorbid got vector for baseCodeShort, expected single TRUE or FALSE value")
  if (length(icd9codeShort)==0 )  stop("icd9ToComorbid got empty vector for icd9CodeShort, expected single TRUE or FALSE value")
  if (length(baseCodeShort)==0 )  stop("icd9ToComorbid got empty vector for baseCodeShort, expected single TRUE or FALSE value")
  
  if (length(baseCodes)==0) stop("icd9ToComorbid expects at least one icd9 code to test against")
  
  warnIfInvalidICD9(icd9codes, callingFunction="icd9ToComorbid-icd9codes", short=icd9codeShort)
  #maybe do once, not every loop: stopIfInvalidICD9(baseCodes, callingFunction="icd9ToComorbid-baseCodes", short=baseCodeShort)
  
  # take a regular string of an ICD9 code of format (ABC.zxyz) with or without leading and trailing zeroes.
  #top level ICD9 code and return T/F if the icd9codes fall within subgroups
  icd9codes %in% icd9DecimalToShort(
    c(
      lapply(baseCodes, FUN=function(x) icd9ExpandBaseCode(icd9=x, short=baseCodeShort)), 
      recursive=TRUE)
  )
}


#' @rdname icd9ToComorbidities
#' @title get co-morbidities for given list of visitId codes of in-patients
#' @description merges the visitId list against the pre-calculated icd9 comorbidities
#' get co-morbidities flagged as Present On Arrival (POA) for given list of PATCOMs
#' get co-morbidities actively marked not Present On Arrival (POA) for given list of PATCOMs
#' @param patcom number or character vector
#' @param patcomField defaults to 'patcom'
#' @param TODO: icd9lk is one of 'comorbidAllInpt','comorbidPoaInpt','comorbidNotPoaInpt'
#' @return data.frame with input patcoms merged with all ever comorbidities
#' @keywords internal
lookupComorbiditiesAll <- function(dat, visitId, icd9lk='comorbidAllInpt', mergeFun=mergeBetter) {
  
  if (icd9lk %nin% c('comorbidAllInpt','comorbidPoaInpt','comorbidNotPoaInpt')) 
    stop("the icd9 comorbidities must be one of: 'comorbidAllInpt','comorbidPoaInpt','comorbidNotPoaInpt' but I received ", icd9lk)
  
  # comorbidAllInpt <- icd9codesToComorbidities(icd9diagInpt, visitId="patcom", icd9Field="i9diag")
  
  mp <- do.call(mergeFun, list(x=dat, by.x=visitId, y=get(icd9lk), by.y=visitId, leftOuterJoin=T))
  
  # update just the new logical rows replacing NA with FALSE. This happens when a patient has no comorbidities.
  comorbidityNames <- names(get(icd9lk))
  comorbidityNames <- comorbidityNames[comorbidityNames != visitId] # select only the logical fields, not the patcom field
  mp[, comorbidityNames] <- mp[, comorbidityNames] & !is.na(mp[, comorbidityNames])
  mp
}

#' @rdname icd9ToComorbidities
#' @export
lookupComorbiditiesAll <- function(dat, visitId)
  lookupComorbidities(dat, visitId, 'comorbidAllInpt')

#' @rdname icd9ToComorbidities
#' @export
lookupComorbiditiesPoa <- function(dat, visitId)
  lookupComorbidities(dat, visitId, 'comorbidPoaInpt')

#' @rdname icd9ToComorbidities
#' @export
lookupComorbiditiesPoa <- function(dat, visitId)
  lookupComorbidities(dat, visitId, 'comorbidNotPoaInpt')

#' @title generate all child codes for given decimal ICD9 codes
#' @description take ICD9 codes in decimal form and lists of all possible
#'   sub-classification codes: e.g. 1.1 returns 1.11, 1.12, 1.13 etc. There are
#'   no codes like 1.10 which are distinct from 1.1, so this can be purely
#'   numeric Also, note that expanding "100.0" (100.00 to 100.09) is different
#'   from expanding "100.00" (no expansion) \code{0.1 == .3/3} is a problem...
#'   \url{http://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f}
#'   
#' @param baseCode ICD9 codes in xxx.xx format
#' @return unsorted vector of ICD9 codes for all subsections of the provided
#'   code.
icd9ExpandBaseCodeDecimal <- function(icd9) {
  
  if (!is.character(icd9)) stop('baseCode must be character only to avoid ambiguity')
  
  stopIfInvalidICD9(icd9, short=F)
  
  #vOrE <- grepl(pattern="[VvEe]", x=baseCode)
  parts <- icd9ExtractPartsDecimal(icd9, minorEmpty="")
  out <- c()
  for (r in rownames(parts)) {
    out <- append(out, paste(parts[[r, "major"]], icd9ExpandMinor(parts[[r, "minor"]]), sep="."))
  }
  out
}

#' @title expand 5 character form 'short' ICD9 to all possible sub codes
#' @description this is so the raw info from SAS code provided by AHRQ can be interpreted without manually reformatting.
#' @param icd9 0 or whitespace padded on left, whitespace padded on right, character
icd9ExpandBaseCodeShort <- function(icd9) {
  if (!is.character(icd9)) stop('must have character only input to expand a short basecode to avoid ambiguity')
  
  stopIfInvalidICD9(icd9, short=T)
  
  # split into major and minor parts
  parts <- icd9ExtractPartsShort(icd9, minorEmpty="")
  out <- c()
  for (r in 1:nrow(parts)) {
    out <- c(out, icd9PartsToShort(parts[[r, "major"]], icd9ExpandMinor(parts[[r, "minor"]])))
  }
  out
}

#' @describeIn icd9ExpandBaseCodeShort
icd9ExpandBaseCode <- function(icd9, short) {
  if (short) return(icd9ExpandBaseCodeShort(icd9))
  return(icd9ExpandBaseCodeDecimal(icd9))
}

#' @title sort short-form icd9 codes
#' @description should work with numeric only, V or E codes.
#' Note that a numeric sort does not work for ICD-9 codes, since "162" > "1620"
#' TODO: write tests
#' @param icd9 character vector of short-form ICD-9 codes
#' @return sorted vector of ICD-9 codes
#' @export
icd9SortShort <- function(icd9) {
  tmp <- strsplit(icd9, "") # split into characters
  # convert to matrix and pad out to five characters, starting from the left
  xmatrix <- do.call(rbind, lapply(tmp, '[', 1:5)) 
  # then order by column, starting from the left:
  xmatrix <- xmatrix[order(xmatrix[,1], xmatrix[,2], xmatrix[,3], xmatrix[,4], xmatrix[,5], na.last = FALSE),]
  # and piece it togehter again, replacing NA with ""
  apply(xmatrix, 
        MARGIN=1, 
        function(x) { x[is.na(x)] <- ""; paste(x, collapse="") }
  )
}

# TODO: make this suffix N?
icd9GenerateShort <- function() icd9ExpandBaseCodeShort(as.character(1:999))
# TODO: this is not quite right...
icd9GenerateShortV <- function() paste("V", icd9ExpandBaseCodeShort(as.character(1:99)), sep="")
# TODO: this is not quite right, since E99 is invalid. 
icd9GenerateShortE <- function() sub("0", "", paste("E", icd9ExpandBaseCodeShort(as.character(80:99)), sep=""))


#' @describeIn icd9ExpandRangeShort
#' @export
"%icd9%" <- function(start, end) icd9ExpandRangeShort(start=start, end=end)

#' @title take SAS icd9 format for character range and expand to a list of codes
#' @description this is pretty horrible code, covering a whole load of edge 
#'   cases relating to the fact that icd9 codes are not in numeric order. An 
#'   alternative strategy would be to list all the ICD9 codes, then a range 
#'   would just pick out start and finish positions, and return subset of the 
#'   list. Not all ICD-9 codes are valid, including some parent codes which have
#'   valid children. However, I expect at least some of these have been used in
#'   some billing databases.
#' #examples
#' "4280 " %icd9% "4289 "
#' "V80 " %icd9% " V8210 "
#' icd9ExpandRangeShort("4280 ", "43014") # should give all codes in 428 EXCEPT "428", and all codes upto 43014 EXCEPT 430 and 4301
#' @export
icd9ExpandRangeShort <- function(start, end) {
  stopifnot(length(start)==1 && length(end)==1)
  if (nchar(start)==nchar(end) && start>end) stop("start is after end time")
  sdf <- icd9ExtractPartsShort(start)
  edf <- icd9ExtractPartsShort(end)
  startMajor <- icd9ZeroPadMajor(sdf[["major"]]) # zero pad to tolerate entering "1" instead of "001"
  endMajor <- icd9ZeroPadMajor(edf[["major"]]) 
  startMinor <- sdf[["minor"]]
  endMinor <- edf[["minor"]]
  
  # use icd9ExtractAlphaNumeric to get just the numbers (ignore V or E), and compare those:
  majorNumsOnly <- as.integer(icd9ExtractAlphaNumeric(c(startMajor, endMajor))[,2])
  if (!all(icd9SortShort(c(start, end)) == c(start, end))) 
    stop("start is after end")
  if (!all(icd9SortShort(c(startMajor, endMajor)) == c(startMajor, endMajor))) 
    stop("start is after end (major part)")
  
  # deal with special case where start major = start minor
  if (startMajor == endMajor) {
    #if (startMinor > endMinor) # not obvious, because "1" is greater than "02"
    #  stop("majors match and end minor has broader scope than start minor. startMinor=", 
    #       startMinor, " and endMinor=", endMinor)
    # the following result works when minors have same length, but incomplete for start minor shorter.
    result <- icd9PartsToShort(startMajor,
                               intersect(icd9SubsequentMinors(startMinor), icd9PrecedingMinors(endMinor))
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
  for (major in startMajor %i9m% endMajor) {
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
#' @param start, end character or numeric icd9 major part (i.e. part before
#'   decimal point, or first Vxx, Exxx or xxx, with or without zero padding)
#' @return character vector with range inclusive of start and end
#' @export
"%i9m%" <- function(start, end) {
  stopifnot(length(start)==1 && length(end)==1)
  c <- icd9ExtractAlphaNumeric(start)
  d <- icd9ExtractAlphaNumeric(end)
  stopifnot(toupper(c[1]) == toupper(d[1])) #
  paste(c[,1], c[,2]:d[,2], sep="")
}

#' @title extract numeric part of icd9 code, i.e. remove whitespace and V or E 
#'   prefix
#' @param icd9 an icd9 code which is not validated. Can be short or decimal 
#'   form.
#' @return vector or matrix, with first item of each row (or whole vector) 
#'   containing V, E or "". The second part contains the numeric parts of the 
#'   code, which may include a decimal point.
#' @export
icd9ExtractAlphaNumeric <- function(icd9) {
  icd9c <- as.character(icd9) # so we can accept numbers
  l <- regmatches(
    x = icd9c,
    m = regexec(
      pattern="([VvEe]?)([[:digit:].]+)", 
      text = icd9c
    )
  )
  # flip the list into a matrix with a row for each code, and the alpha part in
  # first column, and numeric part in the second
  t(vapply(l, function(x) matrix(data=x[2:3],nrow=1,ncol=2),FUN.VALUE=rep(NA_character_, times=2)))
}

#' @title determine subsequent post-decimal parts of ICD9 codes
#' @description this is not simply numeric, since "4" is after "39" and "0" !=
#'   "00". Frustrating to have to do so much string manipulation, but, as I have
#'   learned, it is not possible to treat ICD-9 codes as numbers without risking
#'   ambiguity and subtle mistakes.
#' @param minor is a character vector of length one.
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
#' @param minor character or numeric of decimal part of an ICD9 code. Not a
#'   vector, just a single value.
#' @return NA for invalid minor, otherwise a vector of all possible (perhaps
#'   non-existent) sub-divisions.
#' @keywords internal
icd9ExpandMinor <- function(minor="") {
  if (length(minor) > 1) stop("icd9ExpandMinor received more than one code to expand")
  if (!is.character(minor)) stop("icd9ExpandMinor expects character input only")
  
  # minor should be 0-2 character, digits only
  if (nchar(minor) > 2)
    stop("icd9ExpandMinor: starting length already too long!
          This is an error in ICD9 to comorbidity mapping, not source data.")
  
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
#'   package, but this has a load of messy seq functions in the middle of the
#'   comorbid groups specification. My way allows simple specification of, e.g.
#'   code "100" and all possible child codes are captured.
#' #examples
#' #appendZeroToNine("1")
#' #appendZeroToNine(1:3)
#' @param str vector of numbers (or character representation of numbers)
#' @return vector of characters with 0 to 9 appended to each input value
#' @keywords internal
appendZeroToNine <- function(str) {
  if (!allIsNumeric(str)) stop("appendZeroToNine expects number input, or character input representing numbers")
  apply(expand.grid(str, as.character(0:9),""), 1, paste, collapse="")
}

#' @title stop or warn, then  log if any of given ICD9 codes is invalid
#' @param icd9codes vector of character or numeric type containing icd9 codes
#' @param callingFunction not implemented: ideally look at call stack and indicate who called here.
#' @export
stopIfInvalidICD9 <- function(icd9codes, callingFunction="", short) {
  if (short && any(!icd9ValidShort(icd9codes)))
    stop("Invalid short-form ICD9 codes found: ", getInvalidShortICD9(icd9codes))
  if (!short && any(!icd9ValidDecimal(icd9codes))) 
    stop("Invalid long-form ICD9 codes found: ", getInvalidDecimalIcd9(icd9codes))
}

#' @describeIn stopIfInvalidICD9
#' @export
warnIfInvalidICD9 <- function(icd9codes, callingFunction="", short) {
  if (short && any(!icd9ValidShort(icd9codes)))
    warning("Invalid short-form ICD9 codes found: ", getInvalidShortICD9(icd9codes))
  if (!short && any(!icd9ValidDecimal(icd9codes))) 
    warning("Invalid long-form ICD9 codes found: ", getInvalidDecimalIcd9(icd9codes))
}

#' @title check whether icd9 codes are valid
#' @description Check validity of short or 'long' (i.e. decimal form) ICD9 codes. 
#' The codes may be numeric disease descriptiors or V or E prefixed for procedures or ?complications.
#' @details Long form is not ambiguous. However, the decimal-free form is stored
#' in the Hopkins database. The numbers are written as characters so the
#' essential preceding zeroes are included. This means a non-decimal ICD9 code,
#' like 1000, is ambiguous and should make an error.
#' @param icd9 vector of character or numeric icd9 codes, in decimal format
#' @return logical vector with T or F for each icd9 code provided according to validity
#' @seealso http://www.stata.com/users/wgould/icd9/icd9.hlp and http://www.sascommunity.org/wiki/Validate_the_format_of_ICD-9_codes
#' @export
icd9ValidDecimal <- function(icd9) {
  
  if (class(icd9) != "character" & class(icd9) != 'factor') 
    stop("icd9ValidDecimal expects factor, character or numeric vector input but got class: ", class(icd9))
  if (length(icd9)==0) stop("icd9ValidDecimal expects at least one icd9 code to test")
  
  # quick numeric check, although I think working purely in character would be
  # more reliable. e.g. by not introducing weird rounding errors using %%
  if (is.numeric(icd9)) {
    icd9 <- as.numeric(icd9)
    return(
      icd9>=0 & icd9<1000 & (icd9*100%%1 == 0) # allow zero, which is 'no code' code.
    )
  }
  
  icd9ValidDecimalN(icd9) | icd9ValidDecimalV(icd9) | icd9ValidDecimalE(icd9)
}

#' @describeIn icd9ValidDecimal
icd9ValidShort <- function(icd9) {
  if (!(class(icd9) %in% c("character","factor"))) { 
    stop("isValidShortICD9 expects character vector input. Numeric is ambiguous, 
          so not allowed (although integers would not be ambiguous, simpler to stick to character-only.")
    # this is not just invalid data: there is a programming error in the data structure
  }
  if (length(icd9) == 0) { 
    warning("isValidShortICD9 expects at least one icd9 code to test") 
    return() # return NULL, equivalent of c()
  }
  if (class(icd9) =="factor") # quicker to test rather than always try to convert
    icd9 <- asCharacterNoWarn(icd9) # factor levels are always character, so no concern about introducing ambiguity with e.g. short code of 100 vs 10.0 (0100, 0010)
  
  # as explained in details, a numeric short ID has different validity requirements than a string because of leading zeroes.
  icd9ValidShortN(icd9) | icd9ValidShortV(icd9) | icd9ValidShortE(icd9)
}

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidShortV <- function(icd9) grepl("^[[:space:]]*[Vv](([1-9][[:digit:]])|([[:digit:]][1-9]))[[:digit:]]{0,2}[[:space:]]*$", icd9)

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidShortE <- function(icd9) grepl("^[[:space:]]*[Ee][89][[:digit:]]{2,3}[[:space:]]*$", icd9)

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidShortN <- function(icd9) grepl("^[[:space:]]*[[:digit:]]{1,5}[[:space:]]*$", icd9) # need to allow 0, but not 0.xx as valid code

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidDecimalV <- function(icd9) grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$", icd9)

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidDecimalE <- function(icd9) grepl("^[[:space:]]*[Ee][89][[:digit:]]{2}(\\.[[:digit:]]?)?[[:space:]]*$", icd9)

#' @describeIn icd9ValidDecimal
#' @export
icd9ValidDecimalN <- function(icd9) grepl("^[[:space:]]*((0{1,3})|([1-9][[:digit:]]{0,2})|(0[1-9][[:digit:]]?)|(00[1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$", icd9) # not quite right, since it would validate 0.12

#' @describeIn icd9ValidDecimal
#' @export
getInvalidDecimalIcd9 <- function(icd9) icd9[!icd9ValidDecimal(icd9)]

#' @describeIn icd9ValidDecimal
#' @export
getInvalidShortICD9 <- function(icd9) icd9[!icd9ValidShort(icd9)]

#' @title convert between icd9 decimal and short formats
#' @description converted decimal ICD9 code, e.g. 123.45 to 'short' e.g. 12345 
#'   non-decimal format, or vice versa
#' @param icd9Decimal is a vector of numeric or character type in ICD9 decimal 
#'   or short form
#' @return character vector of converted ICD-9 codes
#' @export
icd9DecimalToShort <- function(icd9Decimal) {
  if (!is.character(icd9Decimal)) stop("icd9DecimalToShort must be given character string, not a numeric type")
  #if (!all(grepl(pattern="[VvEe0-9\\.]", icd9Decimal)) stop()
  
  #warnIfInvalidICD9(icd9Decimal, short=F)
  icd9Decimal[!icd9ValidDecimal(icd9Decimal)] <- NA
  
  icd9Decimal <- icd9ZeroPadDecimal(icd9Decimal)
  x <- icd9ExtractPartsDecimal(icd9Decimal)
  x[is.na(x[["minor"]]), "minor"] <- "" # NA to ""
  y <- paste(x$major, x$minor, sep="")
  y[is.na(x[["major"]])] <- NA
  y
}

#' @describeIn icd9DecimalToShort
#' @export
icd9ShortToDecimal <- function(icd9Short) {
  # unclear whether I should validate icd9 codes here at all.
  
  if (class(icd9Short) != 'character') stop('icd9Short must be a character: number values could be ambiguous if converted blindly to character')
  
  if (any(nchar(icd9Short) < 4)) {
    warning('%d (out of %d ) icd9 short codes are too short so setting to NA. Min nchar is %d', sum(any(nchar(icd9Short<4))), length(icd9Short), min(nchar(icd9Short)))
    icd9Short[nchar(icd9Short) < 4 ] <- NA
  }
  paste0( substr(icd9Short, 1, 3), ".", substr(icd9Short, 4, 10)) # should only be max of 6 chars...
}

#' @title extract major and minor parts of a decimal ICD-9 code
#' @description accepts Vxxxx Exxxx or xxxxx
#' @param icd9 vector of icd-9 codes in character or numeric form
#' @param padMajor logical defaults to true, meaning that the major part will be
#'   padded to three characters with leading zeros. Not applicable for V or E
#'   codes.
#' @param zero character string, vector of length one, to be used in place of
#'   minor part of zero. Defaults to ""
#' @return data.frame with two columns. At least the minor part must be
#'   character, because "03" is different to "3", but "30" is the same as "3" at
#'   least in ICD-9 if padMajor is true, then the major part must also be
#'   character.
#' @keywords internal
icd9ExtractPartsShort <- function(icd9, padMajor=T, minorEmpty="") {
  
  x <- data.frame(
    major=substr(trim(icd9), 0, 3),
    minor=substr(trim(icd9), 4, 5),  # probably breaks for E codes
    stringsAsFactors=F
  )
  
  x[!is.na(x[["minor"]]) & x[["minor"]]=="", "minor"] <- minorEmpty  # equivalent to =="0"
  
  x
}

#' @describeIn icd9ExtractPartsShort
#' @keywords internal
icd9ExtractPartsDecimal <- function(icd9, padMajor=T, minorEmpty="", validate=F) {
  if (validate) icd9[!icd9ValidDecimal(icd9)] <- NA
  a <- strsplit(trim(icd9), "\\.")
  x <- as.data.frame(
    do.call(rbind, lapply(a, '[', 1:2)),
    stringsAsFactors = F
  )  # this may be slow! (need to flip axes from list to data frame)
  names(x) <- c("major","minor")
  if (padMajor) x[["major"]] <- icd9ZeroPadDecimal(x[["major"]])
  x[is.na(x[["minor"]]), "minor"] <- minorEmpty
  x
}

#' @rdname icd9ToComorbidities
#' @title merge comorbidities with icd9 codes per visitId (or other identity)
#' @description default response is Jack's mapping of comorbidities which was 
#'   curated by hand based on several published lists of ICD9 codes used to
#'   ennumerate comorbidities. Other options are \code{deyo} and \code{ahrq}. 
#'   This is slow with long lists of patients, so intended to be used as
#'   intermediate step to save files like comorbidPoaInpt
#' @param icd9df data.frame with fields specified by visitId and icd9Code.
#'   icd9code is assumed to be a non-decimal 'short' form ICD9 code.
#' @param icd9translate list of the comorbidities with each top-level list item
#'   containing a vector of decimal ICD9 codes
#' @export
icd9comorbidities <- function(icd9df, visitId="visitId", icd9Field="icd9Code", 
                              icd9Mapping = icd9JackMapping) {
  
  # validate mapping here, or not at all. Will stop checking down the chain of function calls.
  
  i <- cbind(
    icd9df[visitId],
    vapply(
      X = names(icd9Mapping), # loop through names of icd9 mapping, i.e. one comorbidity at a time
      FUN.VALUE = rep(FALSE, length(icd9df[[icd9Field]])), # way faster with vapply, and it keeps the logicals instead of making them character
      FUN = function(comorbidity) {
        icd9ToComorbid( 
          asCharacterNoWarn(icd9df[[icd9Field]]), # drop factor down to character codes #TODO: is this necessary?
          icd9Mapping[[comorbidity]] # provide vector of base ICD9 codes for this comorbidity group
        )
      }
    )
  )
  ag <- aggregate( x=i[,-which(names(i)==visitId)], by=list(i[[visitId]]), FUN = any, simplify=F)
  names(ag)[1] <- visitId
  ag
}

#' @title gets those comorbidities where the "Present on Arrival" (POA) flag is set or not set.
#' @description this is not a simple binary, since many codes are exempt, unspecified, or unknown. Therefore, two options are given: get all the comorbidities where the POA flag was definitely -ve, coded as "N" or definitely +ve and coded as "Y". Negating one set won't give the other set unless all codes were either Y or N.
#' @describeIn icd9comorbidities
#' @export
icd9comorbiditiesNotPoa <- function(icd9df, visitId="visitId", icd9Field="icd9Code",
                                    poaField="poa", icd9Mapping = icd9JackMapping)
  icd9comorbidities(icd9df[ is.na(icd9df[[poaField]]) | icd9df[[poaField]] != "N",],
                    visitId=visitId, icd9Field=icd9Field, icd9Mapping=icd9Mapping)

#' @describeIn icd9comorbidities
#' @export
icd9comorbiditiesPoa <- function(icd9df, visitId="visitId", icd9Field="icd9Code",
                                 poaField="poa", icd9Mapping = icd9JackMapping)
  icd9comorbidities(icd9df[!is.na(icd9df[[poaField]]) & icd9df[[poaField]] == "Y",],
                    visitId=visitId, icd9Field=icd9Field, icd9Mapping=icd9Mapping)

#' @title pad decimal icd9 codes with leading zeroes
#' @param icd9 vector of icd9 codes in character or numeric form
#' @return character vector of icd9 codes
#' @keywords internal
icd9ZeroPadDecimal <- function(icd9) {
  # zero pad the major part:
  parts <- icd9ExtractPartsDecimal(icd9, padMajor=F) # avoid recursion!
  zeroPaddedDf <- parts
  # now just pad the numeric-only icd9 major parts, and strip whitespace from V and E codes
  zeroPaddedDf[icd9ValidDecimalN(icd9),"major"] <- icd9ZeroPadMajor(parts[icd9ValidDecimalN(icd9),"major"])
  # TODO: strip whitespace from V & E?
  zeroPaddedDecimal <- paste(zeroPaddedDf$major, zeroPaddedDf$minor, sep=".")
  # get rid of NA errors, which sprintf insists on converting to "NA". Also "xxx" becomes "xxx.", so drop these, too
  zeroPaddedDecimal[zeroPaddedDf$minor==""] <- zeroPaddedDf[zeroPaddedDf$minor=="", "major"]
  # if either part is 'NA', then return NA for that value
  zeroPaddedDecimal[is.na(parts$major) | is.na(parts$minor)] <- NA
  zeroPaddedDecimal
}

#' @rdname icd9ZeroPadDecimal
#' @title zero-pad major part of ICD9 code
#' @description three digit codes are returned unchanged, one and two digit 
#'   codes are preceded by 00 or 0. V or E codes are trimmed for whitespace and 
#'   returned without prefxing. NA values are generated where the code is
#'   non-numeric, and doesn't match a valid V or E code format.
#' @param major character (but will permit integer) vector
#' @return character vector
#' @keywords internal
icd9ZeroPadMajor <- function(major) {
  # possible inputs here, for each element of vector, are: integer, non-integer
  # double, integer double, string of integer, string of double, V or E code,
  # other string, other crap.
  
  numMajor <- asNumericNoWarn(major) # make integers and characters into numeric (double) type
  vOrE <- icd9ValidShortV(major) | icd9ValidShortE(major)
  isIntMajor <- !vOrE & (numMajor %% 1 ==0)
  if (any(!vOrE & !isIntMajor, na.rm=T)) stop("if numeric type is given, it must acutally be an integer")
  out <- rep(x=NA, times=length(major))
  out[isIntMajor] <- sprintf("%03d", numMajor[isIntMajor]) # will give NAs for V and E codes, which are unhelpfully converted to "NA"
  out[vOrE] <- trim(major[vOrE])
  out
}

#' @title recompose major and minor parts into icd9 codes
#' @description internal function which checks vector lengths to avoid 
#'   unintentional recycling of vectors when lengths differ. Length of one is 
#'   fine for major or minor.
#' @param major icd9 major part
#' @param minor icd9 minor part
#' @param sep character separator, expected to be "" or "."
#' @return character vector
#' @keywords internal
icd9PartsRecompose <- function(major, minor, sep) {
  if (length(major) != length(minor) && length(major) !=1 && length(minor) !=1) stop("major and minor vectors are of non-unit differing lengths. length(major)=%d length(minor)=%d", length(major), length(minor))
  paste(icd9ZeroPadMajor(major), minor, sep=sep)
}

#' @describeIn icd9PartsRecompose
#' @export
icd9PartsToShort <- function(major, minor) icd9PartsRecompose(major=major, minor=minor, sep="")

#' @describeIn icd9PartsRecompose
#' @export
icd9PartsToLong <- function(major, minor) icd9PartsRecompose(major=major, minor=minor, sep=".")

#' @title explain ICD9 codes
#' @description convert full format (123.45 style) ICD9 codes into the name and description for human review
#' there are official ICD9-CM data tables, not with conversion to decimal notation, but to the textual format. May be better to use these over Hopkins?
#' TODO: could also lookup against the short codes to validate? The Hopkins ICD9 lookup table is a mess. 
#' @param icd9 is a vector of decimal ICD9 codes, or a list of comorbidity to icd9 code mappings.
#' The ICD9 codes may be in character or numeric form, or a mixture.
#' @return data frame, or list of data frames, with fields for ICD9 code, name and description, derived from datamart lookup table
#' @seealso package \code{comorbidities}
#' @references \url{http://www.stata.com/help.cgi?icd9}
#' @export
icd9explain <- function(icd9) UseMethod("icd9explain")

icd9explain.list <- function(icd9) lapply(icd9, icd9explain)
icd9explain.numeric <- function(icd9) icd9explain.character(as.character(icd9))
icd9explain.character <- function(icd9) {
  loadCleanData('lkI9DIAG')
  
  out <- lkI9DIAG[lkI9DIAG$DiagCode %in% icd9ZeroPadDecimal(icd9), c("DiagCode", "I9Diagnosis", "Description")]
  row.names(out) <- NULL
  names(out) <- c("ICD9 code", "Diagnosis", "Description")
  out
}

#icd9Simplify <- function(icd9) {
#  stop("to implement.")
#  #Will take a list of ICD9 codes,     and reply with the minimum number of ICD9
#  #codes    which represent the group.") this doesn't work for real ICD9 codes,
#  #because they rarely fill out all decimal child possibilities, if ever
#}
