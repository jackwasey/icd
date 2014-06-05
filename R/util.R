
# these utility functions are derived from Hmisc and one of my other projects.
# All functions set for internal use in this package only to avoid namespace
# conflicts.


#' @title encode TRUE as 1, and FALSE as 0
#' @description when saving data as text files for distribution, printing large amounts of text containing TRUE and FALSE
#' is inefficient. Convert to binary takes more R memory, but allows more compact output
#' @param dframe dataframe which may contain logical fields
#' @return dframe without logical fields
#' @keywords internal
logicalToBinary <- function(dframe) {
  
  if (class(dframe) != 'data.frame') fstop("logicalToBinary expects a data frame, but got %s", class(dframe))
  if (any(dim(dframe) == 0)) fstop("got zero in at least one dimension in data frame. %d, %d", dim(dframe)[1], dim(drame)[2])
  
  # can condense this code into a one-liner, but this is clearer:
  logicalFields <- names(dframe)[sapply(dframe,class)=='logical']
  if (is.na(logicalFields) || length(logicalFields) == 0) return(dframe)
  
  #update just the logical fields with integers
  dframe[,logicalFields] <-
    vapply(
      X         = dframe[,logicalFields],
      FUN       = function(x) ifelse(x,1L,0L),
      FUN.VALUE = integer(length=dim(dframe)[1])
    )
  dframe
}

#' @title trim whitespace at ends of a line
#' @param x is a character vector to trim
#' @return character vector
#' @keywords internal
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' @title return the actual matches from a bracketed regex
#' @description Be careful: this may throw funny results for exotic regex, but
#'   so far, it seems okay. it also drops the first result which always seems to
#'   be a duplicate or whole-string match
#' @param pattern regular expression: if it has bracketed sections, these
#'   submatches are returned
#' @param text is the string to match against. This vector should be the same
#'   length as the pattern vector, or the patern vector should be length one.
#' @param ... are additional parameters passed to regexec and regmatches. I
#'   haven't tried this: it may need two separate variables containing lists of
#'   params, since this will send everything to both functions.
#' @return list of character vectors, list length being the length of the inptu
#'   text vector.
strMultiMatch <- function(pattern, text, ...) # unlist puts the name in the first position, which I don't think I ever want.
  lapply(text, function(x) unlist(regmatches(x=x, m=regexec(pattern=pattern, text=x, ...), ...))[-1])

#' @title check whether character vector represents all numeric values
#' @description check whether all the items of input vector are numeric without throwing warning
#' derived from Hmsic package
#' @param x is a character vector to be tested
#' @param extras is a vector of character strings which are counted as NA values, defaults to '.' and 'NA'
#' @return logical
#' @keywords internal
allIsNumeric <- function(x, extras=c('.','NA')) {
  old <- options(warn=-1)
  on.exit(options(old))
  xs <- x[x %nin% c('',extras)]
  !any(is.na(as.numeric(xs)))
}

#' @title convert factor or vector to character without warnings
#' @description correctly converts factors to vectors, and then converts to character, which may silently introduce NAs
#' @param x is a vector, probably of numbers of characters
#' @return character vector, may have NA values
#' @keywords internal
asCharacterNoWarn <- function(x) {
  old <- options(warn = -1)
  on.exit(options(old))
  if (class(x)=='factor') x <- levels(x)[x]
  as.character(x)
}

#' @title convert factor or vector to numeric without warnings
#' @aliases asIntegerNoWarn
#' @description correctly converts factors to vectors, and then converts to 
#'   numeric or integer, which may silently introduce NAs. Invisible rounding
#'   errors can be a problem going from numeric to integer, so consider adding
#'   tolerance to this conversion.
#' @param x is a vector, probably of numbers of characters
#' @keywords internal
#' @return numeric vector, may have NA values
asNumericNoWarn <- function(x) {
  old <- options(warn = -1)
  on.exit(options(old))
  if (class(x)=='factor') x <- levels(x)[x]
  as.numeric(x)
}

#' @describeIn asNumericNoWarn
asIntegerNoWarn <- function(x)
  as.integer(asNumericNoWarn(x))

#' @title inverse of \%in\%
#' @description borrowed from Hmisc. See %in%
#' @param x is the vector of values to be matched
#' @param table is actually a vector, to be matched against
#' @return logical vector of length of x
#' @keywords internal
"%nin%" <- function(x, table) match(x, table, nomatch = 0) == 0
# original %in% is: match(x, table, nomatch = 0L) > 0L

#' @title save data in source tree
#' @description attempt to write the date from the source file to RData in the
#'   package source tree.
#' @param varName is the variable name and the part of the filename which will
#'   be saved, followed by ".RData"
#'   @param path is a path name to destination folder for the data: no trailing slash.
saveSourceTreeData <- function(varName, path="~/icd9/data") {
  require(devtools)
  stopifnot(file.exists(path))
  save(list  = varName, 
       envir = parent.frame(), # get from my parent
       file  = file.path(path, paste0(varName, ".RData")),
       compress="XZ"
  )
}

