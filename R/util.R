
#' @title trim whitespace at ends of a line
#' @param x is a character vector to trim
#' @return character vector
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
saveSourceTreeData <- function(varName, path="~/icd9") {
  require(devtools)
  devpath <- file.path(path, "data") 
  stopifnot(file.exists(devpath))
  save(list  = varName, 
       envir = parent.frame(), # get from my parent
       file  = file.path(devpath, paste0(varName, ".RData"))
  )
       #compress="xz"
}

