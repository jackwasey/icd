#
# these utility functions are derived from Hmisc and one of my other projects.
# All functions set for internal use in this package only to avoid namespace
# conflicts.

#' @title encode TRUE as 1, and FALSE as 0
#' @description when saving data as text files for distribution, printing large
#'   amounts of text containing TRUE and FALSE is inefficient. Convert to binary
#'   takes more R memory, but allows more compact output TODO: test
#' @param dframe dataframe which may contain logical fields
#' @return dframe without logical fields
#' @keywords internal manip
logicalToBinary <- function(dframe) {

  if (class(dframe) != 'data.frame') stop("logicalToBinary expects a data frame, but got %s", class(dframe))
  if (any(dim(dframe) == 0)) stop("got zero in at least one dimension in data frame. %d, %d", dim(dframe)[1], dim(dframe)[2])

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

#' @title strip all whitespace
#' @description could do this with regular expression, but slow, and this
#'   function is called frequently. My only use case works with removal of all
#'   whitespace, and I don't expect <TAB>.
#' @param x is a character vector to strip
#' @return character vector
#' @keywords internal
strip <- function (x, pattern = " ") # beware unicode
  gsub(pattern = pattern, replacement = "", x, fixed = TRUE, useBytes = TRUE)

#' @title strip whitespace from ends of each string in given character vector
#' @description slower than \code{strip}.
#' @param x is a character vector to trim
#' @return character vector
#' @keywords internal
trim <- function(x)
  gsub("^\\s+|\\s+$", "", x)

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
#' @param dropEmpty logical whether to drop rows with no matches
#' @return list of character vectors, list length being the length of the inptu
#'   text vector.
strMultiMatch <- function(pattern, text, dropEmpty = FALSE, ...) {
  # unlist puts the name in the first position, which I don't think I ever want.
  result <- lapply(
    text, function(x) unlist(
      regmatches(
        x = x,
        m = regexec(
          pattern = pattern,
          text=x, ...),
        ...)
    )[-1]
  )
  if (!dropEmpty) return(result)
  result[sapply(result, function(x) length(x) != 0)]
}

#' @rdname strMultiMatch
#' @export
#' @description \code{strPaitMatch} differs in that there should only be two
#'   pairs of parenthesis, then the first (by default) becomes the name, and the
#'   second the value.
#' @param swap logical scalar, whether to swap the names and values. Default is
#'   not to swap, so the first match becomes the name.
icd9StrPairMatch <- function(pattern, text, swap = FALSE, dropEmpty = FALSE, ...) {
  res <- strMultiMatch(pattern = pattern, text = text, dropEmpty = TRUE, ...)
  outNames <- vapply(X = res, FUN = '[', FUN.VALUE = character(1), ifelse(swap, 2, 1))
  out <- vapply(X = res, FUN = '[', FUN.VALUE = character(1), ifelse(swap, 1, 2))
  names(out) <- outNames
  out
}

#' @title check whether character vector represents all numeric values
#' @description check whether all the items of input vector are numeric without
#'   throwing warning derived from Hmsic package
#' @param x is a character vector to be tested
#' @param extras is a vector of character strings which are counted as NA
#'   values, defaults to '.' and 'NA'
#' @return logical
#' @keywords internal
allIsNumeric <- function(x, extras=c('.','NA')) {
  old <- options(warn=-1)
  on.exit(options(old))
  xs <- x[x %nin% c('',extras)]
  !any(is.na(as.numeric(xs)))
}

#' @title convert factor or vector to character without warnings
#' @description correctly converts factors to vectors, and then converts to
#'   character, which may silently introduce NAs
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
#'   tolerance to this conversion. \code{asIntegerNoWarn} silently \code{\link{floor}}s.
#' @param x is a vector, probably of numbers of characters
#' @keywords internal
#' @return numeric vector, may have NA values
asNumericNoWarn <- function(x) {
  old <- options(warn = -1)
  on.exit(options(old))
  if (class(x)=='factor') x <- levels(x)[x]
  as.numeric(x)
}

#' @rdname asNumericNoWarn
asIntegerNoWarn <- function(x)
  as.integer(asNumericNoWarn(x))

#' @rdname asNumericNoWarn
areIntegers <- function(x) {
  n <- asNumericNoWarn(x)
  i <- abs(n - floor(n)) < 1e-9
  i[is.na(i)] <- FALSE
  i
}

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
  stopifnot(file.exists(path))
  save(list  = varName,
       envir = parent.frame(), # get from my parent
       file  = file.path(path, paste0(varName, ".RData")),
       compress="xz"
  )
}

#' @title read file from zip at URL
#' @description downloads zip file, and opens named file \code{filename}, or the
#'   single file in zip if \code{filename} is not specified. FUN is a function,
#'   with additional arguments to FUN given by \dots.
#' @param url character vector of length one containing URL of zip file.
#' @param filename character vector of length one containing name of file to
#'   extract from zip. If not specified, and the zip contains a single file,
#'   then this single file will be used.
#' @param FUN function used to process the file in the zip, defaults to
#'   readLines. The first argument to FUN will be the path of the extracted
#'   \code{filename}
#' @param \dots further arguments to FUN
#' @keywords internal
read.zip.url <- function(url, filename = NULL, FUN = readLines, ...) {
  zipfile <- tempfile()
  download.file(url = url, destfile = zipfile, quiet = TRUE)
  zipdir <- tempfile()
  dir.create(zipdir)
  unzip(zipfile, exdir = zipdir) # files="" so extract all
  files <- list.files(zipdir)
  if (is.null(filename)) {
    if (length(files) == 1) {
      filename <- files
    } else {
      stop("multiple files in zip, but no filename specified: ", paste(files, collapse = ", "))
    }
  } else { # filename specified
    stopifnot(length(filename) ==1)
    stopifnot(filename %in% files)
  }
  file <- paste(zipdir, files[1], sep="/")
  do.call(FUN, args = c(list(file.path(zipdir, filename)), list(...)))
}
