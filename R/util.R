
# assume length is one for strim
strim <- function(x) {
  if (!is.na(x[1]))
    return(.Call("icd9_strimCpp", PACKAGE = "icd9", as.character(x)))
  return(NA_character_)
}

trim <- function (x) {
  nax <- is.na(x)
  x[!nax] <- .Call("icd9_trimCpp", PACKAGE = "icd9", as.character(x[!nax]))
  x
}

# EXCLUDE COVERAGE START

allIsNumeric <- function(x, extras = c(".", "NA", NA)) {
  old <- options(warn = - 1)
  on.exit(options(old))
  xs <- x[x %nin% c("", extras)]
  !anyNA(as.numeric(xs))
}

asNumericNoWarn <- function(x) {
  old <- options(warn = - 1)
  on.exit(options(old))
  if (is.factor(x)) x <- levels(x)[x]
  as.numeric(x)
}

asIntegerNoWarn <- function(x)
  as.integer(asNumericNoWarn(x))

asCharacterNoWarn <- function(x) {
  old <- options(warn = - 1)
  on.exit(options(old))
  if (is.factor(x)) return(levels(x)[x])
  as.character(x)
}

"%nin%" <- function(x, table)
  match(x, table, nomatch = 0) == 0

strip <- function (x, pattern = " ", useBytes = TRUE)
  gsub(pattern = pattern, replacement = "", x = x,
       fixed = TRUE, useBytes = useBytes)

saveInDataDir <- function(var, suffix = "") {
  save(list = var,
       envir = parent.frame(),
       file = file.path("data", strip(paste0(var, suffix, ".RData"))),
       compress = "xz")
}

#' @title encode TRUE as 1, and FALSE as 0 (integers)
#' @description when saving data as text files for distribution, printing large
#'   amounts of text containing TRUE and FALSE is inefficient. Convert to binary
#'   takes more R memory, but allows more compact output TODO: test
#' @details Taken from my
#'   \href{http://cran.r-project.org/web/packages/jwutil/index.html}{\code{jwutil}
#'   package}.#' @param x dataframe which may contain logical fields
#' @return data frame without logical fields
#' @keywords internal manip
logicalToBinary <- function(x) {
  checkmate::assertDataFrame(x, min.rows = 1, min.cols = 1)
  if (any(dim(x) == 0))
    stop("got zero in at least one dimension in data frame. %d, %d",
         dim(x)[1], dim(x)[2])

  # can condense this code into a one-liner, but this is clearer:
  logical_fields <- names(x)[sapply(x, is.logical)]
  if (is.na(logical_fields) || length(logical_fields) == 0) return(x)

  #update just the logical fields with integers
  x[, logical_fields] <-
    vapply(
      X         = x[, logical_fields],
      FUN       = function(y) ifelse(y, 1L, 0L),
      FUN.VALUE = integer(length = dim(x)[1])
    )
  x
}

#' @title return the actual matches from a bracketed regex
#' @description Be careful: this may throw funny results for exotic regex, but
#'   so far, it seems okay. it also drops the first result which always seems to
#'   be a duplicate or whole-string match.
#' @details Taken from my
#'   \href{http://cran.r-project.org/web/packages/jwutil/index.html}{\code{jwutil}
#'   package}.
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
#' @keywords internal
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
    )[ - 1]
  )
  if (!dropEmpty) return(result)
  result[sapply(result, function(x) length(x) != 0)]
}

#' @rdname strMultiMatch
#' @description \code{strPaitMatch} differs in that there should only be two
#'   pairs of parenthesis, then the first (by default) becomes the name, and the
#'   second the value.
#' @param swap logical scalar, whether to swap the names and values. Default is
#'   not to swap, so the first match becomes the name.
#' @keywords internal
strPairMatch <- function(pattern, text, swap = FALSE, dropEmpty = FALSE, ...) {
  checkmate::assertString(pattern)
  checkmate::assertCharacter(text, min.len = 1)
  checkmate::assertFlag(swap)
  checkmate::assertFlag(dropEmpty)

  res <- strMultiMatch(pattern = pattern, text = text,
                       dropEmpty = dropEmpty, ...)
  stopifnot(all(sapply(res, function(x) length(x) == 2)))

  outNames <- vapply(X = res,
                     FUN = "[",
                     FUN.VALUE = character(1),
                     ifelse(swap, 2, 1))
  stopifnot(all(!is.na(outNames)))

  out <- vapply(X = res,
                FUN = "[",
                FUN.VALUE = character(1),
                ifelse(swap, 1, 2))
  stopifnot(all(!is.na(out)))

  names(out) <- outNames
  out
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
  stopifnot(length(filename) <= 1)
  stopifnot(is.character(url), length(url) == 1)
  zipfile <- tempfile()
  download.file(url = url, destfile = zipfile, quiet = TRUE)
  zipdir <- tempfile()
  dir.create(zipdir)
  unzip(zipfile, exdir = zipdir)  # files="" so extract all
  files <- list.files(zipdir)
  if (is.null(filename)) {
    if (length(files) == 1) {
      filename <- files
    } else {
      stop("multiple files in zip, but no filename specified: ",
           paste(files, collapse = ", "))
    }
  } else
    stopifnot(filename %in% files)

  do.call(FUN, args = c(list(file.path(zipdir, filename), warn = FALSE),
                        list(...)))
}

# EXCLUDE COVERAGE END

getVisitId <- function(x, visitId = NULL) {
  checkmate::checkDataFrame(x, min.cols = 1, col.names = "named")
  if (is.null(visitId)) {
    if (!any(names(x) == "visitId"))
      visitId <- names(x)[1]
    else
      visitId <- "visitId"
  }
  checkmate::assertString(visitId)
  stopifnot(visitId %in% names(x))
  return(visitId)
}
