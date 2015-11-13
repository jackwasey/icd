# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

#' Trim leading and trailing whitespace from a single string
#'
#' \code{NA} is accepted (and returned as \code{NA_character_})
#' @param x character vector of length one
#' @return character vector of length one
#' @keywords internal
strim <- function(x) {
  checkmate::assertString(x, na.ok = TRUE)
  if (!is.na(x[1]))
    .Call("icd9_strimCpp", PACKAGE = get_pkg_name(), as.character(x))
  else
    return(NA_character_)
}

#' Trim leading and trailing whitespace
#'
#' \code{NA} is accepted and returned, probably as \code{NA_character_}
#' @param x character vector
#' @return character vector
#' @keywords internal
trim <- function (x) {
  nax <- is.na(x)
  x[!nax] <- .Call("icd9_trimCpp", PACKAGE = get_pkg_name(), as.character(x[!nax]))
  x
}

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
  if (is.character(x)) return(x)
  old <- options(warn = -1)
  on.exit(options(old))
  if (is.integer(x)) fastIntToStringRcpp(x)
  if (is.factor(x)) return(levels(x)[x])
  as.character(x)
}

"%nin%" <- function(x, table)
  match(x, table, nomatch = 0) == 0

#' Strip character(s) from character vector
#'
#' @param x character vector
#' @param pattern passed to \code{gsub} default is " "
#' @param useBytes passed to gsub, default is the slightly quicker \code{TRUE}
#' @return character vector of same length as input
#' @keywords internal
strip <- function(x, pattern = " ", useBytes = TRUE)
  gsub(pattern = pattern, replacement = "", x = x,
       fixed = TRUE, useBytes = useBytes)

#' Save given variable in package data directory
#'
#' File is named varname.RData with an optional suffix before .RData
#'
#' @param var_name character or symbol, e.g. "myvar" or \code{myvar}, either of which
#'   would find \code{myvar} in the parent environment, and save it as
#'   \code{myvar.RData} in \code{package_root/data}.
#' @param suffix character scalar
#' @keywords internal
save_in_data_dir <- function(var_name, suffix = "") {
  assertString(suffix)
  var_name <- as.character(substitute(var_name))
  stopifnot(exists(var_name, envir = parent.frame()))
  save(list = var_name,
       envir = parent.frame(),
       file = file.path("data", strip(paste0(var_name, suffix, ".RData"))),
       compress = "xz")
  message("Now reload package to enable updated/new data: ", var_name)
}

#' @title encode TRUE as 1, and FALSE as 0 (integers)
#' @description when saving data as text files for distribution, printing large
#'   amounts of text containing TRUE and FALSE is inefficient. Convert to binary
#'   takes more R memory, but allows more compact output
#' @param x dataframe which may contain logical fields
#' @return data frame without logical fields
#' @keywords internal manip
logical_to_binary <- function(x) {
  stopifnot(is.data.frame(x) || is.matrix(x))

  if (is.matrix(x)) {
    checkmate::assertMatrix(x, min.rows = 1, min.cols = 1)
    return(as.logical(x))
  } else if (is.data.frame(x))
    checkmate::assertDataFrame(x, min.rows = 1, min.cols = 1)
  else
    stop("need a matrix or data.frame")

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
    )[ -1]
  )
  if (!dropEmpty) return(result)
  result[sapply(result, function(x) length(x) != 0)]
}

#' @rdname strMultiMatch
#' @description \code{str_pair_match} differs in that there should only be two
#'   pairs of parenthesis, then the first (by default) becomes the name, and the
#'   second the value.
#' @param swap logical scalar, whether to swap the names and values. Default is
#'   not to swap, so the first match becomes the name.
#' @keywords internal
str_pair_match <- function(text, pattern, swap = FALSE, dropEmpty = FALSE, pos = c(1, 2), ...) {
  assertString(pattern)
  assertCharacter(text, min.len = 1)
  assertFlag(swap)
  assertFlag(dropEmpty)
  assertIntegerish(pos, len = 2, lower = 1, any.missing = FALSE)

  res <- strMultiMatch(pattern = pattern, text = text,
                       dropEmpty = dropEmpty, ...)

  outNames <- vapply(X = res,
                     FUN = "[",
                     FUN.VALUE = character(1),
                     ifelse(swap, pos[2], pos[1]))
  stopifnot(all(!is.na(outNames)))

  out <- vapply(X = res,
                FUN = "[",
                FUN.VALUE = character(1),
                ifelse(swap, pos[1], pos[2]))
  stopifnot(all(!is.na(out)))

  names(out) <- outNames
  out
}

# nocov start

#' unzip a single file
#' @keywords internal
#' @importFrom utils download.file unzip
unzip_single <- function(url, file_name, save_path) {
  zipfile <- tempfile()
  download.file(url = url, destfile = zipfile, quiet = TRUE)
  zipdir <- tempfile()
  dir.create(zipdir)
  unzip(zipfile, exdir = zipdir)  # files="" so extract all
  files <- list.files(zipdir)
  if (is.null(file_name)) {
    if (length(files) == 1) {
      file_name <- files
    } else {
      stop("multiple files in zip, but no file name specified: ",
           paste(files, collapse = ", "))
    }
  } else
    stopifnot(file_name %in% files)

  file.copy(file.path(zipdir, file_name), save_path, overwrite = TRUE)
}

#' Get a zipped file from a URL, or confirm it is in data-raw already
#'
#' @param url url of a zip file
#' @param file_name file name of a single file in that zip
#' @param force logical, if TRUE, then download even if already in \code{data-raw}
#' @return path of unzipped file in \code{data-raw}
#' @keywords internal
unzip_to_data_raw <- function(url, file_name, force = FALSE) {
  pkg_name <- get_pkg_name()
  data_raw_path <- system.file("data-raw", package = pkg_name)
  save_path <- file.path(data_raw_path, file_name)
  if (force || !file.exists(save_path))
    stopifnot(
      unzip_single(url = url, file_name = file_name, save_path = save_path)
    )
  save_path
}

# so that I can change to another package name when needed.
# \code{getPackageName} gives globalenv if running interactively.
get_pkg_name <- function() "icd9"

# nocov end

getVisitId <- function(x, visitId = NULL) {
  guesses <- c("visit.?Id", "patcom", "encounter.?id", "enc.?id",
               "in.*enc", "out.*enc", "visit", "enc")
  checkDataFrame(x, min.cols = 1, col.names = "named")

  if (is.null(visitId)) {
    for (guess in guesses) {
      guess_matched <- grep(guess, names(x), ignore.case = TRUE, value = TRUE)
      if (length(guess_matched) == 1) {
        visitId <- guess_matched
        break
      }
    }
    if (is.null(visitId))
      visitId <- names(x)[1]
  }
  assertString(visitId)
  stopifnot(visitId %in% names(x))
  visitId
}

# guess which field contains the (only) ICD code, in order of preference
# case-insensitive regex. If there are zero or multiple matches, we move on down
# the list, meaning some later possibilities are more or less specific regexes
# than earlier ones.
getIcdField <- function(x, icd9Field = NULL) {
  guesses <- c("icd.?9", "icd.?9.?Code", "icd", "diagnos", "diag.?code", "diag", "i9")
  checkDataFrame(x, min.cols = 1, col.names = "named")
  if (is.null(icd9Field)) {
    for (guess in guesses) {
      guess_matched <- grep(guess, names(x), ignore.case = TRUE, value = TRUE)
      if (length(guess_matched) == 1) {
        icd9Field <- guess_matched
        break
      }
    }
    if (is.null(icd9Field))
      # still NULL so fallback to second column
      icd9Field <- names(x)[2]
    # Could look at contents of the data frame, although this evaluates a
    # promise on potentially a big data frame, so could be costly
  }
  assertString(icd9Field)
  stopifnot(icd9Field %in% names(x))
  icd9Field
}

getLatestBillableVersion <- function() "32"

#' @title trim null or empty values from a list
#' @param x list
#' @return trimmed list
#' @keywords internal
listTrimFlat  <-  function(x) {
  suppressWarnings(
    x[sapply(x, length) != 0 &
        sapply(x, nchar) != 0 &
        !sapply(x, function(y) all(is.null(y))) &
        !sapply(x, function(y) all(is.na(y)))
      ]
  )
}

#' @title swap names and values of a vector
#' @param x named vector
#' @return vector
#' @keywords internal
swapNamesWithVals <- function(x) {
  assertVector(x, strict = TRUE, any.missing = FALSE, names = "named")
  new_names <- unname(x)
  x <- names(x)
  names(x) <- new_names
  x
}

# mimic the R CMD check test
getNonASCII <- function(x)
  x[isNonASCII(x)]

isNonASCII <- function (x) {
  is.na(iconv(x, from = "latin1", to = "ASCII"))
}

utils::globalVariables(c("do_slow_tests", "do_online_tests"))

skip_slow_tests <- function(msg = "skipping slow test") {
  if (!exists("do_slow_tests") || !do_slow_tests)
    testthat::skip(msg)
}

skip_online_tests <- function(msg = "skipping online test") {
  if (!exists("do_online_tests") || !do_online_tests)
    testthat::skip(msg)
}

# will be in next release of testthat
skip_on_travis <- function() {
  if (!identical(Sys.getenv("TRAVIS"), "true")) return()
  testthat::skip("On Travis")
}


# nocov start

#' Fast Factor Generation
#'
#' This function generates factors more quickly, by leveraging
#' \code{fastmatch::\link{fmatch}}. The speed increase for ICD-9 codes is about
#' 33% reduction for 10 million codes.
#'
#' \code{NaN}s are converted to \code{NA} when used on numerics. Extracted from
#' https://github.com/kevinushey/Kmisc.git
#'
#' These feature from base R are missing: \code{exclude = NA, ordered =
#' is.ordered(x), nmax = NA}
#' @author Kevin Ushey, adapted by Jack Wasey
#' @importFrom fastmatch fmatch
#' @param x An object of atomic type \code{integer}, \code{numeric},
#'   \code{character} or \code{logical}.
#' @param levels An optional character vector of levels. Is coerced to the same
#'   type as \code{x}. By default, we compute the levels as
#'   \code{sort(unique.default(x))}.
#' @param labels A set of labels used to rename the levels, if desired.
#' @param na.last If \code{TRUE} and there are missing values, the last level is
#'   set as \code{NA}; otherwise; they are removed.
#' @examples
#' \dontrun{
#' pts <- icd9:::randomUnorderedPatients(1e7)
#' u <- unique.default(pts$icd9)
#' # this shows that stringr (which uses stringi) sort takes 50% longer than
#' # built-in R sort.
#' microbenchmark::microbenchmark(sort(u), stringr::str_sort(u))
#'
#' # this shows that \code{factor_} is about 50% faster than \code{factor} for
#' # big vectors of strings
#'
#' # without sorting is much faster:
#' microbenchmark::microbenchmark(factor(pts$icd9),
#'                                factor_(pts$icd9),
#'                                factor_nosort(pts$icd9),
#'                                times=25)
#' }
#' @keywords internal manip
factor_ <- function(x, levels = NULL, labels = levels, na.last = NA) {

  if (is.factor(x)) return(x)
  if (is.null(levels)) levels <- sort(unique.default(x), na.last = na.last)
  suppressWarnings(f <- fmatch(x, levels, nomatch = if (isTRUE(na.last)) length(levels) else NA_integer_))
  levels(f) <- as.character(labels)
  class(f) <- "factor"
  f
}

# nocov end

#' @rdname factor_
#' @details I don't think there is any requirement for factor levels to be
#'   sorted in advance, especially not for ICD-9 codes where a simple
#'   alphanumeric sorting will likely be completely wrong.
#' @keywords internal manip
factor_nosort <- function(x, levels = NULL, labels = levels) {
  # sort may be pre-requisite for fastmatch
  if (is.factor(x)) return(x)
  if (is.null(levels)) levels <- unique.default(x)
  suppressWarnings(f <- fmatch(x, levels))
  levels(f) <- as.character(labels)
  class(f) <- "factor"
  f
}
