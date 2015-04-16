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

#' @name convert
#' @title Convert ICD9 codes between formats and structures.
#' @description ICD-9 codes are represented in \emph{short} and \emph{decimal} forms.
#' The short form has up to 5 digits, or V or E followed by up to four digits. The decimal form
#' has a dedcimal point to delimit the top-level (henceforth \emph{major}) category, and the
#' \emph{minor} part containing the subsidiary classifications.
#'
#' @section
#' Structures used in this package are:
#' \itemize{
#'   \item vector (usually character vectors) of \emph{short} or \emph{long} codes
#'   \item list, containing two elements with equal length vectors called \code{major}
#'   \code{minor}. Yes, this is very like a data frame, but we do not need the overhead
#'   of structuring it that way.
#'   \item two vectors of separated major and minor parts
#'   }
#' @template major
#' @template minor
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @param parts data.frame with major and minor fields. This can be given
#'   instead of major and minor vectors
#' @param minorEmpty vector of length one, to be used in place of
#' minor part of zero. Defaults to ""
#' @template isShort
#' @return Character vector or list. Deliberately returns zero-padded major, because
#'   otherwise we are creating ambiguous codes (even if we know what we mean)
#' @import Rcpp
#' @family ICD-9 convert
NULL

#' @title convert the chapter headings to lists of codes
#' @description the chapter headings can be converted into the full set of their
#'   children, and then used to look-up which chapter, sub-chapter, or 'major' a
#'   given code belongs. Always returns a map with short-form icd-9 codes. These
#'   can be converted en masse with \code{lapply} and \code{icd9ShortToDecimal}.
#' @param x Either a chapter list itself, or the name of one, e.g.
#'   icd9ChaptersSub
#' @family ICD-9 convert
#' @import checkmate
#' @keywords internal manip
icd9ChaptersToMap <- function(x) {
  if (is.character(x)) x <- get(x)
  checkList(x, types = "character", any.missing = FALSE, min.len = 1, names = "unique")
  ranges <- names(x)
  map <- list()
  for (r in ranges) {
    map[[r]] <- icd9ExpandRangeShort(x[[r]][1], x[[r]][2], onlyReal = FALSE)
  }
  map
}

#' @title convert ICD data from wide to long format
#' @description This is different enough to \code{dcast} in \code{reshape2} that
#'   it needs writing again specifically for ICD codes. This function packages
#'   the core \code{reshape} function. Empty strings and NA values will be
#'   dropped, and everything else kept. No validation of the ICD codes is done.
#' @param x \code{data.frame} in wide format, i.e. one row per patient, and
#'   multiple columns containing ICD codes, empty strings or NA.
#' @template visitid
#' @param icdLabels vector of column names in which codes are found. If NULL,
#'   all columns matching icd or ICD will be included.
#' @param icdName character vector length one containing the new column name for
#'   the ICD codes, defaults to "icdCode"
#' @param icdRegex vector of character strings containg a regex to identify ICD-9 diagnosis
#'   columns to try (case-insensitive) in order. Default is \code{c("icd", "diag", "dx_", "dx")}
#' @template verbose
#' @return data frame with visitId column named the same as input, and a column
#'   named by \code{icd.name} containing all the non-NA and non-empty codes
#'   found in the wide input data.
#' @examples
#'   widedf <- data.frame(visitId = c("a", "b", "c"),
#'     icd9_01 = c("441", "4424", "441"),
#'     icd9_02 = c(NA, "443", NA))
#'   icd9WideToLong(widedf)
#' @family ICD-9 convert
#' @export
icd9WideToLong <- function(x,
                           visitId = NULL,
                           icdLabels = NULL,
                           icdName = "icdCode",
                           icdRegex = c("icd", "diag", "dx_", "dx"),
                           verbose = FALSE) {
  assertDataFrame(x, min.rows = 1, min.cols = 2)
  assertString(icdName)
  assertCharacter(icdRegex, min.chars = 1, any.missing = FALSE, min.len = 1)
  assertFlag(verbose)

  if (is.null(icdLabels)) {
    re <- length(icdRegex)
    while (re > 0) {
      if (verbose) message("checking whether regex: '", rev(icdRegex)[re], "' catches ICD columns.")
      icdLabels <- grep(rev(icdRegex)[re], names(x), ignore.case = TRUE, value = TRUE)
      if (verbose) message("found labels: ", paste(icdLabels, collapse = ", "))
      if (length(icdLabels)) break
      re <- re - 1
    }
  }
  assertCharacter(icdLabels, any.missing = FALSE, min.chars = 1,
                  min.len = 1, max.len = ncol(x) - 1)
  stopifnot(all(icdLabels %in% names(x)))

  visitId <- getVisitId(x, visitId)

  res <- reshape(x,
                 direction = "long",
                 varying = icdLabels,
                 idvar = visitId,
                 timevar = NULL,
                 v.names = icdName)

  res <- res[!is.na(res[[icdName]]), ]
  res <- res[nchar(asCharacterNoWarn(res[[icdName]])) > 0, ]
  res <- res[order(res[[visitId]]), ]
  rownames(res) <- NULL
  res
}

#' @title convert ICD data from long to wide format
#' @description This is more complicated than reshape or reshape2::dcast allows.
#'   This is a reasonably simple solution using built-in functions.
#' @param icd9df data.frame of long-form data, one column for visitId and one
#'   for ICD code
#' @template visitid
#' @template icd9field
#' @param prefix character, default "icd_" to prefix new columns
#' @param min.width, single integer, if specified, writes out this many columns
#'   even if no patients have that many codes. Must be greater than or equal to
#'   the maximum number of codes per patient.
#' @param aggregate single logical value, if TRUE (the default) will take more
#'   time to find out-of-order visitIds, and combine all the codes for each
#'   unique visitId. If \code{FALSE}, then out-of-order visitIds will result in
#'   a row in the output data per contiguous block of identical visitIds.
#' @param return.df single logical value, if \code{TRUE}, return a data frame
#'   with a field for the visitId. This may be more convenient, but the default
#'   of \code{FALSE} gives the more natural return data of a matrix with
#'   rownames being the visitIds.
#' @examples
#'   longdf <- data.frame(visitId = c("a", "b", "b", "c"),
#'     icd9 = c("441", "4424", "443", "441"))
#'   icd9LongToWide(longdf)
#'   icd9LongToWide(longdf, prefix = "ICD10_")
#' @family ICD-9 convert
#' @keywords manip
#' @export
icd9LongToWide <- function(icd9df,
                           visitId = NULL,
                           icd9Field = NULL,
                           prefix = "icd_",
                           min.width = 0,
                           aggregate = TRUE,
                           return.df = FALSE) {

  icd9Field <- getIcdField(icd9df, icd9Field)
  visitId <- getVisitId(icd9df, visitId)

  assertDataFrame(icd9df, col.names = "named")
  assertString(prefix)
  assertCount(min.width, na.ok = FALSE)
  assertFlag(aggregate)
  assertFlag(return.df)

  # we're now going to return a matrix
  icd9VisitWasFactor <- is.factor(icd9df[[visitId]])
  if (icd9VisitWasFactor) ivLevels <- levels(icd9df[[visitId]])
  icd9df[[visitId]] <- asCharacterNoWarn(icd9df[[visitId]])
  icd9df[[icd9Field]] <- asCharacterNoWarn(icd9df[[icd9Field]])
  if (return.df) {
    mat <- icd9LongToWideCpp(icd9df, visitId, icd9Field, aggregate)
    if (icd9VisitWasFactor)
      rownm <- factor(x = rownames(mat), levels = ivLevels)
    else
      rownm <- rownames(mat)
    df.out <- cbind(rownm, as.data.frame(unname(mat)), stringsAsFactors = icd9VisitWasFactor)
    names(df.out)[1] <- visitId
    # perhaps leave (duplicated) rownames which came from the matrix:
    rownames(df.out) <- NULL
    nc <- ncol(df.out) - 1
    if (nc < min.width) {
      df.out <- cbind(df.out, matrix(rep(NA, min.width - nc), nrow = 1))
      nc <- min.width
    }
    names(df.out)[-1] <- paste(prefix, sprintf("%03d", 1:nc), sep = "")
    return(df.out)
  }
  icd9LongToWideCpp(icd9df, visitId, icd9Field, aggregate)
}

#' @title convert matrix of comorbidities into data frame, preserving visitId
#'   information
#' @param x Matrix of comorbidities, with row and columns names defined
#' @param visitId Single character string with name for new column in output
#'   data frame. Everywhere else, \code{visitId} describes the input data, but
#'   here it is for output data.
#' @param stringsAsFactors whether the resulting data frame should have strings,
#'   i.e. visitId converted to factor. Default is to follow the current session
#'   option.
#' @examples
#' longdf <- data.frame(visitId = c("a", "b", "b", "c"),
#'     icd9 = c("441", "4424", "443", "441"))
#' mat <- icd9ComorbidElix(longdf)
#' class(mat)
#' typeof(mat)
#' rownames(mat)
#' df.out <- icd9ComorbidMatToDf(mat)
#' stopifnot(is.data.frame(df.out))
#' # output data frame has a factor for the visitId column
#' stopifnot(identical(rownames(mat), as.character(df.out$visitId)))
#' df.out[, 1:4]
#' @export
icd9ComorbidMatToDf <- function(x, visitId = "visitId",
                                stringsAsFactors = getOption("stringsAsFactors")) {
  checkMatrix(x, min.rows = 1, min.cols = 1, row.names = "named", col.names = "named")
  checkString(visitId)
  checkFlag(stringsAsFactors)
  out <- data.frame(rownames(x), x, stringsAsFactors = stringsAsFactors)
  names(out)[1] <- visitId
  out
}

#' @title convert matrix of comorbidities into data frame, preserving visitId
#'   information
#' @param x data frame, with a \code{visitId} column (not necessarily first),
#'   and other columns with flags for comorbidities, as such column names are
#'   required.
#' @template visitid
#' @param stringsAsFactors whether the resulting data frame should have strings,
#'   i.e. visitId converted to factor. Default is to follow the current session
#'   option.
#' @examples
#' longdf <- data.frame(visitId = c("a", "b", "b", "c"),
#'     icd9 = c("441", "4424", "443", "441"))
#' cmbdf <- icd9ComorbidElix(longdf, return.df = TRUE)
#' class(cmbdf)
#' rownames(cmbdf)
#' mat.out <- icd9ComorbidDfToMat(cmbdf)
#' stopifnot(is.matrix(mat.out))
#' mat.out[, 1:4]
#' @export
icd9ComorbidDfToMat <- function(x, visitId = NULL,
                                stringsAsFactors = getOption("stringsAsFactors")) {
  checkDataFrame(x, min.rows = 1, min.cols = 2, col.names = "named")
  checkFlag(stringsAsFactors)
  visitId <- getVisitId(x, visitId)

  out <- as.matrix.data.frame(x[-which(names(x) == visitId)])
  rownames(out) <- x[[visitId]]
  out
}
