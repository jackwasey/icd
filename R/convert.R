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
#' @param major
#' @param minor
#' @param icd9-any
#' @param icd9-short
#' @param icd9-long
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
#' @keywords internal manip
icd9ChaptersToMap <- function(x) {
  if (length(x) == 1) x <- get(x)
  ranges <- names(x)
  map <- list()
  for (r in ranges) {
    map[[r]] <- icd9ExpandRangeShort(x[[r]][1], x[[r]][2])
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
#' @param icd.labels vector of column names in which codes are found. If NULL,
#'   all columns matching icd or ICD will be included.
#' @param icd.name character vector length one containing the new column name
#'   for the ICD codes, defaults to "icd9Code"
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
                           icd.labels = NULL,
                           icd.name = "icdCode") {
  if (is.null(icd.labels))
    icd.labels <- grep("icd", names(x), ignore.case = TRUE, value = TRUE)
  else
    stopifnot(all(icd.labels %in% names(x)))

  if (is.null(visitId)) {
    if (!any(names(x) == "visitId"))
      visitId <- names(x)[1]
    else
      visitId <- "visitId"
  } else
    stopifnot(visitId %in% names(x))
  stopifnot(is.character(visitId))
  stopifnot(length(visitId) == 1)

  res <- reshape(x,
                 direction = "long",
                 varying = icd.labels,
                 idvar = visitId,
                 timevar = NULL,
                 v.names = icd.name)

  rownames(res) <- NULL
  res <- res[!is.na(res[[icd.name]]), ]
  res <- res[nchar(levels(res[[icd.name]])[res[[icd.name]]]) > 0, ]
  res[order(res[[visitId]]), ]
}

#' @title convert ICD data from long to wide format
#' @description This is more complicated than reshape or reshape2::dcast allows.
#'   This is a reasonably simple solution using built-in functions.
#' @param x data.frame of long-form data, one column for visitId and one for ICD
#'   code
#' @param visitId single character, if NULL will use "visitId" if exists,
#'   otherwise the first column
#' @param icdId single character string with name of column containing the ICD
#'   codes. If left as NULL, the field is guessed to be the first matching ICD
#'   or icd, and a warning is given if multiple columns match.
#' @param prefix character, default "icd_" to prefix new columns
#' @param empty value to fill out empty fields of wide output data, defaults to
#'   \code{NA}
#' @param width, single integer, if specified, writes out this many columns even
#'   if no patients have that many codes. Must be greater than or equal to the
#'   maximum number of codes per patient.
#' @examples
#'   longdf <- data.frame(visitId = c("a", "b", "b", "c"),
#'     icd9 = c("441", "4424", "443", "441"))
#'   icd9LongToWide(longdf)
#'   icd9LongToWide(longdf, prefix = "ICD10_", empty = "")
#' @family ICD-9 convert
#' @export
icd9LongToWide <- function(x,
                           visitId = NULL,
                           icdId = NULL,
                           prefix = "icd_",
                           empty = NA,
                           width = NULL) {
  stopifnot(is.data.frame(x))
  if (is.null(visitId)) {
    if (!any(names(x) == "visitId"))
      visitId <- names(x)[1]
    else
      visitId <- "visitId"
  } else
    stopifnot(visitId %in% names(x))
  stopifnot(is.character(visitId))
  stopifnot(length(visitId) == 1)

  if (is.null(icdId)) {
    im <- grep("icd", names(x), ignore.case = TRUE, value = TRUE)
    if (length(im) >= 1) {
      icdId <- im[1]
      if (length(im) > 1) warning("multiple possible ICD column: using first")
    } else
      stop("no ICD column found: use argument icdId to specify it")
  } else
    stopifnot(icdId %in% names(x))

  lst <- aggregate(x[names(x) %nin% visitId], by = x[visitId], asCharacterNoWarn)
  ncol <- max(sapply(lst[[icdId]], length))
  if (is.null(width))
    width = ncol
  else
    stopifnot(ncol <= width)

  m <-matrix(data = empty,
             nrow = length(lst[[visitId]]),
             ncol = width)
  for (v in 1:length(lst[[visitId]])) {
    codes <- lst[[icdId]][[v]]
    m[v, 1:length(codes)] <- codes
  }
  res <- data.frame(lst[visitId], as.data.frame(m))
  names(res)[-1] <- paste(prefix, sprintf("%02d", 1:ncol), sep = "")
  res
}
