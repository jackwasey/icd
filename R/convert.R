#' @title convert between icd9 decimal and short formats
#' @description converted decimal ICD9 code, e.g. 123.45 to 'short' e.g. 12345
#'   non-decimal format
#' @template icd9-decimal
#' @template invalid
#' @return character vector of converted ICD-9 codes
#' @family ICD-9 convert
#' @keywords manip
#' @export
icd9DecimalToShort <- function(icd9Decimal,
                               invalidAction = icd9InvalidActions) {

  if (!is.character(icd9Decimal))
    stop("icd9DecimalToShort must be given character string,
         not a numeric type")
  if (length(icd9Decimal) == 0) return(character())

  icd9Decimal <- icd9ValidNaWarnStopDecimal(icd9Decimal, invalidAction)

  # should return everything zero-padded by default. Good default behaviour.
  x <- icd9DecimalToParts(icd9Decimal)

  x[is.na(x$minor), "minor"] <- "" # NA to ""
  y <- paste(
    icd9AddLeadingZeroesMajor(x$major, addZeroV = TRUE,
                              invalidAction = "ignore"),
    x$minor,sep  = "")
  y[is.na(x$major)] <- NA # to avoid "NA" strings appearing...
  y
}

#' @title convert decimal-form ICD-9 code to major and minor parts
#' @template icd9-decimal
#' @param minorEmpty vector of length one, to be used in place of
#'   minor part of zero. Defaults to ""
#' @template invalid
#' @keywords manip
#' @export
icd9DecimalToParts <- function(icd9Decimal, minorEmpty = "",
                               invalidAction = icd9InvalidActions) {

  stopifnot(length(minorEmpty) == 1)
  if (is.na(minorEmpty)) minorEmpty <- NA_character_

  if (length(icd9Decimal) == 0) return(data.frame(major = character(),
                                                  minor = character()))
  icd9Decimal <- icd9ValidNaWarnStopDecimal(icd9Decimal,
                                            match.arg(invalidAction))
  icd9Decimal <- trim(icd9Decimal)
  icd9Decimal[icd9Decimal == ""] <- "." # don't ask
  a <- strsplit(icd9Decimal, ".", fixed = TRUE)
  x <- as.data.frame(
    do.call(rbind, lapply(a, "[", 1:2)),
    stringsAsFactors = FALSE
  )  # this may be slow! (need to flip axes from list to data frame)
  names(x) <- c("major", "minor")
  # if major is NA, then I think the minor must be NA, regardless of minorEmpty.
  x[is.na(x$minor) & !is.na(x$major), "minor"] <- minorEmpty
  x
}

#' @title extract major part from short or decimal ICD-9 code
#' @description Simply extracts parts, then returns only the major part in a
#'   character vector
#' @template icd9-any
#' @template isShort
#' @template invalid
#' @return character vector
icd9GetMajor <- function(icd9, isShort,
                         invalidAction = icd9InvalidActions) {
  invalidAction <- match.arg(invalidAction)
  if (isShort) {
    i <- icd9ShortToParts(icd9Short = icd9, invalidAction)
  } else {
    i <- icd9DecimalToParts(icd9Decimal = icd9, invalidAction)
  }
  i$major
}

#' @rdname icd9GetMajor
#' @template icd9-decimal
icd9DecimalToMajor <- function(icd9Decimal,
                               invalidAction = icd9InvalidActions) {
  icd9GetMajor(icd9 = icd9Decimal, isShort = FALSE,
               invalidAction = match.arg(invalidAction))
}

#' @rdname icd9GetMajor
#' @template icd9-short
icd9ShortToMajor <- function(icd9Short,
                             invalidAction = icd9InvalidActions) {
  icd9GetMajor(icd9 = icd9Short, isShort = TRUE,
               invalidAction = match.arg(invalidAction))
}

#' @title convert short-form ICD-9 code to decimal form
#' @description converts ICD-9 'short' form to decimal form
#' @template icd9-short
#' @template invalid
#' @family ICD-9 convert
#' @keywords manip
#' @export
icd9ShortToDecimal <- function(icd9Short,
                               invalidAction = icd9InvalidActions) {
  # prevalidate regardless of invalidAction - TODO: ensure this is done
  # consistently for every public entry point.
  if (!is.character(icd9Short))
    stop("icd9Short must be a character: number values could be ambiguous if converted blindly to character")

  if (length(icd9Short) == 0) return(character())

  icd9Short <- icd9ValidNaWarnStopShort(icd9Short,  match.arg(invalidAction))
  parts <- icd9ShortToParts(icd9Short)
  out <- paste( parts$major, ".", parts$minor, sep = "")
  if (any(parts$minor == "") || is.na(parts$minor)) {

    out[parts$minor == "" | is.na(parts$minor)] <-
      parts[parts$minor == "" | is.na(parts$minor), "major"]
  }

  # paste inevitably makes <NA> into a string "NA", so if major was NA, then
  # restore that:
  out[is.na(parts$major)] <- NA_character_
  out
}

#' @title extract major and minor parts of a decimal ICD-9 code
#' @description accepts Vxxxx Exxxx or xxxxx
#' @template icd9-short
#' @param minorEmpty vector of length one, to be used in place of
#'   minor part of zero. Defaults to ""
#' @template invalid
#' @return data.frame with two columns. At least the minor part must be
#'   character, because "03" is different to "3", but "30" is the same as "3"
#' @keywords  manip
#' @export
icd9ShortToParts <- function(icd9Short, minorEmpty = "",
                             invalidAction = icd9InvalidActions) {
  icd9Short <- icd9ValidNaWarnStopShort(icd9Short, invalidAction)
  # assume bytes not unicode, for speed.
  eCodes <- icd9IsE(icd9Short)
  icd9Short <- strip(icd9Short)
  x <- icd9MajMinToParts(
    major = substr(icd9Short, 0, 3),
    minor = substr(icd9Short, 4, 5)
  )
  # now fix the E codes:
  x[eCodes, "major"] <- substr(icd9Short[eCodes], 0, 4)
  x[eCodes, "minor"] <- substr(icd9Short[eCodes], 5, 5)
  x$minor[is.na(x$minor)] <- minorEmpty
  if (minorEmpty != "")
    x[!is.na(x$minor) & x$minor == "", "minor"] <- minorEmpty
  x
}

icd9ShortToPartsNV <- function(icd9Short) {
  vapply(X = icd9Short,
         FUN.VALUE = c("", ""),
         FUN = function(x) c(substr(x, 0, 3), substr(x, 4, 5))
  )
}

icd9ShortToPartsE <- function(icd9Short) {
  vapply(X = icd9Short,
         FUN.VALUE = c("", ""),
         FUN = function(x) c(substr(x, 0, 4), substr(x, 5, 5))
  )
}

#' @title recompose major and minor parts into icd9 codes
#' @aliases icd9PartsToShort icd9PartsToDecimal
#' @description internal function which checks vector lengths to avoid
#'   unintentional recycling of vectors when lengths differ. Length of one is
#'   fine for major or minor.
#' @param parts data.frame with major and minor fields. This can be given
#'   instead of major and minor vectors
#' @template isShort
#' @return character vector. Deliberately returns zero-padded major, because
#'   otherwise we are creating ambiguous codes (even if we know what we mean)
#' @family ICD-9 convert
#' @keywords internal
icd9PartsRecompose <- function(parts, isShort) {

  major <- asCharacterNoWarn(parts$major)
  minor <- asCharacterNoWarn(parts$minor)

  ## TODO: don't allow Vx single digit V codes to be appended when making a short.

  # only allow pass through of non-zero-padded majors in short if no minor.
  # Otherwise, major is passed through unchanged.
  if (isShort)
    major <- icd9AddLeadingZeroesMajor(major, addZeroV = TRUE)

  minor[is.na(minor)] <- ""

  if (isShort)
    out <- sprintf("%s%s", major, minor)
  else
    out <- sprintf("%s.%s", major, minor)
  out[is.na(major)] <- NA_character_
  out
}

#' @rdname icd9PartsRecompose
#' @export
icd9PartsToShort <- function(parts)
  icd9PartsRecompose(parts = parts, isShort = TRUE)


#' @rdname icd9PartsRecompose
#' @export
icd9PartsToDecimal <- function(parts)
  icd9PartsRecompose(parts = parts, isShort = FALSE)

#' @rdname icd9PartsRecompose
#' @description icd9MajMinToDf simply composes the data frame needed
#'   as input to the PartsToXxxx functions
#' @export
icd9MajMinToParts <- function(major, minor)
  data.frame(major = major, minor = minor, stringsAsFactors = FALSE)

#' @rdname icd9PartsRecompose
#' @description icd9MajMinTo\{Short|Decimal\} simply composes the data frame
#'   needed as input to the PartsToXxxx functions. Having two inputs breaks the
#'   ability to 'pipe' commands together using \code{magrittr}, so passing a
#'   single \code{data.frame} is preferred.
#' @export
icd9MajMinToShort <- function(major, minor)
  icd9PartsToShort(icd9MajMinToParts(major, minor))

#' @rdname icd9PartsRecompose
#' @export
icd9MajMinToDecimal <- function(major, minor)
  icd9PartsToDecimal(parts = icd9MajMinToParts(major, minor))

#' @title convert the chapter headings to lists of codes
#' @description the chapter headings can be converted into the full set of their
#'   children, and then used to look-up which chapter, sub-chapter, or 'major' a
#'   given code belongs. Always returns a map with short-form icd-9 codes. These
#'   can be converted en masse with \code{lapply} and \code{icd9ShortToDecimal}.
#' @param x Either a chapter list itself, or the name of one, e.g.
#'   icd9ChaptersSub
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
