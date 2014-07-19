#' @title convert between icd9 decimal and short formats
#' @description converted decimal ICD9 code, e.g. 123.45 to 'short' e.g. 12345
#'   non-decimal format
#' @template icd9-decimal
#' @template invalid
#' @return character vector of converted ICD-9 codes
#' @family ICD-9 convert
#' @keywords manip
#' @export
icd9DecimalToShort <- function(icd9Decimal, invalidAction = icd9InvalidActions) {

  if (!is.character(icd9Decimal)) stop("icd9DecimalToShort must be given character string, not a numeric type")
  if (length(icd9Decimal) == 0) return(character()) # question whether an empty vector is valid?

  icd9Decimal <- icd9ValidNaWarnStopDecimal(icd9Decimal, invalidAction)

  x <- icd9DecimalToParts(icd9Decimal) # should return everything zero-padded by default. Good default behaviour.
  x[is.na(x[["minor"]]), "minor"] <- "" # NA to ""
  y <- paste(icd9AddLeadingZeroesMajor(x$major, addZeroV = TRUE, invalidAction = "ignore"), x$minor,sep  = "")
  y[is.na(x[["major"]])] <- NA # to avoid "NA" strings appearing...
  y
}

#' @title convert decimal-form ICD-9 code to major and minor parts
#' @template icd9-decimal
#' @param minorEmpty vector of length one, to be used in place of
#'   minor part of zero. Defaults to ""
#' @template invalid
#' @keywords manip
#' @export
icd9DecimalToParts <- function(icd9Decimal, minorEmpty = "", invalidAction = icd9InvalidActions) {

  stopifnot(length(minorEmpty) == 1)
  if (is.na(minorEmpty)) minorEmpty <- NA_character_ # we're working in characters, even if given "logical" NA

  if (length(icd9Decimal) == 0) return (data.frame(major = character(), minor = character()))
  icd9Decimal <- icd9ValidNaWarnStopDecimal(icd9Decimal, match.arg(invalidAction))
  icd9Decimal <- trim(icd9Decimal)
  icd9Decimal[icd9Decimal == ""] <- "." # don't ask
  a <- strsplit(icd9Decimal, ".", fixed = TRUE)
  x <- as.data.frame(
    do.call(rbind, lapply(a, '[', 1:2)),
    stringsAsFactors = FALSE
  )  # this may be slow! (need to flip axes from list to data frame)
  names(x) <- c("major", "minor")
  # if major is NA, then I think the minor must be NA, regardless of minorEmpty.
  x[is.na(x[["minor"]]) & !is.na(x[["major"]]), "minor"] <- minorEmpty
  x
}

#' @title extract major part from short or decimal ICD-9 code
#' @description Simply extracts parts, then returns only the major part in a character vector
#' @template icd9-any
#' @template isShort
#' @template invalid
#' @return character vector
icd9GetMajor <- function(icd9, isShort, invalidAction = icd9InvalidActions) {
  invalidAction = match.arg(invalidAction)
  if (isShort) {
    i <- icd9ShortToParts(icd9Short = icd9, invalidAction)
  } else {
    i <- icd9DecimalToParts(icd9Decimal = icd9, invalidAction)
  }
  i[["major"]]
}

#' @rdname icd9GetMajor
#' @template icd9-decimal
icd9DecimalToMajor <- function(icd9Decimal, invalidAction = icd9InvalidActions) {
  icd9GetMajor(icd9 = icd9Decimal, isShort = FALSE, invalidAction = match.arg(invalidAction))
}

#' @rdname icd9GetMajor
#' @template icd9-short
icd9ShortToMajor <- function(icd9Short, invalidAction = icd9InvalidActions) {
  icd9GetMajor(icd9 = icd9Short, isShort = TRUE, invalidAction = match.arg(invalidAction))
}

#' @title convert short-form ICD-9 code to decimal form
#' @description converts ICD-9 'short' form to decimal form
#' @template icd9-short
#' @template invalid
#' @family ICD-9 convert
#' @keywords manip
#' @export
icd9ShortToDecimal <- function(icd9Short, invalidAction = icd9InvalidActions) {
  # prevalidate regardless of invalidAction - TODO: ensure this is done consistently for every public entry point.
  if (class(icd9Short) != "character")
    stop("icd9Short must be a character: number values could be ambiguous if converted blindly to character")

  if (length(icd9Short) == 0) return(character()) # question whether an empty vector is valid?

  icd9Short <- icd9ValidNaWarnStopShort(icd9Short,  match.arg(invalidAction))
  parts <- icd9ShortToParts(icd9Short)
  out <- paste( parts[["major"]], ".", parts[["minor"]], sep = "") # should only be max of 6 chars...
  if (any(parts[["minor"]] == "") || is.na(parts[["minor"]])) {

    out[parts[["minor"]] == "" | is.na(parts[["minor"]])] <-
      parts[parts[["minor"]] == "" | is.na(parts[["minor"]]), "major"]
  }

  # paste inevitably makes <NA> into a string "NA", so if major was NA, then restore that:
  out[is.na(parts[["major"]])] <- NA_character_
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
icd9ShortToParts <- function(icd9Short, minorEmpty = "", invalidAction = icd9InvalidActions) {
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
  if (minorEmpty != "")
    x[!is.na(x[["minor"]]) & x[["minor"]] == "", "minor"] <- minorEmpty
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
#' @template invalid
#' @return character vector. Deliberately returns zero-padded major, because
#'   otherwise we are creating ambiguous codes (even if we know what we mean)
#' @family ICD-9 convert
#' @keywords internal
icd9PartsRecompose <- function(parts, isShort, invalidAction = icd9InvalidActions) {
  invalidAction <- match.arg(invalidAction)
  stopifnot(class(isShort) == "logical")

  sep = "."
  if (isShort) sep = ""

  stopifnot(names(parts) == c("major", "minor"))
  major <- asCharacterNoWarn(parts[["major"]]) # no factors, please. TODO: might it be okay? It would be more memory efficient with big lists.
  minor <- asCharacterNoWarn(parts[["minor"]])

  minor[is.na(minor)] <- ""

  # only allow pass through of non-zero-padded majors in short if no minor. Otherwise, major is passed through unchanged.
  if (isShort) {
    nonEmptyMinors <- minor != ""
    major[nonEmptyMinors] <- icd9AddLeadingZeroesMajor(major[nonEmptyMinors], addZeroV = TRUE, invalidAction = invalidAction)
  }

  # paste regardless of major or minor validity. If major is NA, then adding leading zeroes also gives NA.
  out <- paste(major, minor, sep = sep)
  out[is.na(major)] <- NA_character_

  # now optionally check if the result is valid, default being not to check.
  out <- icd9ValidNaWarnStop(icd9 = out, isShort = isShort, invalidAction = invalidAction)
  out
}

#' @rdname icd9PartsRecompose
#' @export
icd9PartsToShort <- function(parts, invalidAction = icd9InvalidActions)
  icd9PartsRecompose(parts = parts, isShort = TRUE, invalidAction = match.arg(invalidAction))


#' @rdname icd9PartsRecompose
#' @export
icd9PartsToDecimal <- function(parts, invalidAction = icd9InvalidActions)
  icd9PartsRecompose(parts = parts, isShort = FALSE, invalidAction = match.arg(invalidAction))

#' @rdname icd9PartsRecompose
#' @description icd9MajMinToDf simply composes the data frame needed
#'   as input to the PartsToXxxx functions
#' @export
icd9MajMinToParts <- function(major, minor)
  data.frame(major = major, minor = minor, stringsAsFactors = FALSE)

#' @rdname icd9PartsRecompose
#' @description icd9MajMinTo\{Short|Decimal\} simply composes the data frame needed
#'   as input to the PartsToXxxx functions. Having two inputs breaks the ability to 'pipe' commands together using \link{magrittr}, so passing a single \code{data.frame} is preferred.
#' @export
icd9MajMinToShort <- function(major, minor, invalidAction = icd9InvalidActions)
  icd9PartsToShort(parts = icd9MajMinToParts(major, minor), invalidAction = match.arg(invalidAction))

#' @rdname icd9PartsRecompose
#' @export
icd9MajMinToDecimal <- function(major, minor, invalidAction = icd9InvalidActions)
  icd9PartsToDecimal(parts = icd9MajMinToParts(major, minor), invalidAction = match.arg(invalidAction))

#' @title convert the chapter headings to lists of codes
#' @description the chapter headings can be converted into the full set of their
#'   children, and then used to look-up which chapter, sub-chapter, or 'major' a
#'   given code belongs.
#' @param x Either a chapter list itself, or the name of one, e.g.
#'   icd9ChaptersSub
#' @keywords internal manip
icd9ChaptersToMap <- function(x, isShort) {
  if (length(x) == 1) x <- get(x)
  ranges <- names(x)
  map <- list()
  for (r in ranges) {
    map[[r]] <- icd9ChildrenRangeShort(r)
  }
  map
}
