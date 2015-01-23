#' @title check whether any ICD-9 code is syntactically valid
#' @description This does not check whether the code coresponds to a real
#'   ICD-9-CM billing code, or parent grouping. For that, see
#'   \code{\link{icd9Real}}.
#'
#'   factors are accepted, and since the validation is done with \code{grepl}
#'   these are handled correctly.
#' @template icd9-any
#' @template isShort
#' @seealso \code{\link{icd9ValidDecimal}} and \code{\link{icd9ValidShort}}
#' @family ICD9 validation
#' @export
icd9Valid <- function(icd9, isShort)
  if (isShort) icd9ValidShort(icd9) else icd9ValidDecimal(icd9)

#' @title check whether decimal icd9 codes are valid
#' @description Check validity of 'long' (i.e. decimal form) ICD9 codes. The
#'   codes may be numeric disease descriptiors or V or E prefixed.
#' @details Long form is not ambiguous so additional zeroes are tolerated.
#'   Although integer ICD-9 codes could be correct, there is a difference
#'   between 100 and 100.0, and indeed 100.00. Therefore, character class is
#'   enforced to avoid this problem.
#' @template icd9-decimal
#' @return logical vector with T or F for each icd9 code provided according to
#'   validity
#' @seealso \url{http://www.stata.com/users/wgould/icd9/icd9.hlp}
#'   url{http://www.sascommunity.org/wiki/Validate_the_format_of_ICD-9_codes}
#' @family ICD9 validation
#' @export
icd9ValidDecimal <- function(icd9Decimal) {
  if (is.numeric(icd9Decimal)) {
    warnNumericCode()
    icd9Decimal <- as.character(icd9Decimal)
  }
  stopifnot(is.character(icd9Decimal) || is.factor(icd9Decimal))
  if (length(icd9Decimal) == 0) return(logical())

  icd9ValidDecimalN(icd9Decimal) |
    icd9ValidDecimalV(icd9Decimal) |
    icd9ValidDecimalE(icd9Decimal)
}

#' @title validate ICD-9 short form code
#' @description As in \code{icd9ValidDecimal}, character type of the
#'   input is enforced. An integer 100 could correspond to decimal ICD-9 codes
#'   1.00 10.0 or 100.
#' @template icd9-short
#' @return logical vector with T or F for each icd9 code provided according to
#'   validity
#' @family ICD9 validation
#' @seealso \url{http://www.stata.com/users/wgould/icd9/icd9.hlp}
#'   \url{http://www.sascommunity.org/wiki/Validate_the_format_of_ICD-9_codes}
#' @export
icd9ValidShort <- function(icd9Short) {

  if (length(icd9Short) == 0) {
    #warning("isValidShortICD9 expects at least one icd9 code to test")
    return(logical())
  }

  stopifnot(is.character(icd9Short) || is.factor(icd9Short))
  # this is not just invalid data: there is a programming error in the data
  # structure

  # quicker to test rather than always try to convert. Factor levels are always
  # character, so no concern about introducing ambiguity with e.g. short code of
  # 100 vs 10.0 (0100, 0010)
  if (is.factor(icd9Short)) icd9Short <- asCharacterNoWarn(icd9Short)

  # as explained in details, a numeric short ID has different validity
  # requirements than a string because of leading zeroes.
  icd9ValidShortN(icd9Short) |
    icd9ValidShortV(icd9Short) |
    icd9ValidShortE(icd9Short)
}

#' @rdname icd9ValidShort
#' @export
icd9ValidShortV <- function(icd9Short)
  grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))[[:digit:]]{0,2}[[:space:]]*$",
        icd9Short)

#' @rdname icd9ValidShort
#' @export
icd9ValidShortE <- function(icd9Short)
  grepl("^[[:space:]]*[Ee][[:digit:]]{1,4}[[:space:]]*$", icd9Short)

#' @rdname icd9ValidShort
#' @export
icd9ValidShortN <- function(icd9Short)
  grepl("^[[:space:]]*[[:digit:]]{1,5}[[:space:]]*$", icd9Short)


#' @rdname icd9ValidDecimal
#' @export
icd9ValidDecimalV <- function(icd9Decimal)
  grepl("^[[:space:]]*[Vv](([1-9][[:digit:]]?)|([[:digit:]][1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$",
        icd9Decimal)

#' @rdname icd9ValidDecimal
#' @export
icd9ValidDecimalE <- function(icd9Decimal)
  #need Perl regex for lookbehind. may even be quicker, according to the docs.
  #grepl("^E(?!0+($|\\.))[[:digit:]][[:digit:]]{0,2}(\\.[[:digit:]]?)?$",
  #trim(icd9Decimal), perl = TRUE)
  grepl("^[[:space:]]*[Ee][[:digit:]]{1,3}(\\.[[:digit:]]?)?[[:space:]]*$",
        icd9Decimal)

#' @rdname icd9ValidDecimal
#' @note TODO: icd9ValidDecimalN not quite right, since it would validate 0.12
#' @export
icd9ValidDecimalN <- function(icd9Decimal)
  grepl("^[[:space:]]*((0{1,3})|([1-9][[:digit:]]{0,2})|(0[1-9][[:digit:]]?)|(00[1-9]))(\\.[[:digit:]]{0,2})?[[:space:]]*$",
        icd9Decimal)

#' @title validate a major part
#' @description validation for just the 'major' part of an ICD-9 code. This can
#'   in fact be provided as a numeric, since there is no ambiguity. Numeric-only
#'   codes should be one to three digitis, V codes are followed by one or two
#'   digits, and E codes always by three digits between 800 and 999.
#'
#'   example/test grepl(c("", "1", "22", "333", "4444", "V", "V2", "V34",
#'   "V567", "E", "E1", "E70", "E"))
#'
#' @template major
#' @family ICD9 validation
#' @export
icd9ValidMajor <- function(major)
  grepl(
    pattern = "^[[:space:]]*([[:digit:]]{1,3}[[:space:]]*$)|([Vv][[:digit:]]{1,2}[[:space:]]*$)|([Ee][[:digit:]]{1,3}[[:space:]]*$)",
    x = major
  )


#' @rdname icd9ValidMajor
icd9ValidMajorN <- function(major)
  grepl(
    pattern = "^[[:space:]]*[[:digit:]]{1,3}[[:space:]]*$",
    x = major
  )

#' @rdname icd9ValidMajor
icd9ValidMajorV <- function(major)
  grepl(
    pattern = "^[[:space:]]*[Vv][[:digit:]]{1,2}[[:space:]]*$",
    x = major
  )

#' @rdname icd9ValidMajor
icd9ValidMajorE <- function(major)
  grepl(
    pattern = "^[[:space:]]*[Ee][[:digit:]]{1,3}[[:space:]]*$",
    x = major
  )

#' @title validate an icd9 mapping to comorbidities
#' @description just takes each item in each vector of the list of vectors and
#'   checks validity
#' @template mapping
#' @template isShort
#' @family ICD9 validation
#' @export
icd9ValidMapping <- function(icd9Mapping, isShort)
  # TOOD: warn/return the invalid labels
  all(unlist(
    lapply(icd9Mapping, FUN = function(icd9Map) icd9Valid(icd9Map, isShort)),
    use.names = FALSE
  ))

#' @rdname icd9ValidMapping
#' @export
icd9ValidMappingShort <- function(icd9Mapping)
  icd9ValidMapping(icd9Mapping, TRUE)

#' @rdname icd9ValidMapping
#' @export
icd9ValidMappingDecimal <- function(icd9Mapping)
  icd9ValidMapping(icd9Mapping, FALSE)

#' @rdname icd9ValidMapping
#' @export
icd9GetInvalidMappingShort <- function(icd9Mapping) {
  x <- lapply(icd9Mapping, FUN = icd9GetInvalidShort)
  x[lapply(x, length) > 0]
}

#' @rdname icd9ValidMapping
#' @export
icd9GetInvalidMappingDecimal <- function(icd9Mapping) {
  x <- lapply(icd9Mapping, FUN = icd9GetInvalidDecimal)
  x[lapply(x, length) > 0]
}

#' @title invalid subset of decimal or short ICD-9 codes
#' @description given vector of short or decimal ICD-9 codes in
#'   \code{icd9Decimal} or \code{icd9Short}, return (in the same format) those
#'   codes which are invalid. Useful for generating error messages with the
#'   faulty codes if validation fails.
#' @template icd9-decimal
#' @keywords manip
#' @family ICD-9 validation
#' @export
icd9GetInvalidDecimal <- function(icd9Decimal)
  icd9Decimal[!icd9ValidDecimal(icd9Decimal)]

#' @rdname icd9GetInvalidDecimal
#' @template icd9-short
#' @export
icd9GetInvalidShort <- function(icd9Short)
  icd9Short[!icd9ValidShort(icd9Short)]

icd9IsMajor <- function(icd9)
  nchar(icd9) - icd9IsE(icd9) < 4

#' @title Does ICD-9 code exist
#' @description This is different from syntactic validity: it looks it up in the
#'   list of icd9 codes. This may have been easier all along, but checking
#'   syntactic validity still very useful, with a changing list of icd-9 codes
#'   over time, and possibly imperfections in the master list derived from CMS.
#' @template icd9-any
#' @template isShort
#' @return logical vector
#' @export
icd9IsReal <- function(icd9, isShort, majorOk = TRUE) {
  if (isShort) return(icd9IsRealShort(icd9, majorOk = majorOk))
  icd9IsRealDecimal(icd9, majorOk = majorOk)
}

#' @rdname icd9IsReal
#' @template icd9-short
#' @export
icd9IsRealShort <- function(icd9Short, majorOk = TRUE) {
  if (majorOk)
    return(icd9Short %in% c(icd9::icd9Hierarchy[["icd9"]],
                            icd9::icd9ChaptersMajor))
  icd9Short %in% icd9::icd9Hierarchy[["icd9"]]

}

#' @rdname icd9IsReal
#' @template icd9-decimal
#' @export
icd9IsRealDecimal <- function(icd9Decimal, majorOk = TRUE) {
  icd9IsRealShort(icd9DecimalToShort(icd9Decimal), majorOk = majorOk)
}

#' @rdname icd9IsReal
#' @template icd9-any
#' @export
icd9GetReal <- function(icd9, isShort, majorOk = TRUE) {
  if (isShort) return(icd9GetRealShort(icd9, majorOk = majorOk))
  icd9GetRealDecimal(icd9, majorOk = majorOk)
}

#' @rdname icd9IsReal
#' @template icd9-short
#' @export
icd9GetRealShort <- function(icd9Short, majorOk = TRUE)
  icd9Short[icd9IsRealShort(icd9Short, majorOk = majorOk)]

#' @rdname icd9IsReal
#' @template icd9-decimal
#' @export
icd9GetRealDecimal <- function(icd9Decimal, majorOk = TRUE)
  icd9Decimal[icd9IsRealDecimal(icd9Decimal, majorOk = majorOk)]

warnNumericCode <- function()
  warning("input data is in numeric format. This can easily lead to errors in short or decimal codes, e.g. short code 1000: is it 10.00 or 100.0; or decimal codes, e.g. 10.1 was supposed to be 10.10", call. = FALSE)
