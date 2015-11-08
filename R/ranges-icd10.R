
utils::globalVariables("icd10cm2016")

#' real icd10 children based on 2016 list
#' @importFrom stringr str_trim
icd10ChildrenRealShort <- function(icd10Short) {

  assertCharacter(icd10Short)

  icd10Short <- stringr::str_trim(icd10Short)

  # if the codes are not in the source file, we ignore, warn, drop silently?
  matches <- match(icd10Short, icd10cm2016[["code"]])
  icd10Short <- icd10Short[matches]
  last_row <- nrow(icd10cm2016)

  nc <- nchar(icd10cm2016[["code"]]) # TODO: pre-compute and save in package data

  kids <- character(0)

    if (length(icd10Short) == 0 ) {
    warning("none of the provided ICD-10 codes matched the canonical list")
    return(kids)
  }

  for (i in seq_along(icd10Short)) {
    # now the children, assuming the source file is sorted logically, will be subsequent codes, until a code of the same length is found
    i_len <- nchar(icd10Short[i])
    check_row <- matches[i] + 1
    parent_len <- nc[matches[i]]
    while (nc[check_row] > parent_len && check_row != last_row + 1)
      check_row <- check_row + 1

    kids <- c(kids, icd10cm2016[matches[i]:(check_row - 1), "code"])
  }
  kids

}

#' Generate possible children of given ICD-10 codes
#'
#' ultimately, this will have to be coding-scheme dependent, e.g. ICD-10-CM for
#' USA, vs various national or international schemes
#'
#' @details This is inefficient due to the large number of combinations of
#'   possible codes. I've already limited the scope to letters which appear at
#'   certain positions in ICD-10-CM 2016. Maybe best just to limit to 'real'
#'   codes, and then, when a user gives a squiffy code, e.g. a known code with
#'   an additional unknown value, we can: first account for all known codes in a
#'   list, then for unknown codes, partial match from left for known codes, and
#'   if we match more than one, then we take the longest matching known code.
#'
#'   presumption is that we start with at least a major level code. We don't
#'   extrapolate from A10 to A10-19 for example. regex pattern for ICD-10 is
#'   [[:alpha:]][[:digit:]][[:alnum]]{1,5} (not case sensitive)
#'
#'   can have x as placeholder: ignoring it will have same effect if making
#'   children
#'
#'   there are very many alphanumeric values possible in last 4 digits (26^4 =
#'   456976) however, hardly any alphas are used.
#'
#' @param icd910Short character vector of ICD-10 codes
#' @import checkmate
#' @export
icd10ChildrenPossibleShort <- function(icd10Short) {
  checkmate::assertCharacter(icd10Short)

  fourth <- unlist(strsplit("0123456789ABCDEFGHIJKXZ", ""))
  fifth  <- unlist(strsplit("0123456789AXYZ", ""))
  sixth  <- unlist(strsplit("0123456789X", ""))
  seventh  <- unlist(strsplit("0123459ABCDEFGHJKMNPQRS", ""))

  minor_chars <- list(fourth, fifth, sixth, seventh)
  minor_chars <- lapply(minor_chars, append, "")

  # combining using 'outer' and 'paste'
  # as.vector(outer(fourth, fifth, paste, sep=""))

  # minimum code length is 3: i.e. [[:alpha:]][[:digit:]][[:alnum]]
  nc <- nchar(icd10Short)
  out_complete <- icd10Short[nc == 7]

  icd10Short <- icd10Short[nc >= 3 & nc < 7] # TODO: strip whitespace first if expedient
  if (length(icd10Short) == 0)
    return(out_complete)

  out <- character(0)
  for (i in icd10Short) {
    o <- i
    n <- nchar(i)
    for (j in n:6)
      o <- as.vector(outer(o, minor_chars[[j - 2]], paste, sep = ""))

    out <- c(out, o)
  }

  sort(c(out_complete, out))


}
