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

utils::globalVariables("icd10cm2016")

#' real icd10 children based on 2016 list
#' @importFrom stringr str_trim
icd10ChildrenRealShort <- function(icd10Short) {

  assertCharacter(icd10Short)

  icd10Short <- stringr::str_trim(icd10Short)

  matches_bool <- icd10Short %in% icd9::icd10cm2016[["code"]]
  # if the codes are not in the source file, we ignore, warn, drop silently?
  if (!all(matches_bool)) warning("some values did not match any ICD-10-CM codes: ",
                                  paste(icd10Short[!matches_bool], collapse = ", "))

  icd10Short <- icd10Short[matches_bool]
  matches <- match(icd10Short, icd9::icd10cm2016[["code"]])
  last_row <- nrow(icd9::icd10cm2016)

  nc <- nchar(icd9::icd10cm2016[["code"]]) # TODO: pre-compute and save in package data

  kids <- character(0)

    if (length(icd10Short) == 0 ) {
    warning("none of the provided ICD-10 codes matched the canonical list")
    return(kids)
  }

  for (i in seq_along(icd10Short)) {
    # now the children, assuming the source file is sorted logically, will be subsequent codes, until a code of the same length is found
    check_row <- matches[i] + 1
    parent_len <- nc[matches[i]]
    while (nc[check_row] > parent_len && check_row != last_row + 1)
      check_row <- check_row + 1

    kids <- c(kids, icd9::icd10cm2016[matches[i]:(check_row - 1), "code"])
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

icd10ExpandRangeRealShort <- function(start, end) {
  assertScalar(start) # i'll permit numeric but prefer char
  assertScalar(end)

  # check whether valid?
  # check whether real?
  # minimal check for type:
  stopifnot(grepl("[^.]", c(start, end)))

  # for ranges, we can semi-legitimately strip any ".x" part ( but other X values CAN appear later). Quan uses x in position 4, but I'm not aware of any ICD-10 code that does this.

  # deliberately not case sensitive, as this is a Quan quirk:
  if (substr(start, 4, 100) == "x")
    start <- substr(start, 1, 3)

  if (substr(end, 4, 100) == "x")
    end <- substr(end, 1, 3)

  # TODO: either search down supposedly well ordered list until substring of end
  # changes, or find all children, and get pos of last one.

  end_kids <- icd10ChildrenRealShort(end)
  new_end <- end_kids[length(end_kids)]

  # find the start and end code positions in the master list
  pos <- match(c(start, new_end), icd9::icd10cm2016[["code"]])
  if (is.na(pos[1])) stop(sprintf("start code '%s' not found", pos[1]))
  if (is.na(pos[2])) stop(sprintf("calculated end code '%s' not found", pos[2]))
  stopifnot(pos[2] >= pos[1])

  icd9::icd10cm2016[pos[1]:pos[2], "code"]
}

# WIP
icd10ExpandRangePossibleShort <- function(start, end) {
  assertScalar(start) # i'll permit numeric but prefer char
  assertScalar(end)
  # check whether valid?
  # check whether real?


  start <- stringr::str_trim(start)
  end <- stringr::str_trim(end)

  start_char <- substr(start, 1, 1)
  end_char <- substr(end, 1, 1)
  stopifnot(start_char <= end_char)

  start_two_digits <- as.integer(substr(start, 2, 3))
  end_two_digits <- as.integer(substr(end, 2, 3))
  if (start_char == end_char)
    stopifnot(start_two_digits <= end_two_digits)

  start_other_chars <- substr(start, 4, 10)
  end_other_chars <- substr(end, 4, 10)

}
