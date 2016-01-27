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

#' Get children of ICD codes
#'
#' Expand ICD codes to all possible sub-codes, optionally limiting to those
#' codes which are \emph{defined} or \emph{billable} (leaf nodes).
#' @param x data, e.g. character vector of ICD codes.
#' @param defined single logical value, whether returned codes should only
#'   include those which have definitions. Definition is based on the ICD
#'   version being used, e.g. ICD-9-CM, the WHO version of ICD-10, or other.
#' @param billable single logical value, whether to limit return codes also by
#'   whether they are billable, i.e. leaf nodes. This is really only designed
#'   for use with ICD-9-CM, iCD-10-CM etc, since the WHO versions are not
#'   designed for billing, but for public health and death reporting.
#' @template short_code
#' @template dotdotdot
#' @keywords manip
#' @family ICD-9 ranges
#' @examples
#' library(magrittr)
#'
#' # no children other than self
#' icd_children("10201", short_code = TRUE, defined =FALSE)
#'
#' # guess it was ICD-9 and a short, not decimal code
#' icd_children("0032", FALSE)
#'
#' # empty because 102.01 is not meaningful
#' icd_children("10201", short_code = TRUE, defined =TRUE)
#' icd_children("003", short_code = TRUE, defined =TRUE) %>%
#'   icd_explain(condense = FALSE, short_code = TRUE)
#'
#' icd_children(short_code = FALSE, "100.0")
#' icd_children(short_code = FALSE, "100.00")
#' icd_children(short_code = FALSE, "2.34")
#' @export
icd_children <- function(x, ...)
  UseMethod("icd_children")

#' @describeIn icd_children Get child codes, guessing ICD version and short versus decimal format
#' @export
icd_children.character <- function(x, ...) {
  ver <- icd_guess_version(x)
  switch(ver,
    "icd9" = icd_children.icd9(x = x, ...),
    "icd10" = stop("icd_children.icd10 not implemented yet"),
    # "icd10" = icd_children.icd10(x, short_code, ...)
    NULL)
}

#' @describeIn icd_children Get children of ICD-9 codes
#' @export
icd_children.icd9 <- function(x, short_code = icd_guess_short(x),
                         defined = TRUE, billable = FALSE, ...) {
  assert(checkFactor(x), checkCharacter(x)) # assertFactorOrCharacter(x)
  assertFlag(short_code)
  assertFlag(defined)
  assertFlag(billable)

  if (short_code)
    res <- .Call("icd9_icd9ChildrenShortCpp", PACKAGE = get_pkg_name(), toupper(x), defined)
  else
    res <- .Call("icd9_icd9ChildrenDecimalCpp", PACKAGE = get_pkg_name(), toupper(x), defined)

  res <- icd_guess_short_update(res)

  if (billable)
    icd9GetBillable(icd9cm(res), short_code)
  else
    icd9(res)
}

# this is just lazy package data, but apparently need to declare it to keep CRAN
# happy.
utils::globalVariables("icd10cm2016")

#' defined children of ICD codes
#'
#' defined icd10 children based on 2016 ICD-10-CM list. "defined" may be a three digit
#' code, or a leaf node. This is distinct from 'billable'.
#'
#' @keywords internal
icd_children_defined <- function(x)
  UseMethod("icd_children_defined")

#' @describeIn icd_children_defined get the children of ICD-10 code(s)
#' @export
#' @keywords internal
icd_children_defined.icd10cm <- function(x, short_code = icd_guess_short(x)) {

  assertCharacter(x)
  assertFlag(short_code)

  if (inherits(x, "icd10") && !inherits(x, "icd10cm"))
    warning("This function primarily gives 'defined' child codes for ICD-10-CM,
            which is mostly a superset of ICD-10 WHO")

  icd10Short <- str_trim(x)

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

