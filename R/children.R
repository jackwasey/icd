# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

#' Get children of ICD codes
#'
#' Expand ICD codes to all possible sub-codes, optionally limiting to those
#' codes which are \emph{defined} or \emph{billable} (leaf nodes).
#' @param x data, e.g. character vector of ICD codes.
#' @param defined single logical value, whether returned codes should only
#'   include those which have definitions. Definition is based on the ICD
#'   version being used, e.g. ICD-9-CM, the WHO version of ICD-10, or other.
#' @template billable
#' @template short_code
#' @template dotdotdot
#' @keywords manip
#' @family ICD-9 ranges
#' @examples
#' library(magrittr, warn.conflicts = FALSE, quietly = TRUE) # optional
#'
#' # no children other than self
#' children("10201", short_code = TRUE, defined = FALSE)
#'
#' # guess it was ICD-9 and a short, not decimal code
#' children("0032")
#'
#' # empty because 102.01 is not meaningful
#' children("10201", short_code = TRUE, defined = TRUE)
#' children("003", short_code = TRUE, defined = TRUE) %>%
#'   explain(condense = FALSE, short_code = TRUE)
#'
#' children(short_code = FALSE, "100.0")
#' children(short_code = FALSE, "100.00")
#' children(short_code = FALSE, "2.34")
#' @return Returns a vector of ICD codes, with class of \code{character} and the
#'   class of the identified or specified ICD code, e.g. \code{icd9}
#' @export
children <- function(x, ...)
  UseMethod("children")

#' @describeIn children Get child codes, guessing ICD version and short
#'   versus decimal format
#' @export
children.character <- function(x, ...) {
  ver <- guess_version(x)
  # eventually UseMethod again, but this would be circular until the icd10
  # method is defined.
  if (ver %in% icd9_classes)
    return(children.icd9cm(x = x, ...))
  if (ver == "icd10")
    return(children.icd10(x = x, ...))
  if (ver == "icd10cm")
    return(children.icd10cm(x = x, ...))
}

#' @describeIn children Get children of ICD-9-CM codes
#' @export
children.icd9cm <- function(x, short_code = guess_short(x),
                            defined = TRUE, billable = FALSE, ...) {
  assert(check_factor(x), check_character(x))
  assert_flag(short_code)
  assert_flag(defined)
  assert_flag(billable)
  # TODO order/unorder consistently for decimal and short
  res <- if (short_code)
    .Call("_icd_icd9ChildrenShortUnordered",
          icd9Decimal = toupper(x),
          icd9cmReal = icd::icd9cm_hierarchy$code,
          onlyReal = defined)
  else
    .Call("_icd_icd9ChildrenDecimalCpp",
          icd9Decimal = toupper(x),
          icd9cmReal = icd::icd9cm_hierarchy$code,
          onlyReal = defined)
  res <- sort_icd.icd9(res)
  res <- if (billable)
    get_billable.icd9cm(icd9cm(res), short_code)
  else
    as.icd9(res)
  if (is.icd9cm(x))
    return(as.icd9cm(res))
  res
}

#' @describeIn children Get children of ICD-9 codes, based on the superset
#'   ICD-9-CM at present
#' @export
children.icd9 <- function(x, short_code = guess_short(x),
                          defined = TRUE, billable = FALSE, ...)
  children.icd9cm(x, short_code = short_code,
                  defined = defined, billable = billable)

#' @describeIn children Get children of ICD-10 codes (warns because this
#'   only applies to ICD-10-CM for now).
#' @export
children.icd10 <- function(x, short_code = guess_short(x), defined, billable = FALSE, ...) {
  res <- children.icd10cm(x, short_code, defined, billable, ...)
  if (!is.icd10cm(x)) {
    cl <- class(res)
    cl <- cl[cl != "icd10cm"]
    class(res) <- cl
  }
  res
}

#' @describeIn children Get children of ICD-10-CM codes
#' @export
children.icd10cm <- function(x, short_code = guess_short(x), defined, billable = FALSE, ...) {
  assert(check_factor(x), check_character(unclass(x)))
  assert_flag(short_code)
  assert_flag(billable)
  if (!missing(defined) && !defined)
    stop("Finding children of anything but defined ICD-10-CM codes is current not supported.")
  res <- children_defined.icd10cm(x = x, short_code = short_code)
  if (is.icd10cm(x))
    as.icd10cm(res)
  res
}

#' defined children of ICD codes
#'
#' Find defined ICD-10 children based on 2016 ICD-10-CM list. "defined" may be a
#' three digit code, or a leaf node. This is distinct from 'billable'.
#'
#' @keywords internal
children_defined <- function(x)
  UseMethod("children_defined")

#' @describeIn children_defined Internal function to get the children of
#'   ICD-10 code(s)
#' @param warn single logical value, if \code{TRUE} will generate warnings when
#'   some input codes are not known ICD-10-CM codes
#' @param use_cpp single logical flag, whether to use C++ version
#' @export
children_defined.icd10cm <- function(x, short_code = guess_short(x), warn = FALSE) {
  assert_character(x)
  assert_flag(short_code)
  assert_flag(warn)
  x <- trim(x)
  x <- toupper(x)
  if (!short_code)
    x <- decimal_to_short.icd10cm(x)
  kids <- icd10cm_children_defined_cpp(x, icd10cm2016, .nc)
  as.icd10cm(kids, short_code)
}
