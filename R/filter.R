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

#' @title Filter ICD-9 codes by validity.
#' @description Filters a data.frame of patients for valid or invalid ICD-9 codes
#' @template icd_df
#' @template icd_name
#' @template short_code
#' @param invert single logical value, if TRUE will return invalid instead of valid rows.
#' @export
icd_filter_valid <- function(...)
  UseMethod("icd_filter_valid")

#' @rdname icd_filter_valid
#' @export
icd_filter_invalid <- function(...)
  UseMethod("icd_filter_invalid")

#' @describeIn icd_filter_valid Filter data frame for valid ICD-9 codes
#' @export
icd_filter_valid.icd9 <- function(x, icd_name = NULL,
                                  short_code =  NULL, invert = FALSE, ...) {
  assertDataFrame(x, min.cols = 1, col.names = "named")
  assert(checkNull(icd_name), checkString(icd_name))
  assert(checkNull(short_code), checkFlag(short_code))
  icd_name <- get_icd_name(x)
  if (is.null(short_code))
    short_code <- icd_guess_short(x[[icd_name]])
  assertDataFrame(x, min.cols = 1, col.names = "named")
  assertFlag(short_code)
  assertFlag(invert)
  x[icd_is_valid.icd9(x[[icd_name]], short_code = short_code) != invert, ]
}

### @describeIn icd_filter_invalid Filter in (or out) invalid ICD-9 codes

#' @rdname icd_filter_valid
#' @export
icd_filter_invalid.icd9 <- function(x, icd_name = NULL, short_code = NULL, invert = FALSE, ...) {
  icd_filter_valid.icd9(x, icd_name = icd_name, short_code = short_code, invert = !invert)
}

#' @name icd_filter
#' @title Filters data frame based on present-on-arrival flag
#' @description Present On Arrival (POA) is not a simple flag, since many codes
#'   are exempt, unspecified, or unknown. Therefore, two options are given: get
#'   all the comorbidities where the POA flag was definitely -ve, coded as "N"
#'   or definitely +ve and coded as "Y". Negating one set won't give the other
#'   set unless all codes were either Y or N.
#' @template icd_df
#' @template poa_name
#' @template poa
#' @examples
#' \dontrun{
#' # using magrittr is beautiful:
#' library("magrittr", quietly = TRUE, warn.conflicts = FALSE)
#' myData <- data.frame(
#'   visitId = c("v1", "v2", "v3", "v4"),
#'   diag = c("39891", "39790", "41791", "4401"),
#'   poa = c("Y", "N", NA, "Y"),
#'   stringsAsFactors = FALSE
#' )
#' myData %>% icd9FilterPoaNotNo() %>% icd9ComorbidAhrq
#' # can fill out named fields also:
#' myData %>% icd9FilterPoaYes(poa_name="poa") %>%
#'   icd9ComorbidAhrq(icd9Field = "diag", visitId = "visitId", isShort = TRUE)
#' # can call the core icd9Comorbid function with an arbitrary mapping
#' myData %>%
#' icd9FilterPoaYes() %>%
#' icd9Comorbid(icd9Field = "diag", visitId = "visitId",
#'   icd9Mapping = quanElixComorbid,
#'  isShortMapping = TRUE)
#' }
#' @export
icd_filter_poa <- function(x, poa_name = "poa", poa = icd9PoaChoices) {
  poa <- match.arg(poa)
  assertDataFrame(x, min.cols = 1, col.names = "named")
  assertString(poa_name)
  stopifnot(poa_name %in% names(x))
  switch(poa,
          "yes" = icd_filter_poa_yes(x, poa_name = poa_name),
          "no" = icd_filter_poa_no(x, poa_name = poa_name),
          "notYes" = icd_filter_poa_not_yes(x, poa_name = poa_name),
          "notNo" = icd_filter_poa_not_no(x, poa_name = poa_name)
  )
}

.icd_filter_poa <- function(x, poa_name, choice, negative = FALSE) {
  assertDataFrame(x, min.cols = 1, col.names = "named")
  assertString(poa_name, na.ok = FALSE)
  assertCharacter(choice, min.chars = 1, min.len = 1, any.missing = FALSE)
  assertFlag(negative)
  stopifnot(poa_name %in% names(x))
  p <- x[[poa_name]]
  if (negative)
    return(x[is.na(p) | p %nin% choice, names(x) != poa_name])
  x[!is.na(p) & p %in% choice, names(x) != poa_name]
}

#' @describeIn icd_filter Select rows where Present-on-Arrival flag is
#'   explicitly "Yes."
#' @export
icd_filter_poa_yes <- function(x, poa_name = "poa") {
  .icd_filter_poa(x, poa_name, choice = c("Y", "y"), negative = FALSE)
}

#' @describeIn icd_filter Select rows where Present-on-Arrival flag is
#'   explicitly "No."
#' @export
icd_filter_poa_no <- function(x, poa_name = "poa") {
  .icd_filter_poa(x, poa_name, choice = c("N", "n"), negative = FALSE)
}

#' @describeIn icd_filter Select rows where Present-on-Arrival flag is
#'   anything but "No." This includes unknown, exempt, other codes, and of
#'   course all those marked "Yes."
#' @export
icd_filter_poa_not_no <- function(x, poa_name = "poa") {
  .icd_filter_poa(x, poa_name, choice = c("N", "n"), negative = TRUE)
}

#' @describeIn icd_filter Select rows where Present-on-Arrival flag is
#'   anything byt "Yes." This would group exempt, unknown and other codes under
#'   "Not POA" which is unlikely to be a good choice, since exempt codes, of
#'   which there are a quite large number, tend to describe chronic or
#'   out-of-hospital characteristics.
#' @export
icd_filter_poa_not_yes <- function(x, poa_name = "poa") {
  .icd_filter_poa(x, poa_name, choice = c("Y", "y"), negative = TRUE)
}
