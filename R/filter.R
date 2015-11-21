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

#' @describeIn icd_filter_valid Filter data frame for valid ICD-9 codes
#' @export
icd_filter_valid.icd9 <- function(icd_df, icd_name = NULL,
                                  short_code =  NULL, invert = FALSE) {
  assertDataFrame(icd_df, min.cols = 1, col.names = "named")
  icd_name <- get_icd_name(icd_df)
  if (is.null(short_code)) isShort <- icd_guess_short(icd_df[[icd_name]])
  assertDataFrame(icd_df, min.cols = 1, col.names = "named")
  assertFlag(isShort)
  assertFlag(invert)
  v <- icd_is_valid.icd9(icd = icd_df[[icd_name]], short_code = short_code) != invert
  icd_df[v, ]
}

#' @title Filter ICD-9 codes by invalidity.
#' @description Filters a data.frame of patients for valid or invalid ICD-9 codes
#' @details Unlike \code{icd_filter_valid} this is not an S3 function
#' @template icd_df
#' @template icd_name
#' @template short_code
#' @param invert single logical value, if TRUE will return valid instead of invalid rows.
#' @export
icd_filter_invalid <- function(icd_df, icd_name = NULL, short_code = NULL, invert = FALSE) {
  icd_filter_valid(icd_df = icd_df, icd_name = icd_name, short_code = short_code, invert = !invert)
}

#' @name icd9FilterPoa
#' @title Filters data frame based on present-on-arrival flag
#' @description Present On Arrival (POA) is not a simple flag, since many codes
#'   are exempt, unspecified, or unknown. Therefore, two options are given: get
#'   all the comorbidities where the POA flag was definitely -ve, coded as "N"
#'   or definitely +ve and coded as "Y". Negating one set won't give the other
#'   set unless all codes were either Y or N. #describeIn icd9Comorbid
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
icd_filter_poa <- function(icd_df, poa_name = "poa", poa = icd9PoaChoices) {
  poa <- match.arg(poa)
  assertDataFrame(icd_df, min.cols = 1, col.names = "named")
  assertString(poa_name)
  stopifnot(poa_name %in% names(icd_df))
  switch(poa,
          "yes" = icd_filter_poa_yes(icd_df, poa_name = poa_name),
          "no" = icd_filter_poa_no(icd_df, poa_name = poa_name),
          "notYes" = icd_filter_poa_not_yes(icd_df, poa_name = poa_name),
          "notNo" = icd_filter_poa_not_no(icd_df, poa_name = poa_name)
  )
}

.icd_filter_poa <- function(icd_df, poa_name, choice, negative = FALSE) {
  assertDataFrame(icd_df, min.cols = 1, col.names = "named")
  assertString(poa_name, na.ok = FALSE)
  assertCharacter(choice, min.chars = 1, min.len = 1, any.missing = FALSE)
  assertFlag(negative)
  stopifnot(poa_name %in% names(icd_df))
  p <- icd_df[[poa_name]]
  if (negative)
    return(icd_df[is.na(p) | p %nin% choice, names(icd_df) != poa_name])
  icd_df[!is.na(p) & p %in% choice, names(icd_df) != poa_name]
}

#' @describeIn icd9FilterPoa Select rows where Present-on-Arrival flag is
#'   explicitly "Yes."
#' @export
icd_filter_poa_yes <- function(icd_df, poa_name = "poa") {
  .icd_filter_poa(icd_df, poa_name, choice = c("Y", "y"), negative = FALSE)
}

#' @describeIn icd9FilterPoa Select rows where Present-on-Arrival flag is
#'   explicitly "No."
#' @export
icd_filter_poa_no <- function(icd_df, poa_name = "poa") {
  .icd_filter_poa(icd_df, poa_name, choice = c("N", "n"), negative = FALSE)
}

#' @describeIn icd9FilterPoa Select rows where Present-on-Arrival flag is
#'   anything but "No." This includes unknown, exempt, other codes, and of
#'   course all those marked "Yes."
#' @export
icd_filter_poa_not_no <- function(icd_df, poa_name = "poa") {
  .icd_filter_poa(icd_df, poa_name, choice = c("N", "n"), negative = TRUE)
}

#' @describeIn icd9FilterPoa Select rows where Present-on-Arrival flag is
#'   anything byt "Yes." This would group exempt, unknown and other codes under
#'   "Not POA" which is unlikely to be a good choice, since exempt codes, of
#'   which there are a quite large number, tend to describe chronic or
#'   out-of-hospital characteristics.
#' @export
icd_filter_poa_not_yes <- function(icd_df, poa_name = "poa") {
  .icd_filter_poa(icd_df, poa_name, choice = c("Y", "y"), negative = TRUE)
}


# deprecated functions:

#' @export
icd9FilterValid <- function(icd9df, icd9Field = NULL,
                            isShort =  NULL, invert = FALSE) {
  .Deprecated("icd_filter_valid")
  icd_filter_valid.icd9(icd_df = icd9df, icd_name = icd9Field, short_code = isShort, invert = invert)
}

#' @export
icd9FilterInvalid <- function(icd9df, icd9Field = NULL, isShort = NULL, invert = FALSE) {
  .Deprecated("icd_filter_valid")
  icd_filter_valid.icd9(icd_df = icd9df, icd_name = icd9Field, short_code = isShort, invert = !invert)
}

#' @export
icd9FilterPoa <- function(icd9df, poaField = "poa", poa = icd9PoaChoices) {
  .Deprecated("icd_filter_poa")
  icd_filter_poa(icd_df = icd9df, poa_name = poaField, poa = poa)
}

#' @describeIn icd9FilterPoa Select rows where Present-on-Arrival flag is
#'   explicitly "Yes."
#' @export
icd9FilterPoaYes <- function(icd9df, poaField = "poa") {
  .Deprecated("icd_filter_poa_yes")
  icd_filter_poa_yes(icd_df = icd9df, poa_name = poaField)
}

#' @describeIn icd9FilterPoa Select rows where Present-on-Arrival flag is
#'   explicitly "No."
#' @export
icd9FilterPoaNo <- function(icd9df, poaField = "poa") {
  .Deprecated("icd_filter_poa_no")
  icd_filter_poa_no(icd_df = icd9df, poa_name = poaField)
}

#' @export
icd9FilterPoaNotNo <- function(icd9df, poaField = "poa") {
  .Deprecated("icd_filter_poa_not_no")
  icd_filter_poa_not_no(icd_df = icd9df, poa_name = poaField)
}

#' @export
icd9FilterPoaNotYes <- function(icd9df, poaField = "poa") {
  .Deprecated("icd_filter_poa_not_yes")
  icd_filter_poa_not_yes(icd_df = icd9df, poa_name = poaField)
}
