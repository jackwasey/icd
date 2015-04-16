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
#' @template icd9df
#' @template icd9field
#' @template isShort
#' @param invert single logical value, if TRUE will return invalid instead of valid rows.
#' @export
icd9FilterValid <- function(icd9df, icd9Field = NULL,
                            isShort =  NULL, invert = FALSE) {
  icd9Field <- getIcdField(icd9df, icd9Field)
  if (is.null(isShort)) isShort <- icd9GuessIsShort(icd9df[[icd9Field]])
  assertDataFrame(icd9df, min.cols = 1, col.names = "named")
  assertFlag(isShort)
  assertFlag(invert)
  v <- icd9IsValid(icd9 = icd9df[[icd9Field]], isShort = isShort) != invert
  icd9df[v, ]
}

#' @title Filter ICD-9 codes by invalidity.
#' @description Filters a data.frame of patients for valid or invalid ICD-9 codes
#' @template icd9df
#' @template icd9field
#' @template isShort
#' @param invert single logical value, if TRUE will return valid instead of invalid rows.
#' @export
icd9FilterInvalid <- function(icd9df, icd9Field = NULL, isShort = NULL, invert = FALSE)
  icd9FilterValid(icd9df, icd9Field, isShort, invert = !invert)


#' @name icd9FilterPoa
#' @title Filters data frame based on present-on-arrival flag
#' @description Present On Arrival (POA) is not a simple flag, since many codes
#'   are exempt, unspecified, or unknown. Therefore, two options are given: get
#'   all the comorbidities where the POA flag was definitely -ve, coded as "N"
#'   or definitely +ve and coded as "Y". Negating one set won't give the other
#'   set unless all codes were either Y or N. #describeIn icd9Comorbid
#' @template icd9df
#' @template poaField
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
#' myData %>% icd9FilterPoaYes(poaField="poa") %>%
#'   icd9ComorbidAhrq(icd9Field = "diag", visitId = "visitId", isShort = TRUE)
#' # can call the core icd9Comorbid function with an arbitrary mapping
#' myData %>%
#' icd9FilterPoaYes() %>%
#' icd9Comorbid(icd9Field = "diag", visitId = "visitId",
#'   icd9Mapping = quanElixComorbid,
#'  isShortMapping = TRUE)
#' }
#' @export
icd9FilterPoa <- function(icd9df, poaField = "poa", poa = icd9PoaChoices) {
  poa <- match.arg(poa)
  assertDataFrame(icd9df, min.cols = 1, col.names = "named")
  assertString(poaField, na.ok = FALSE)
  stopifnot(poaField %in% names(icd9df))
  if (poa == "yes") return(icd9FilterPoaYes(icd9df, poaField = poaField))
  if (poa == "no") return(icd9FilterPoaNo(icd9df, poaField = poaField))
  if (poa == "notYes") return(icd9FilterPoaNotYes(icd9df, poaField = poaField))
  #if (poa == "notNo")
  icd9FilterPoaNotNo(icd9df, poaField = poaField)
}

.icd9FilterPoa <- function(icd9df, poaField, choice, negative = FALSE) {
  assertDataFrame(icd9df, min.cols = 1, col.names = "named")
  assertString(poaField, na.ok = FALSE)
  assertCharacter(choice, min.chars = 1, min.len = 1, any.missing = FALSE)
  assertFlag(negative)
  stopifnot(poaField %in% names(icd9df))
  p <- icd9df[[poaField]]
  if (negative)
    return(icd9df[is.na(p) | p %nin% choice, names(icd9df) != poaField])
  icd9df[!is.na(p) & p %in% choice, names(icd9df) != poaField]
}

#' @describeIn icd9FilterPoa Select rows where Present-on-Arrival flag is
#'   explicitly "Yes."
#' @export
icd9FilterPoaYes <- function(icd9df, poaField = "poa")
  .icd9FilterPoa(icd9df, poaField, choice = c("Y", "y"), negative = FALSE)

#' @describeIn icd9FilterPoa Select rows where Present-on-Arrival flag is
#'   explicitly "No."
#' @export
icd9FilterPoaNo <- function(icd9df, poaField = "poa")
  .icd9FilterPoa(icd9df, poaField, choice = c("N", "n"), negative = FALSE)

#' @describeIn icd9FilterPoa Select rows where Present-on-Arrival flag is
#'   anything but "No." This includes unknown, exempt, other codes, and of
#'   course all those marked "Yes."
#' @export
icd9FilterPoaNotNo <- function(icd9df, poaField = "poa")
  .icd9FilterPoa(icd9df, poaField, choice = c("N", "n"), negative = TRUE)

#' @describeIn icd9FilterPoa Select rows where Present-on-Arrival flag is
#'   anything byt "Yes." This would group exempt, unknown and other codes under
#'   "Not POA" which is unlikely to be a good choice, since exempt codes, of
#'   which there are a quite large number, tend to describe chronic or
#'   out-of-hospital characteristics.
#' @export
icd9FilterPoaNotYes <- function(icd9df, poaField = "poa")
  .icd9FilterPoa(icd9df, poaField, choice = c("Y", "y"), negative = TRUE)
