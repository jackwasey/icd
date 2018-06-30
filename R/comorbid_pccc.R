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

globalVariables(c("code_name", "icd10_map_pccc_dx", "icd9_map_pccc_dx",
                  "icd9_map_pccc_pcs", "icd10_map_pccc_pcs"))

#' Calculate pediatric complex chronic conditions (PCCC) comorbidities
#'
#' Unlike with ICD-9 and ICD-10 diagnostic codes, 'icd' doesn't
#' currently have a method for guessing which fields are procedure
#' codes, so \code{icd_name} must be specified for the \code{_pcs}
#' functions.
#' @inheritParams comorbid
#' @examples
#' # not pediatric data, but let's look for this example
#' head(icd9_comorbid_pccc_dx(wide_to_long(vermont_dx)))
#' @export
comorbid_pccc_dx <- function(x, visit_name = get_visit_name(x),
                             icd_name = get_icd_name(x),
                             short_code = guess_short(x, icd_name = icd_name),
                             return_df = FALSE, return_binary = FALSE, ...)
  switch_ver_cmb(x = x, funs = list(icd9 = icd9_comorbid_pccc_dx,
                                    icd10 = icd10_comorbid_pccc_dx),
                 visit_name = visit_name, icd_name = icd_name,
                 short_code = short_code, return_df = return_df,
                 return_binary = return_binary, ...)

#' @describeIn comorbid_pccc_dx Calculate the PCCC comorbidities based
#'   on procedure codes,
#'   @examples
#' # Six random codes from each PCCC procedure code map. 'icd' will use
#' # an heuristic to guess whether ICD-9 or ICD-10:
#' pts <- data.frame(encounters = c(10, 11, 12),
#'                   icd9_pcs = c("0152", "304", "0050"),
#'                   icd10_pcs = c("0B110Z4", "02YA0Z2", "031209D"))
#' comorbid_pccc_pcs(pts, icd_name = "icd9_pcs", return_binary = TRUE)
#' comorbid_pccc_pcs(pts, icd_name = "icd10_pcs", return_binary = TRUE)
#'
#' # All ICD-9 procedure codes are numeric, some ICD-10 procedure codes
#' # are numeric, so best to call functions directly:
#' pts <- data.frame(encounters = c(100), icd10_pcs = c("0016070"))
#' icd10_comorbid_pccc_pcs(pts, icd_name = "icd10_pcs")
#' @family comorbidity computations
#' @family comorbidities
#' @export
comorbid_pccc_pcs <- function(x, visit_name = get_visit_name(x),
                              icd_name,
                              return_df = FALSE,
                              return_binary = FALSE, ...) {
  stopifnot(visit_name %in% names(x), icd_name %in% names(x))
  n <- min(n, length(x[[icd_name]]))
  test_some <- x[seq_len(n), icd_name]
  nines_tens <- grepl('.*[A-Za-z].*', as.character(test_some), ignore.case = TRUE)
  is_icd9 = TRUE
  threshold = 0.7
  if (sum(nines_tens) / n > threshold)
    is_icd9 = FALSE
  else if (sum(nines_tens) / n > (1 - threshold))
    warning("many invalid ICD-9 procedure codes, but not enough to ",
            "determine that all the codes were ICD-10 codes. Please",
            "check the input data. Assuming ICD-9.")
  if (is_icd9)
    icd9_comorbid_pccc_pcs(x,
                            visit_name = visit_name,
                            icd_name = icd_name,
                            return_df = return_df,
                            return_binary = return_binary, ...)
  else
    icd10_comorbid_pccc_pcs(x,
                            visit_name = visit_name,
                            icd_name = icd_name,
                            return_df = return_df,
                            return_binary = return_binary, ...)
}

#' @describeIn comorbid_pccc_dx Calculate PCCC comorbidities from ICD-9
#'   diagnosis codes
#' @export
icd9_comorbid_pccc_dx <-
  function(x, visit_name = NULL, icd_name = NULL,
           short_code = guess_short(x, icd_name = icd_name),
           return_df = FALSE, return_binary = FALSE, ...)
    icd9_comorbid(x = x,
                  map = icd9_map_pccc_dx,
                  visit_name = visit_name,
                  icd_name = icd_name,
                  short_code = short_code,
                  short_map = TRUE,
                  return_df = return_df,
                  return_binary = return_binary,
                  ...)

#' @describeIn comorbid_pccc_dx Calculate PCCC comorbidities from ICD-10
#'   diagnosis codes
#' @export
icd10_comorbid_pccc_dx <-
  function(x, visit_name = NULL, icd_name = NULL,
           short_code = guess_short(x, icd_name = icd_name),
           return_df = FALSE, return_binary = FALSE, ...)
    icd10_comorbid(x = x,
                   map = icd10_map_pccc_dx,
                   visit_name = visit_name,
                   icd_name = icd_name,
                   short_code = short_code,
                   short_map = TRUE,
                   return_df = return_df,
                   return_binary = return_binary,
                   ...)

#' @describeIn comorbid_pccc_dx Calculate PCCC comorbidities from ICD-9
#'   procedure codes
#' @export
icd9_comorbid_pccc_pcs <- function(x, visit_name = get_visit_name(x), icd_name,
                                   return_df = FALSE,
                                   return_binary = FALSE, ...)
  categorize_simple(x = x,
                    map = icd9_map_pccc_pcs,
                    id_name = visit_name, code_name = icd_name,
                    return_df = return_df,
                    return_binary = return_binary, ...)

#' @describeIn comorbid_pccc_dx Calculate PCCC comorbidities from ICD-10
#'   procedure codes
#' @export
icd10_comorbid_pccc_pcs <- function(x, visit_name = get_visit_name(x),
                                    icd_name, return_df = FALSE,
                                    return_binary = FALSE, ...)
  categorize_simple(x = x,
                    map = icd10_map_pccc_pcs,
                    id_name = visit_name, code_name = icd_name,
                    return_df = return_df,
                    return_binary = return_binary, ...)
