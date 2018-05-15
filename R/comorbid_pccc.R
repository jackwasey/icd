
#' Calculate pediatric complex chronic conditions (PCCC) comorbidities
#'
#' @inheritParams comorbid
#' @examples
#' # not pediatric data, but let's look for this example
#' head(icd9_comorbid_pccc_dx(wide_to_long(vermont_dx)))
#' @export
comorbid_pccc_dx <- function(x, visit_name = get_visit_name(x),
                             icd_name = get_icd_name(x),
                             short_code = guess_short(x, icd_name = icd_name),
                             return_df = FALSE, return_binary = FALSE, ...) {
  switch_ver_cmb(x = x, funs = list(icd9 = icd9_comorbid_pccc_dx,
                                    icd10 = icd10_comorbid_pccc_dx),
                 visit_name = visit_name, icd_name = icd_name,
                 short_code = short_code, return_df = return_df,
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
                  map = icd::icd9_map_pccc_dx,
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
                   map = icd::icd10_map_pccc_dx,
                   visit_name = visit_name,
                   icd_name = icd_name,
                   short_code = short_code,
                   short_map = TRUE,
                   return_df = return_df,
                   return_binary = return_binary,
                   ...)
