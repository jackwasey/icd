
comorbid_pccc_dx <- function(...) {
  switch_ver_cmb(x, list(icd9 = icd9_comorbid_pccc_dx,
                         icd10 = icd10_comorbid_pccc_dx), ...)
}

#' Calculate pediatric complex chronic conditions (PCCC) comorbidities
#' @examples
#' # not pediatric data, but let's look for this example
#' head(icd9_comorbid_pccc_dx(wide_to_long(vermont_dx)))
#' @export
icd9_comorbid_pccc_dx <- function(x,
                                  visit_name = NULL,
                                  icd_name = NULL,
                                  short_code = guess_short(x, icd_name = icd_name),
                                  return_df = FALSE, ...) {
  i9map <- if (!pccc_orig_maps) icd::icd9_map_pccc_dx else icd::icd9_map_pccc_orig_dx
  i9fixed <- if (!pccc_orig_maps) icd::icd9_map_pccc_fixed_dx else icd::icd9_map_pccc_orig_fixed_dx
  not_fixed <- icd9_comorbid(x = x,
                             map = i9map,
                             visit_name = visit_name,
                             icd_name = icd_name,
                             short_code = short_code,
                             short_map = TRUE,
                             return_df = FALSE,
                             ...)
  fixed <- icd9_comorbid(x = x,
                         map = i9fixed,
                         visit_name = visit_name,
                         icd_name = icd_name,
                         short_code = short_code,
                         short_map = TRUE,
                         return_df = FALSE,
                         ...)
  not_fixed[, colnames(fixed)] <- not_fixed[, colnames(fixed)] | fixed
  if (return_df)
    comorbid_mat_to_df(not_fixed)
  else
    not_fixed
}
