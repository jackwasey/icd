#' Generate deprecated data
#'
#' Unfortunately, this is easiest (and has a chance of passing CRAN checks) if I
#' just duplicate data rather than trying to change the namespace
#' @param save_data save the data in \code{data/deprecated.RData}, default is
#'   \code{TRUE}
#' @keywords internal
generate_deprecated_data <- function(save_data = FALSE) {

  # no particular reason to use assign here; relic of having been trying to
  # assign to package namespace.
  icd9Billable <- icd::icd9cm_billable
  icd9Hierarchy <- icd::icd9cm_hierarchy
  names(icd9Hierarchy)[1] <- "icd9"
  ahrqComorbid <- icd::icd9_map_ahrq
  quanDeyoComorbid <- icd::icd9_map_quan_deyo
  quanElixComorbid <- icd::icd9_map_quan_elix
  elixComorbid <- icd::icd9_map_elix
  elixComorbidNames <- icd::icd_names_elix
  elixComorbidNamesAbbrev <- icd::icd_names_elix_abbrev
  elixComorbidNamesHtn <- icd::icd_names_elix_htn
  elixComorbidNamesHtnAbbrev <- icd::icd_names_elix_htn_abbrev
  quanElixComorbidNames <- icd::icd_names_quan_elix
  quanElixComorbidNamesAbbrev <- icd::icd_names_quan_elix_abbrev
  quanElixComorbidNamesHtn <- icd::icd_names_quan_elix_htn
  quanElixComorbidNamesHtnAbbrev <- icd::icd_names_quan_elix_htn_abbrev
  ahrqComorbidNames <- icd::icd_names_ahrq
  ahrqComorbidNamesAbbrev <- icd::icd_names_ahrq_abbrev
  ahrqComorbidNamesHtn <- icd::icd_names_ahrq_htn
  ahrqComorbidNamesHtnAbbrev <- icd::icd_names_ahrq_htn_abbrev
  charlsonComorbidNames <- icd::icd_names_charlson
  charlsonComorbidNamesAbbrev <- icd::icd_names_charlson_abbrev

  if (save_data)
    save(icd9Billable,
         icd9Hierarchy,
         # comorbidity mappings
         ahrqComorbid,
         quanDeyoComorbid,
         quanElixComorbid,
         elixComorbid,
         # now the naming
         elixComorbidNames,
         elixComorbidNamesAbbrev,
         elixComorbidNamesHtn,
         elixComorbidNamesHtnAbbrev,
         quanElixComorbidNames,
         quanElixComorbidNamesAbbrev,
         quanElixComorbidNamesHtn,
         quanElixComorbidNamesHtnAbbrev,
         ahrqComorbidNames,
         ahrqComorbidNamesAbbrev,
         ahrqComorbidNamesHtn,
         ahrqComorbidNamesHtnAbbrev,
         charlsonComorbidNames,
         charlsonComorbidNamesAbbrev,
         file = file.path("data", "deprecated.RData"), compress = "xz")

  invisible(named_list(icd9Billable,
                       icd9Hierarchy,
                       # comorbidity mappings
                       ahrqComorbid,
                       quanDeyoComorbid,
                       quanElixComorbid,
                       elixComorbid,
                       # now the naming
                       elixComorbidNames,
                       elixComorbidNamesAbbrev,
                       elixComorbidNamesHtn,
                       elixComorbidNamesHtnAbbrev,
                       quanElixComorbidNames,
                       quanElixComorbidNamesAbbrev,
                       quanElixComorbidNamesHtn,
                       quanElixComorbidNamesHtnAbbrev,
                       ahrqComorbidNames,
                       ahrqComorbidNamesAbbrev,
                       ahrqComorbidNamesHtn,
                       ahrqComorbidNamesHtnAbbrev,
                       charlsonComorbidNames,
                       charlsonComorbidNamesAbbrev)
  )
}

