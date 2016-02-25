#' Generate deprecated data
#'
#' Unfortunately, this is easiest (and has a chance of passing CRAN checks) if I
#' just duplicate data rather than trying to change the namespace
#' @param save_data save the data in \code{data/deprecated.RData}, default is
#'   \code{TRUE}
#' @keywords internal
generate_deprecated_data <- function(save_data = FALSE,
                                     path = file.path("data", "deprecated.RData")) {

  icd9Billable <- icd::icd9cm_billable
  for (b in seq_along(icd9Billable))
    # deprecated column names also differ; changed so icd9 as a class name is not constantly
    # confused with a column of the same name
    names(icd9Billable[[b]]) <- c("icd9", "descShort", "descLong")

  icd9Hierarchy <- icd::icd9cm_hierarchy
  names(icd9Hierarchy)[1:3] <- c("icd9", "descShort", "descLong")
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

  if (save_data) {
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
         file = path, compress = "xz")
    message("now reload to update deprecated data")
  }

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
