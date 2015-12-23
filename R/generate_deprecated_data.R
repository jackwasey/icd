#' Generate deprecated data
#'
#' Unfortunately, this is easiest (and has a chance of passing CRAN checks) if I
#' just duplicate data rather than trying to change the namespace
#' @keywords internal
generate_deprecated_data <- function() {

  # no particular reason to use assign here; relic of having been trying to
  # assign to package namespace.
  assign("icd9Billable", icd9::icd9cm_billable)
  assign("icd9Hierarchy", icd9::icd9_hierarchy)
  assign("ahrqComorbid", icd9::icd9_map_ahrq)
  assign("ahrqComorbidAll", icd9::icd9_map_ahrq_all)
  assign("quanDeyoComorbid", icd9::icd9_map_quan_deyo)
  assign("quanElixComorbid", icd9::icd9_map_quan_elix)
  assign("elixComorbid", icd9::icd9_map_elix)
  assign("elixComorbidNames", icd9::icd_names_elix)
  assign("elixComorbidNamesAbbrev", icd9::icd_names_elix_abbrev)
  assign("elixComorbidNamesHtn", icd9::icd_names_elix_htn)
  assign("elixComorbidNamesHtnAbbrev", icd9::icd_names_elix_htn_abbrev)
  assign("quanElixComorbidNames", icd9::icd_names_quan_elix)
  assign("quanElixComorbidNamesAbbrev", icd9::icd_names_quan_elix_abbrev)
  assign("quanElixComorbidNamesHtn", icd9::icd_names_quan_elix_htn)
  assign("quanElixComorbidNamesHtnAbbrev", icd9::icd_names_quan_elix_htn_abbrev)
  assign("ahrqComorbidNames", icd9::icd_names_ahrq)
  assign("ahrqComorbidNamesAbbrev", icd9::icd_names_ahrq_abbrev)
  assign("ahrqComorbidNamesHtn", icd9::icd_names_ahrq_htn)
  assign("ahrqComorbidNamesHtnAbbrev", icd9::icd_names_ahrq_htn_abbrev)
  assign("charlsonComorbidNames", icd9::icd_names_charlson)
  assign("charlsonComorbidNamesAbbrev", icd9::icd_names_charlson_abbrev)

  save(icd9Billable,
       icd9Hierarchy,
# comorbidity mappings
       ahrqComorbid,
       ahrqComorbidAll,
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
}

