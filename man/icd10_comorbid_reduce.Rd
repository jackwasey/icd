% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comorbid.R
\name{icd10_comorbid_reduce}
\alias{icd10_comorbid_reduce}
\title{ICD-10 comorbidities by reducing problem size}
\usage{
icd10_comorbid_reduce(
  x = x,
  map,
  visit_name,
  icd_name,
  short_code,
  short_map,
  return_df,
  return_binary = FALSE,
  categorize_fun = categorize_simple,
  ...
)
}
\description{
Use method to reduce ICD-10 problem by initially finding only relevant codes
in the map, then populating map only with the exact patient ICD codes (not
the original map codes), before doing categorization.
}
\keyword{internal}
