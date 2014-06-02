#' @title reading, analysis and manipulation of ICD-9 codes
#' @description ICD-9 codes are not numbers, and great care is needed when
#'   matching individual codes and ranges of codes. It is easy to make mistake,
#'   hence the need for this code and test suite. ICD-9 codes can be presented
#'   in 'short' 5 character format, or 'decimal' format, seperating the code nto
#'   two groups. Zeros after a decimal place are meaningful, so numerics cannot
#'   be used in most cases. This package enforces character specification to
#'   avoid mistakes. The package also provides a means of looking up which
#'   comorbidities exist in a patient given a set of ICD-9 codes, based on a
#'   mapping of ICD-9 codes to comorbidity groups specified in R. This entails
#'   parsing source SAS source code, and this is also included in the package,
#'   so that subsequent releases of SAS FORMAT data for comorbidities can be
#'   used.
#' @docType package
#' @name icd9-package
#' @aliases icd9
NULL
