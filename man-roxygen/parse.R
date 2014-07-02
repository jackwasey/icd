#' @param condense logical whether to condense the result ranges to the minimal
#'   set of parent ICD-9 codes which exactly define the list. Defaults to
#'   \code{FALSE} so that the saved list will allow rapid look-up of codes
#'   without further processing. Small memory versus time trade-off.
#' @param save logical whether to save the result in the source tree. Defaults
#'   to \code{FALSE}.
#' @param saveDir path to directory to save the data. This is typically the data
#'   folder in the devleopment source tree. Defaults to \code{"~/icd9/data"}.
#'   Could possibly avoid hard-coding path with devtools functions.
#' @return invisibly returns the list of vectors, where a
#'   co-morbidity name is associated with a character vector of ICD-9 codes.
