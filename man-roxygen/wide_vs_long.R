#' Note the distinction between labelling existing data with any classes which
#' \code{icd} provides, and actually converting the structure of the data.
#' @section Long and Wide Formats: As is common with many data sets, key
#'   variables can be concentrated in one column or spread over several. Tools
#'   format of clinical and administrative hospital data, we can perform the
#'   conversion efficiently and accurately, while keeping some metadata about
#'   the codes intact, e.g. whether they are ICD-9 or ICD-10.
#' @section Data structure: Long or wide format ICD data are all expected to be
#'   in a data frame. The \code{data.frame} itself does not carry any ICD
#'   classes at the top level, even if it only contains one type of code;
#'   whereas its constituent columns may have a class specified, e.g.
#'   \code{icd9} or \code{icd10who}.
