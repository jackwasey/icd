#' @param visitId The name of the column in the data frame which contains the
#'   patient or visit identifier. Typically this is the visit identifier, since
#'   patients come leave and enter hospital with different ICD-9 codes. It is a
#'   character vector of length one. If left empty, or \code{NULL}, then an
#'   attempt is made to guess which field has the ID for the patient encounter
#'   (not a patient ID, although this can of course be specified directly). The
#'   guesses proceed until a single match is made. Data frames may be wide with
#'   many matching fields, so to avoid false positives, anything but a single
#'   match is rejected. If there are no successful guesses, and \code{visitId}
#'   was not specified, then the first column of the data frame is used.
