#' @param poa single character value, being one of \code{poaChoices} whether to
#'   account for co-morbidities flagged as present-on-arrival. This is not a
#'   simple binary, since many codes are exempt, unspecified, or unknown.
#'   \code{poaField} gives the choices: yes, not no, no, not yes. The
#'   intermediate codes, such as "exempt", "unknown" and \code{NA} mean that
#'   "yes" is not the same as "not no."
