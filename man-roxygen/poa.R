#' @param poa single character value, being one of \code{Yes}, \code{No},
#'   \code{NotYes}, and \code{NotNo}, indicating whether to account for
#'   comorbidities flagged as present-on-arrival. This is not a simple flag,
#'   because many codes are exempt, unspecified, or unknown. The intermediate
#'   codes, such as "exempt", "unknown" and \code{NA} mean that "yes" is not the
#'   same as "not no."
