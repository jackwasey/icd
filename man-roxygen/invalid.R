#' @param <% if (exists("invalidActionName")) cat(invalidActionName) else cat("invalidAction") %>
#' character string (unit length) containing one of:
#' "ignore", "silent", "warn", "stop"
#'
#' - \code{ignore} no check for validity of ICD-9 code is made implicitly. This
#' is the fastest option. If the function call is explicitly to check validity,
#' e.g. icd9ValidShort(icd9Short = "12345", invalidAction = "ignore"), then the
#' behavior is the same as \code{silent}.
#'
#' - \code{silent} invalid ICD-9 codes are replaced silently by \code{NA}
#' (actually \code{NA_character_}). If the function returns \code{TRUE} or
#' \code{FALSE} rather than the ICD-9 code itself, then \code{FALSE} is returned
#'
#' - \code{warn} invalid ICD-9 codes are replaced by \code{NA} with a warning
#' revealing the invalid code(s).
#'
#' - \code{stop} stop with error if any invalid codes are found in input. This
#' is the preferred method when doing one-time validation, e.g. of the
#' co-morbidity mapping lists.
