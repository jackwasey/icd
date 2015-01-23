
icd9FilterValid <- function(x, ...) UseMethod("icd9FilterValid")

icd9FilterValid.data.frame <- function(x, icd9Field = "icd9",
                                       isShort = TRUE, invert = FALSE) {
  v <- icd9Valid(icd9 = x[[icd9Field]], isShort = isShort)
  if (invert) v <- !v
  x[v, ]
}

icd9FilterValid.character <- function(x, isShort = TRUE, invert = FALSE) {
  v <- icd9Valid(icd9 = x, isShort = isShort)
  if (invert) v <- !v
  x[v]
}

icd9FilterValid.list <- function(x, isShort = TRUE, invert = FALSE) {
  v <- icd9Valid(icd9 = x, isShort = isShort)
  if (invert) v <- !v
  x[v]
}

icd9FilterInvalid <- function(x, ...)
  icd9FilterValid(x, invert = TRUE, ...)

#' @title filters data frame based on present-on-arrival flag
#' @description this is not a simple binary, since many codes are exempt,
#'   unspecified, or unknown. Therefore, two options are given: get all the
#'   comorbidities where the POA flag was definitely -ve, coded as "N" or
#'   definitely +ve and coded as "Y". Negating one set won't give the other set
#'   unless all codes were either Y or N. #describeIn icd9Comorbid
#' @template icd9df
#' @template poaField
#' @template poa
#' @examples
#' \dontrun{
#' # using magrittr is beautiful:
#' library("magrittr", quietly = TRUE, warn.conflicts = FALSE)
#' myData <- data.frame(
#'   visitId = c("v1", "v2", "v3", "v4"),
#'   icd9 = c("39891", "39790", "41791", "4401"),
#'   poa = c("Y", "N", NA, "Y"),
#'   stringsAsFactors = FALSE
#' )
#' myData %>% icd9FilterPoaNotNo() %>% icd9ComorbidAhrq(isShort = TRUE)
#' # can fill out named fields also:
#' myData %>% icd9FilterPoaYes(poaField="poa") %>%
#'   icd9ComorbidAhrq(icd9Field = "icd9", visitId = "visitId")
#' # can call the core icd9Comorbid function with an arbitrary mapping
#' myData %>%
#' icd9FilterPoaYes() %>%
#' icd9Comorbid(icd9Field = "icd9", visitId = "visitId",
#'   icd9Mapping = quanElixComorbid,
#'  validateMapping = TRUE,
#'  isShortMapping = TRUE)
#' }
#' @export
icd9FilterPoa <- function(icd9df, poaField = "poa", poa = icd9PoaChoices) {
  poa <- match.arg(poa)
  stopifnot(is.data.frame(icd9df))
  stopifnot(is.character(poaField), length(poaField) == 1)
  stopifnot(poaField %in% names(icd9df))
  if (poa == "yes") return(icd9FilterPoaYes(icd9df, poaField = poaField))
  if (poa == "no") return(icd9FilterPoaNo(icd9df, poaField = poaField))
  if (poa == "notYes") return(icd9FilterPoaNotYes(icd9df, poaField = poaField))
  if (poa == "notNo") return(icd9FilterPoaNotNo(icd9df, poaField = poaField))
}

.icd9FilterPoa <- function(icd9df, poaField, choice, negative = FALSE) {
  stopifnot(poaField %in% names(icd9df))
  p <- icd9df[[poaField]]
  if (negative)
    return(icd9df[is.na(p) | p %nin% choice, names(icd9df) != poaField])
  icd9df[!is.na(p) & p %in% choice, names(icd9df) != poaField]
}

#' @rdname icd9FilterPoa
#' @export
icd9FilterPoaYes <- function(icd9df, poaField = "poa")
  .icd9FilterPoa(icd9df, poaField, choice = c("Y", "y"), negative = FALSE)

#' @rdname icd9FilterPoa
#' @export
icd9FilterPoaNo <- function(icd9df, poaField = "poa")
  .icd9FilterPoa(icd9df, poaField, choice = c("N", "n"), negative = FALSE)

#' @rdname icd9FilterPoa
#' @export
icd9FilterPoaNotNo <- function(icd9df, poaField = "poa")
  .icd9FilterPoa(icd9df, poaField, choice = c("N", "n"), negative = TRUE)

#' @rdname icd9FilterPoa
#' @export
icd9FilterPoaNotYes <- function(icd9df, poaField = "poa")
  .icd9FilterPoa(icd9df, poaField, choice = c("Y", "y"), negative = TRUE)
