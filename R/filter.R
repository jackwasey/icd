#' @title Filter ICD-9 codes by validity.
#' @description Filters a data.frame of patients for valid or invalid ICD-9 codes
#' @template icd9df
#' @template icd9field
#' @template isShort
#' @param invert single logical value, if TRUE will return invalid instead of valid rows.
#' @export
icd9FilterValid <- function(icd9df, icd9Field = "icd9",
                            isShort = icd9GuessIsShort(icd9df[[icd9Field]]), invert = FALSE) {
  v <- icd9IsValid(icd9 = icd9df[[icd9Field]], isShort = isShort) != invert
  icd9df[v, ]
}

#' @title Filter ICD-9 codes by invalidity.
#' @description Filters a data.frame of patients for valid or invalid ICD-9 codes
#' @template icd9df
#' @template icd9field
#' @template isShort
#' @param invert single logical value, if TRUE will return valid instead of invalid rows.
#' @export
icd9FilterInvalid <- function(icd9df, icd9Field = "icd9",
                              isShort = icd9GuessIsShort(icd9df[[icd9Field]]), invert = FALSE)
  icd9FilterValid(icd9df, icd9Field, isShort, invert = !invert)


#' @name icd9FilterPoa
#' @title Filters data frame based on present-on-arrival flag
#' @description Present On Arrival (POA) is not a simple flag, since many codes
#'   are exempt, unspecified, or unknown. Therefore, two options are given: get
#'   all the comorbidities where the POA flag was definitely -ve, coded as "N"
#'   or definitely +ve and coded as "Y". Negating one set won't give the other
#'   set unless all codes were either Y or N. #describeIn icd9Comorbid
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

#' @describeIn icd9FilterPoa
#' @export
icd9FilterPoaYes <- function(icd9df, poaField = "poa")
  .icd9FilterPoa(icd9df, poaField, choice = c("Y", "y"), negative = FALSE)

#' @describeIn icd9FilterPoa
#' @export
icd9FilterPoaNo <- function(icd9df, poaField = "poa")
  .icd9FilterPoa(icd9df, poaField, choice = c("N", "n"), negative = FALSE)

#' @describeIn icd9FilterPoa
#' @export
icd9FilterPoaNotNo <- function(icd9df, poaField = "poa")
  .icd9FilterPoa(icd9df, poaField, choice = c("N", "n"), negative = TRUE)

#' @describeIn icd9FilterPoa
#' @export
icd9FilterPoaNotYes <- function(icd9df, poaField = "poa")
  .icd9FilterPoa(icd9df, poaField, choice = c("Y", "y"), negative = TRUE)
