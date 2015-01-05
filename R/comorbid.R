#' @title present-on-admission flags
#' @description Present-on-admission (POA) is not simply true or false. It can
#'   be one of a number of indeterminate values, including \code{NA}, or "Y" or
#'   "N". "Present-on-arrival" in this context will mean a positive "Y" flag and
#'   nothing else. Other interpretations are to include all ICD-9 codes not
#'   flagged 'N': but this would include many unknowns. Conversely, when looking
#'   for definite new diagnoses, we should only find 'N' flagged codes, and
#'   ignore anything marked "Y" or indeterminate. This gives four options: poa
#'   == "Y" , poa == "N", poa != "N" , poa != "Y".
#' @keywords character
#' @export
icd9PoaChoices <- c("yes", "no", "notYes", "notNo")

#' spawn reference codes into all possible lower-level codes (and memoise)
#'
#' Take a regular string of an ICD9 code of format (ABC.zxyz) with or without
#' leading and trailing zeroes. top level ICD9 code and return T/F if the icd9
#' fall within subgroups. This takes several seconds on an unimpressive desktop
#' PC, so would benefit from memoization.
#'
#' @import memoise
#' @keywords internal
spawnReferenceChildren <-
  function(icd9Reference, isShortReference) {
    c(
      lapply(
        icd9Reference,
        FUN = function(x) icd9Children(icd9 = x, isShort = isShortReference)
      ),
      recursive = TRUE
    )
  }

# this runs outside of a function, on package load, in package namespace
library(memoise)
memSpawnRefKids <- memoise::memoise(spawnReferenceChildren)

#' @title match ICD9 codes
#' @aliases "%i9in%"
#' @description Finds children of ricd9Reference and looks for icd9 in the
#'   resulting vector.  It is a glorified %in% function.
#' @templateVar icd9AnyName "icd9,icd9Reference"
#' @template icd9-any
#' @template isShort
#' @param isShortReference logical, see argument \code{isShort}
#' @templateVar invalidActionName "invalidAction,invalidActionReference"
#' @template invalid
#' @return logical vector of which icd9 match or are subcategory of
#'   icd9Referenec
#' @keywords internal
icd9InReferenceCode <- function(icd9, icd9Reference,
                                isShort = TRUE,
                                isShortReference = TRUE,
                                invalidAction = icd9InvalidActions,
                                invalidActionReference = icd9InvalidActions) {

  stopifnot(is.numeric(icd9) || is.character(icd9))
  stopifnot(is.numeric(icd9Reference) || is.character(icd9Reference))
  stopifnot(is.logical(isShort), is.logical(isShortReference))
  stopifnot(is.logical(isShort), is.logical(isShortReference))
  stopifnot(length(isShort) ==  1, length(isShortReference) == 1)
  stopifnot(length(icd9Reference) > 0)

  icd9 <- icd9ValidNaWarnStop(
    icd9 = icd9, isShort = isShort, isMajor = FALSE,
    invalidAction = match.arg(invalidAction))
  icd9Reference <- icd9ValidNaWarnStop(
    icd9 = icd9Reference, isShort = isShort, isMajor = FALSE,
    invalidAction = match.arg(invalidActionReference))

  # TODO: this may be omitted if all the children are elaborated in the
  # comorbidity mappings in advance. This is fine for ones I provide, but not
  # necessarily user-generated ones. It is a slow step, hence memoisation. It
  # would be simpler and faster if this could be skipped. I'm currently also
  # elaborating all syntactically possible children, not just codes listed in
  # the official ICD-9-CM list.
  kids <- memSpawnRefKids(icd9Reference, isShortReference)

  # convert to short form to make comparison
  if (!isShort)          icd9 <- icd9DecimalToShort(icd9)
  if (!isShortReference) kids <- icd9DecimalToShort(kids)

  icd9AddLeadingZeroesShort(icd9) %in% kids
}

#' @rdname icd9InReferenceCode
#' @export
#' @examples
#' "1024" %i9in% "102"
#' "1024" %i9in% c("102","1025")
#' c("102", "1024","1025") %i9in% "102"
#' c("102", "1024","1025") %i9in% c("1024", "1025")
#' c("102", "1024","1025") %i9in% c("102", "1024", "1025")
"%i9in%" <- function(icd9, icd9Reference) {
  icd9InReferenceCode(icd9 = icd9, icd9Reference = icd9Reference)
}

#' @title find comorbidities from ICD-9 codes.
#' @description This is the main function which extracts co-morbidities from a
#'   set of ICD-9 codes. This is when some trivial post-processing of the
#'   comorbidity data is done, e.g. renaming to human-friendly field names, and
#'   updating fields according to rules. The exact fields from the original
#'   mappings can be obtained using \code{applyHierarchy = FALSE}, but for
#'   comorbidity counting, Charlson Score, etc., the rules should be applied.
#' @template icd9df
#' @template visitid
#' @template icd9field
#' @template isShort
#' @param icd9Mapping list (or name of a list if character vector of length one
#'   is given as argument) of the comorbidities with each top-level list item
#'   containing a vector of decimal ICD9 codes. This is in the form of a list,
#'   with the names of the items corresponding to the comorbidities (e.g. "HTN",
#'   or "diabetes") and the contents of each list item being a character vector
#'   of short-form (no decimal place but ideally zero left-padded) ICD-9 codes.
#'   No default: user should prefer to use the derivative functions, e.g.
#'   icd9ComorbidAhrq, since these also provide appropriate naming for the
#'   fields, and squashing the hierarchy (see \code{applyHierarchy} below)
#' @param validateMapping logical, whether to validate all the ICD-9 codes in
#'   the mapping list. Default is not to check. If validation fails, stop with
#'   an error. This is probably worth doing at least once for each mapping used,
#'   since there should never be an error in mapping.
#' @param isShortMapping logical, whether the mapping is defined with short
#'   ICD-9 codes (TRUE, the default), or decimal if set to FALSE.
#' @template abbrevHier
#' @export
icd9Comorbid <- function(icd9df,
                         visitId = "visitId",
                         icd9Field = "icd9",
                         isShort,
                         icd9Mapping,
                         validateMapping = FALSE,
                         isShortMapping = TRUE) {

  stopifnot(is.data.frame(icd9df))
  stopifnot(is.character(visitId), length(visitId) == 1)
  stopifnot(is.character(icd9Field), length(icd9Field) == 1)
  stopifnot(is.logical(isShort), length(isShort) == 1)
  stopifnot(is.logical(validateMapping), length(validateMapping) == 1)
  stopifnot(is.logical(isShortMapping), length(isShortMapping) == 1)
  stopifnot(visitId %in% names(icd9df), icd9Field %in% names(icd9df))

  if (is.character(icd9Mapping)) {
    stopifnot(exists(icd9Mapping))
    icd9Mapping <- get(icd9Mapping)
  }

  if (validateMapping) stopifnot(icd9ValidMapping(icd9Mapping, isShortMapping))

  # drop factor down to character codes #TODO: is this necessary or desirable?
  ic <- asCharacterNoWarn(icd9df[[icd9Field]])

  # loop through names of icd9 mapping, and put the results together so each
  # column is one comorbidity in a data frame. This is much faster with vapply,
  # and it keeps the logicals instead of making them characters

  i <- cbind(
    icd9df[visitId],
    vapply(
      X = names(icd9Mapping),
      # FUN looks up each visit icd9 code in given set of comorbidity icd9 codes
      FUN = function(comorbidity)
        icd9InReferenceCode( icd9 = ic,
                             icd9Reference = icd9Mapping[[comorbidity]],
                             isShort = isShort,
                             isShortReference = isShortMapping),
      FUN.VALUE = rep(FALSE, length(icd9df[[icd9Field]]))
    )
  )
  # at this point, 'i' still has multiple rows per visit, but with a column per
  # comorbidity: next step aggregates all the comorbidities together to give one
  # row per visitid
  aggregate(
    x = i[, names(i) != visitId],
    by = list(visitId = i[[visitId]]),
    FUN = any,
    simplify = TRUE
  )
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbidAhrq <- function(icd9df,
                             visitId = "visitId",
                             icd9Field = "icd9",
                             isShort,
                             validateMapping = FALSE,
                             abbrevNames = TRUE,
                             applyHierarchy = TRUE) {

  cbd <- icd9Comorbid(icd9df = icd9df, visitId = visitId,
                      icd9Field = icd9Field, isShort = isShort,
                      icd9Mapping = icd9::ahrqComorbid)
  if (applyHierarchy) {

    # Use >0 rather than logical - apparently faster, and future proof against
    # change to binary from logical values in the matirx.
    cbd[cbd[["Mets"]] > 0, "Tumor"] <- FALSE
    cbd[cbd[["DMcx"]] > 0, "DM"] <- FALSE
    cbd[["HTN"]] <- cbd[["HTN"]] + cbd[["HTNcx"]] > 0
    cbd[["HTNcx"]] <- NULL

    if (abbrevNames)
      names(cbd)[-1] <- icd9::ahrqComorbidNamesAbbrev
    else
      names(cbd)[-1] <- icd9::ahrqComorbidNames
  } else {
    if (abbrevNames)
      names(cbd)[-1] <- icd9::ahrqComorbidNamesHtnAbbrev
    else
      names(cbd)[-1] <- icd9::ahrqComorbidNamesHtn
  }
  cbd
}

#' @rdname icd9Comorbid
#' @description For Charlson-based comorbidities, strictly speaking, there is no
#'   dropping of more e.g. uncomplicated DM if complicated DM exists, however,
#'   this is probaably useful, in general and is essential when calculating the
#'   Charlson score.
#' @export
icd9ComorbidQuanDeyo <- function(icd9df,
                                 visitId = "visitId",
                                 icd9Field = "icd9",
                                 isShort,
                                 validateMapping = FALSE,
                                 abbrevNames = TRUE,
                                 applyHierarchy = TRUE) {
  cbd <- icd9Comorbid(icd9df = icd9df, visitId = visitId,
                      icd9Field = icd9Field, isShort = isShort,
                      icd9Mapping = icd9::quanDeyoComorbid)
  if (applyHierarchy) {

    # Use >0 rather than logical - apparently faster, and future proof against
    # change to binary from logical values in the matirx.
    cbd[cbd[["Mets"]] > 0, "Cancer"] <- FALSE
    cbd[cbd[["DMcx"]] > 0, "DM"] <- FALSE
    cbd[cbd[["LiverSevere"]] > 0, "LiverMild"] <- FALSE
  }
  if (abbrevNames)
    names(cbd)[-1] <- icd9::charlsonComorbidNamesAbbrev
  else
    names(cbd)[-1] <- icd9::charlsonComorbidNames

  cbd
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbidQuanElix <- function(icd9df,
                                 visitId = "visitId",
                                 icd9Field = "icd9",
                                 isShort,
                                 validateMapping = FALSE,
                                 abbrevNames = TRUE,
                                 applyHierarchy = TRUE) {
  cbd <- icd9Comorbid(icd9df = icd9df, visitId = visitId,
                      icd9Field = icd9Field, isShort = isShort,
                      icd9Mapping = icd9::quanElixComorbid)
  if (applyHierarchy) {

    # Use >0 rather than logical - apparently faster, and future proof against
    # change to binary from logical values in the matirx.
    cbd[cbd[["Mets"]] > 0, "Tumor"] <- FALSE
    cbd[cbd[["DMcx"]] > 0, "DM"] <- FALSE
    # combine HTN
    cbd[["HTN"]] <- cbd[["HTN"]] + cbd[["HTNcx"]] > 0
    cbd[["HTNcx"]] <- NULL

    # if we didn't apply the hierarchy, we have to use the naming scheme with
    # HTN separated out:

    # assume that the comorbidities are the last 31 fields. At present, the
    # icd9Comorbid function doesn't attempt to aggregate fields it doesn't
    # know about, e.g. POA, or anything else the user provides in the data
    # frame, so these are just dropped, leaving the fields for visitId and all
    # the comorbidities:

    if (abbrevNames)
      names(cbd)[-1] <- icd9::quanElixComorbidNamesAbbrev
    else
      names(cbd)[-1] <- icd9::quanElixComorbidNames
  } else {
    if (abbrevNames)
      names(cbd)[-1] <- icd9::quanElixComorbidNamesHtnAbbrev
    else
      names(cbd)[-1] <- icd9::quanElixComorbidNamesHtn
  }
  cbd
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbidElix <- function(icd9df,
                             visitId = "visitId",
                             icd9Field = "icd9",
                             isShort,
                             validateMapping = FALSE,
                             abbrevNames = TRUE,
                             applyHierarchy = TRUE) {
  cbd <- icd9Comorbid(icd9df = icd9df, visitId = visitId,
                      icd9Field = icd9Field, isShort = isShort,
                      icd9Mapping = icd9::elixComorbid)
  if (applyHierarchy) {
    cbd[cbd[["Mets"]] > 0, "Tumor"] <- FALSE
    cbd[cbd[["DMcx"]] > 0, "DM"] <- FALSE
    cbd[["HTN"]] <- cbd[["HTN"]] + cbd[["HTNcx"]] > 0
    cbd[["HTNcx"]] <- NULL
    if (abbrevNames)
      names(cbd)[-1] <- icd9::elixComorbidNamesAbbrev
    else
      names(cbd)[-1] <- icd9::elixComorbidNames
  } else {
    if (abbrevNames)
      names(cbd)[-1] <- icd9::elixComorbidNamesHtnAbbrev
    else
      names(cbd)[-1] <- icd9::elixComorbidNamesHtn
  }
  cbd
}

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

#' @rdname icd9FilterPoa
#' @export
icd9FilterPoaYes <- function(icd9df, poaField = "poa") {
  stopifnot(poaField %in% names(icd9df))
  icd9df[!is.na(icd9df[[poaField]]) & icd9df[[poaField]] %in% c("Y", "y"),
         names(icd9df) != poaField]
}

#' @rdname icd9FilterPoa
#' @export
icd9FilterPoaNo <- function(icd9df, poaField = "poa") {
  stopifnot(poaField %in% names(icd9df))
  icd9df[!is.na(icd9df[[poaField]]) & icd9df[[poaField]] %in% c("N", "n"),
         names(icd9df) != poaField]
}

#' @rdname icd9FilterPoa
#' @export
icd9FilterPoaNotNo <- function(icd9df, poaField = "poa") {
  stopifnot(poaField %in% names(icd9df))
  icd9df[is.na(icd9df[[poaField]]) | icd9df[[poaField]] %nin% c("N", "n"),
         names(icd9df) != poaField]
}

#' @rdname icd9FilterPoa
#' @export
icd9FilterPoaNotYes <- function(icd9df, poaField = "poa") {
  stopifnot(poaField %in% names(icd9df))
  icd9df[is.na(icd9df[[poaField]]) | icd9df[[poaField]] %nin% c("Y", "y"),
         names(icd9df) != poaField]
}

icd9CharlsonFromIcd <- function(icd9df) {
  #icd9df %>%
  #icd9CharlsonFromComorbidities
}

icd9CharlsonFromComorbidities <- function(comorbid) {

}

#' @title count comorbidities for each patient
#' @description takes a data frame of comorbidities and returns a simple count
#'   of comorbidities for each patient.
#' @param comorbid data frame with one row per patient, and a true/false or 1/0
#'   flag for each column. The first column is the patient identifier and is not
#'   counted.
#' @export
#' @return vector of the counts, with one per row of the input data
icd9CountComorbid <- function(comorbid) {
  apply(logicalToBinary(comorbid[, -1]),
        MARGIN = 1,
        FUN = sum)
}
