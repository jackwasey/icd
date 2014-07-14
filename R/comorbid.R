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
#' take a regular string of an ICD9 code of format (ABC.zxyz) with or without
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

# this runs outside of a function, on package load
library(memoise)
memSpawnRefKids <- memoise::memoise(spawnReferenceChildren)

#' @title match ICD9 codes
#' @aliases "%i9in%"
#' @description This does the hard work of finding whether a given icd9 code
#'   falls under a group of reference ICD9 codes. icd9Reference is expanded to cover
#'   all possible subgroups, then we look for matches where the given ICD9 codes
#'   appear in the icd9Reference.
#'   http://www.acep.org/Clinical---Practice-Management/V-and-E-Codes-FAQ/
#' @seealso comorbidities.
#' @templateVar icd9AnyName "icd9,icd9Reference"
#' @template icd9-any
#' @template isShort
#' @template validate
#' @param isShortReference logical, see argument \code{isShort}
#' @return logical vector of which icd9 match or are subcategory of
#'   icd9Referenec
#' @keywords internal
icd9InReferenceCode <- function(icd9, icd9Reference,
                                isShort = TRUE, isShortReference = TRUE,
                                validate = FALSE, validateReference = FALSE) {

  if (!class(icd9) %in% c("character", "numeric", "integer"))
    stop("icd9InReferenceCode expects a character or number vector for icd9, but got: ", class(icd9))
  if (!class(icd9Reference) %in% c("character", "numeric", "integer"))
    stop("icd9InReferenceCode expects a character or number vector for the basecodes,
         to avoid ambiguity with trailing zeroes, but got: ", class(icd9Reference))
  stopifnot(class(isShort) == 'logical', class(isShortReference) == "logical")

  if (length(isShort) >  1)
    stop("icd9InReferenceCode got vector for isShort, expected single TRUE or FALSE value")
  if (length(isShortReference) >  1)
    stop("icd9InReferenceCode got vector for isShortReference, expected single TRUE or FALSE value")
  if (length(isShort) == 0 )
    stop("icd9InReferenceCode got empty vector for isShort, expected single TRUE or FALSE value")
  if (length(isShortReference) == 0)
    stop("icd9InReferenceCode got empty vector for isShortReference, expected single TRUE or FALSE value")

  if (length(icd9Reference) == 0)
    stop("icd9InReferenceCode expects at least one icd9 code to test against")

  if (validate) stopIfInvalidIcd9(icd9, isShort = isShort)
  if (validateReference) stopIfInvalidIcd9(icd9Reference, isShort = isShortReference)

  kids <- memSpawnRefKids(icd9Reference, isShortReference)

  # convert to short form to make comparison
  if (isShort == FALSE) icd9 <- icd9DecimalToShort(icd9)
  if (isShortReference == FALSE) kids <- icd9DecimalToShort(kids)

  icd9 %in% kids
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
#' @param icd9df data.frame with fields specified by visitId and icd9Code.
#'   icd9Code is assumed to be a non-decimal 'short' form ICD9 code. There is a
#'   many to many ratio of icd9:visitId. This table contains multiple visitId
#'   rows, with one row per ICD-9 code. Therefore, every ICD-9 code listed is
#'   associated with at least one visit ID.
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
#'   icd9ComorbiditiesAhrq, since these also provide appropriate naming for the
#'   fields, and squashing the hierarchy (see \code{applyHierarchy} below)
#' @param validateMapping logical, whether to validate all the ICD-9 codes in
#'   the mapping list. Default is not to check. If validation fails, stop with
#'   an error. This is probably worth doing at least once for each mapping used,
#'   since there should never be an error in mapping. There is overhead to check
#'   the mapping each time, so not done by default. Could consider using
#'   \code{memoise} to cache the result of the check. (TODO)
#' @param longNames single locical value that defaults to \code{TRUE}, in which
#'   case the more human-readable names stored in e.g. \code{ahrqComorbidNames}
#'   are applied to the data frame column names.
#' @param applyHierarchy single logical value that defaults to \code{TRUE}, in
#'   which case the hierarchy defined for the mapping is applied. E.g. in
#'   Elixhauser, you can't have uncomplicated and complicated diabetes both
#'   flagged.
#' @param isShortMapping logical, whether the mapping is defined with short
#'   ICD-9 codes (TRUE, the default), or decimal if set to FALSE.
#' @export
icd9Comorbidities <- function(icd9df,
                              visitId = "visitId",
                              icd9Field = "icd9",
                              isShort,
                              icd9Mapping,
                              validateMapping = FALSE,
                              isShortMapping = TRUE) {

  stopifnot(visitId %in% names(icd9df), icd9Field %in% names(icd9df))

  if (is.character(icd9Mapping)) {
    stopifnot(exists(icd9Mapping))
    icd9Mapping <- get(icd9Mapping)
  }

  stopifnot(icd9ValidMapping(icd9Mapping = icd9Mapping, isShort = isShortMapping))

  # drop factor down to character codes #TODO: is this necessary or desirable?
  icd9c <- asCharacterNoWarn(icd9df[[icd9Field]])

  # loop through names of icd9 mapping, and put the results together so each
  # column is one comorbidity in a data frame. This is much faster with vapply,
  # and it keeps the logicals instead of making them characters

  i <- cbind(
    icd9df[visitId],
    vapply(
      X = names(icd9Mapping),
      FUN.VALUE = rep(FALSE, length(icd9df[[icd9Field]])),
      # FUN looks up each visit icd9 code in given set of comorbidity icd9 codes
      FUN = function(comorbidity) {
        icd9InReferenceCode( # this function is just a fancy %in% with sanity checks
          icd9c,
          icd9Mapping[[comorbidity]]
        )
      }
    )
  )
  # at this point, 'i' still has multiple rows per visit, but with a column per
  # comorbidity: next step aggregates all the comorbidities together to give one
  # row per visitid
  aggregate(
    x = i[, -which(names(i) == visitId)], # all cols except visit ID will be aggregated
    by = list(visitId = i[[visitId]]), # group by the visitId
    FUN = any,
    simplify = TRUE
  )
}

#' @rdname icd9Comorbidities
#' @export
icd9ComorbiditiesAhrq <- function(icd9df,
                                  visitId = "visitId",
                                  icd9Field = "icd9",
                                  isShort,
                                  validateMapping = FALSE,
                                  abbrevNames = FALSE,
                                  applyHierarchy = TRUE) {

  cbd <- icd9Comorbidities(icd9df = icd9df, visitId = visitId, icd9Field = icd9Field,
                    isShort = isShort, icd9Mapping = ahrqComorbid)
  if (applyHierarchy) {

    # Use >0 rather than logical - apparently faster, and future proof against
    # change to binary from logical values in the matirx.
    cbd[cbd[["Mets"]] > 0, "Tumor"] <- 0
    cbd[cbd[["DM"]] > 0, "DMcx"] <- 0
    # HTN already combined in AHRQ
  }
  if (abbrevNames) { names(cbd)[-1] <- ahrqComorbidNamesAbbrev } else { names(cbd)[-1] <- ahrqComorbidNames }
  cbd
}

#' @rdname icd9Comorbidities
#' @description For Charlson-based comorbidities, strictly speaking, there is no
#'   dropping of more e.g. uncomplicated DM if complicated DM exists, however,
#'   this is probaably useful, in general and is essential when calculating the
#'   Charlson score.
#' @export
icd9ComorbiditiesQuanDeyo <- function(icd9df,
                                      visitId = "visitId",
                                      icd9Field = "icd9",
                                      isShort,
                                      validateMapping = FALSE,
                                      abbrevNames = TRUE,
                                      applyHierarchy = TRUE) {
  cbd <- icd9Comorbidities(icd9df = icd9df, visitId = visitId, icd9Field = icd9Field,
                    isShort = isShort, icd9Mapping = quanDeyoComorbid)
  if (applyHierarchy) {

    # Use >0 rather than logical - apparently faster, and future proof against
    # change to binary from logical values in the matirx.
    cbd[cbd[["Metastatic Carcinoma"]] > 0, "Cancer"] <- 0
    cbd[cbd[["Diabetes with complications"]] > 0, "Diabetes without complications"] <- 0
    cbd[cbd[["Moderate or Severe Liver Disease"]] > 0, "Mild Liver Disease"] <- 0
  }
  if (abbrevNames) names(cbd)[-1] <- CharlsonComorbidNames
}

#' @rdname icd9Comorbidities
#' @export
icd9ComorbiditiesQuanElixhauser <- function(icd9df,
                                            visitId = "visitId",
                                            icd9Field = "icd9",
                                            isShort,
                                            validateMapping = FALSE,
                                            rename = TRUE,
                                            applyHierarchy = TRUE) {
  cbd <- icd9Comorbidities(icd9df = icd9df, visitId = visitId, icd9Field = icd9Field,
                    isShort = isShort, icd9Mapping = quanElixhauserComorbid)
  if (applyHierarchy) {

    # Use >0 rather than logical - apparently faster, and future proof against
    # change to binary from logical values in the matirx.
    cbd[cbd[["mets"]] > 0, "solid tumor"] <- 0
    cbd[cbd[["dm.comp"]] > 0, "dm.uncomp"] <- 0
    # combine HTN
    cbd[["htn"]] <- cbd[["htn"]] + cbd[["htncx"]] > 0
    cbd[["htncx"]] <- NULL

    if (rename) names(cbd)[-1] <- quanElixhauserComorbidNames
  } else {
    # if we didn't apply the hierarchy, we have to use the naming scheme with HTN separated out:

    # assume that the comorbidities are the last 31 fields. At present, the
    # icd9Comorbidities function doesn't attempt to aggregate fields it doesn't
    # know about, e.g. POA, or anything else the user provides in the data
    # frame, so these are just dropped, leaving the fields for visitId and all
    # the comorbidities:
    if (rename) names(cbd)[-1] <- quanElixhauserComorbidNamesHtn
  }
  cbd
}

#' @rdname icd9Comorbidities
#' @export
icd9ComorbiditiesElixhauser <- function(icd9df,
                                        visitId = "visitId",
                                        icd9Field = "icd9",
                                        isShort,
                                        validateMapping = FALSE,
                                        rename = TRUE,
                                        applyHierarchy = TRUE) {
  cbd <- icd9Comorbidities(icd9df = icd9df, visitId = visitId, icd9Field = icd9Field,
                           isShort = isShort, icd9Mapping = elixhauserComorbid)
  if (applyHierarchy) {
    cbd[cbd[["mets"]] > 0, "solid tumor"] <- 0
    cbd[cbd[["dm.comp"]] > 0, "dm.uncomp"] <- 0
    cbd[["htn"]] <- cbd[["htn"]] + cbd[["htncx"]] > 0
    cbd[["htncx"]] <- NULL
    if (rename) names(cbd)[-1] <- elixhauserComorbidNames
  } else {
    if (rename) names(cbd)[-1] <- elixhauserComorbidNamesHtn
  }
  cbd
}

#' @title filters data frame based on present-on-arrival flag
#' @description this is not a simple binary, since many codes are exempt,
#'   unspecified, or unknown. Therefore, two options are given: get all the
#'   comorbidities where the POA flag was definitely -ve, coded as "N" or
#'   definitely +ve and coded as "Y". Negating one set won't give the other set
#'   unless all codes were either Y or N. #describeIn icd9Comorbidities
#' @template poaField
#' @examples
#' # using magrittr is beautiful:
#' myData <- data.frame(
#'   visitId = c("v1", "v2", "v3", "v4"),
#'   icd9 = c("39891", "39790", "41791", "4401"),
#'   poa = c("Y", "N", NA, "Y"),
#'   stringsAsFactors = FALSE
#' )
#' myData %>% icd9FilterPoaNotNo() %>% icd9ComorbiditiesAhrq(isShort = TRUE)
#' # can fill out named fields also:
#' myData %>% icd9FilterPoaYes(poaField="poa") %>% icd9ComorbiditiesAhrq(icd9Field = "icd9", visitId = "visitId")
#' # can call the core icd9Comorbidities function with an arbitrary mapping
#' myData %>%
#' icd9FilterPoaYes() %>%
#' icd9Comorbidities(icd9Field = "icd9", visitId = "visitId", icd9Mapping = quanElixhauserComorbid, validateMapping = TRUE, isShortMapping = TRUE)
#' @export
icd9FilterPoa <- function(icd9df, poaField = "poa", poa = icd9PoaChoices) {
  poa = match.arg(poa)
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
  icd9df[!is.na(icd9df[[poaField]]) & icd9df[[poaField]] %in% c("Y", "y"), -which(names(icd9df) == poaField)]
}

#' @rdname icd9FilterPoa
#' @export
icd9FilterPoaNo <- function(icd9df, poaField = "poa") {
  stopifnot(poaField %in% names(icd9df))
  icd9df[!is.na(icd9df[[poaField]]) & icd9df[[poaField]] %in% c("N", "n"), -which(names(icd9df) == poaField)]
}

#' @rdname icd9FilterPoa
#' @export
icd9FilterPoaNotNo <- function(icd9df, poaField = "poa") {
  stopifnot(poaField %in% names(icd9df))
  icd9df[is.na(icd9df[[poaField]]) | icd9df[[poaField]] %nin% c("N", "n"), -which(names(icd9df) == poaField)]
}

#' @rdname icd9FilterPoa
#' @export
icd9FilterPoaNotYes <- function(icd9df, poaField = "poa") {
  stopifnot(poaField %in% names(icd9df))
  icd9df[is.na(icd9df[[poaField]]) | icd9df[[poaField]] %nin% c("Y", "y"), -which(names(icd9df) == poaField)]
}
