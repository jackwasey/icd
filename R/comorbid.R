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

#' @rdname icd9InReferenceCode
#' @export
#' @examples
#' #%i9in% assumes both test code(s) and reference set of codes are \emph{short}
#' "1024" %i9in% "102"
#' "1024" %i9in% c("102","1025")
#' c("102", "1024","1025") %i9in% "102"
#' c("102", "1024","1025") %i9in% c("1024", "1025")
#' c("102", "1024","1025") %i9in% c("102", "1024", "1025")
"%i9in%" <- function(icd9, icd9Reference) {
  icd9InReferenceCode(icd9 = icd9, icd9Reference = icd9Reference,
                      isShort = TRUE, isShortReference = TRUE)
}
#' @title find comorbidities from ICD-9 codes.
#' @description This is the main function which extracts co-morbidities from a
#'   set of ICD-9 codes. This is when some trivial post-processing of the
#'   comorbidity data is done, e.g. renaming to human-friendly field names, and
#'   updating fields according to rules. The exact fields from the original
#'   mappings can be obtained using \code{applyHierarchy = FALSE}, but for
#'   comorbidity counting, Charlson Score, etc., the rules should be applied.
#' @template icd9df
#' @param icd9Mapping list (or name of a list if character vector of length one
#'   is given as argument) of the comorbidities with each top-level list item
#'   containing a vector of decimal ICD9 codes. This is in the form of a list,
#'   with the names of the items corresponding to the comorbidities (e.g. "HTN",
#'   or "diabetes") and the contents of each list item being a character vector
#'   of short-form (no decimal place but ideally zero left-padded) ICD-9 codes.
#'   No default: user should prefer to use the derivative functions, e.g.
#'   icd9ComorbidAhrq, since these also provide appropriate naming for the
#'   fields, and squashing the hierarchy (see \code{applyHierarchy} below)
#' @template visitid
#' @template icd9field
#' @details There is a change in behavior from previous versions. The visitId
#'   column is (implicitly) sorted by using std::set container. Previously, the
#'   visitId output order was whatever R's \code{aggregate} produced.
#' @examples
#'   pts = data.frame(visitId = c("2", "1", "2", "3", "3"),
#'                    icd9 = c("39891", "40110", "09322", "41514", "39891"))
#'    icd9ComorbidShort(pts, ahrqComorbid) # visitId is now sorted
#' @export
icd9Comorbid <- function(icd9df,
                         icd9Mapping,
                         visitId = "visitId",
                         icd9Field = "icd9",
                         isShort = icd9GuessIsShort(icd9df[[icd9Field]]),
                         isShortMapping = icd9GuessIsShort(icd9Mapping[[1]])) {

  checkmate::checkDataFrame(icd9df, min.cols = 2)
  #checkmate::checkString(visitId)
  visitId <- as.character(visitId)
  checkmate::checkString(icd9Field)
  checkmate::checkLogical(isShort, any.missing = FALSE, len = 1)
  checkmate::checkLogical(isShortMapping, any.missing = FALSE, len = 1)
  stopifnot(visitId %in% names(icd9df), icd9Field %in% names(icd9df))

  if (!isShort)
    icd9df[[visitId]] <- icd9DecimalToShort(icd9df[[visitId]])

  if (!isShortMapping)
    icd9Mapping <- lappply(icd9Mapping, icd9DecimalToShort)

  # return via call to the C++ function:
  icd9ComorbidShort(icd9df, icd9Mapping, visitId, icd9Field)

}

# old version, now in C++
icd9Comorbid_R <- function(icd9df,
                         visitId = "visitId",
                         icd9Field = "icd9",
                         isShort,
                         icd9Mapping,
                         validateMapping = FALSE,
                         isShortMapping = TRUE) {


  if (is.character(icd9Mapping)) {
    stopifnot(exists(icd9Mapping))
    icd9Mapping <- get(icd9Mapping)
  }

  if (validateMapping) stopifnot(icd9ValidMapping(icd9Mapping, isShortMapping))

  # drop factor down to character codes #TODO: is this necessary or desirable?
  ic <- jwutil::asCharacterNoWarn(icd9df[[icd9Field]])

  # loop through names of icd9 mapping, and put the results together so each
  # column is one comorbidity in a data frame. This is much faster with vapply,
  # and it keeps the logicals instead of making them characters

  i <- cbind(
    icd9df[visitId],
    sapply(
      X = names(icd9Mapping),
      # FUN looks up each visit icd9 code in given set of comorbidity icd9 codes
      FUN = function(comorbidity)
        icd9InReferenceCode( icd9 = ic,
                             icd9Reference = icd9Mapping[[comorbidity]],
                             isShort = isShort,
                             isShortReference = isShortMapping),
      simplify = FALSE # vapply always simplifies if only one row
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
#' @description For Charlson/Deyo comorbidities, strictly speaking, there is no
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

#' @title Calculate Charlson Comorbidity Index (Charlson Score)
#' @rdname icd9Charlson
#' @description Charlson score is calculated in the basis of the Quan revision
#'   of Deyo's ICD-9 mapping. (Peptic Ulcer disease no longer warrants a point.)
#'   Quan published an updated set of scores, but it seems most people use the
#'   original scores for easier comaprison between studies, even though Quan's
#'   were more predictive. TODO: add Quan Charlson score calculation.
#' @details Per Quan, "The following comorbid conditions were mutually
#'   exclusive: diabetes with chronic complications and diabetes without chronic
#'   complications; mild liver disease and moderate or severe liver disease; and
#'   any malignancy and metastatic solid tumor.""
#' @param x data frame containing a column of visit or patient identifiers, and
#'   a column of ICD-9 codes. It may have other columns which will be ignored.
#'   By default, the first column is the patient identifier and is not counted.
#'   If \code{visitId} is not specified, the first column is used.
#' @template visitid
#' @param return.df single logical value, if true, a two column data frame will
#'   be returned, with the first column named as in input data frame (i.e.
#'   \code{visitId}), containing all the visits, and the second column
#'   containing the Charlson Comorbidity Index.
#' @param stringsAsFactors single logical, passed on when constructing
#'   data.frame if \code{return.df} is \code{TRUE}. If the input data frame
#'   \code{x} has a factor for the visitId, this is not changed, but a
#'   non-factor visitId may be converted or not converted according to your
#'   system default or this setting.
#' @param ... further arguments to pass on to \code{icd9ComorbidQuanDeyo}, e.g.
#'   \code{icd9Field}
#' @examples
#' mydf <- data.frame(visitId = c("a", "b", "c"),
#'                    icd9 = c("441", "412.93", "044.9"))
#' cmb <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE, applyHierarchy = TRUE)
#' cmb
#' icd9Charlson(mydf, isShort = FALSE)
#' icd9Charlson(mydf, isShort = FALSE, return.df = TRUE)
#' icd9CharlsonComorbid(cmb)
#' @export
icd9Charlson <- function(x, visitId = NULL,
                         return.df = FALSE,
                         stringsAsFactors = getOption("stringsAsFacotrs"),
                         ...) {
  stopifnot(is.data.frame(x))
  if (is.null(visitId))
    visitId <- names(x)[1]
  else
    stopifnot(visitId %in% names(x))
  stopifnot(is.character(visitId))
  stopifnot(length(visitId) == 1)
  stopifnot(is.logical(return.df))
  stopifnot(length(return.df) == 1)
  res <- icd9CharlsonComorbid(
    icd9ComorbidQuanDeyo(x, visitId, applyHierarchy = TRUE, ...))

  if (return.df) return(cbind(x[visitId],
                              data.frame("Charlson" = res),
                              stringsAsFactors = stringsAsFactors))
  res
}

#' @rdname icd9Charlson
#' @param applyHierarchy single logical value, default is FALSE. If TRUE, will
#'   drop DM if DMcx is present, etc.
#' @export
icd9CharlsonComorbid <- function(x, visitId = NULL, applyHierarchy = FALSE) {
  stopifnot(is.data.frame(x))
  if (is.null(visitId)) {
    if (!any(names(x) == "visitId"))
      visitId <- names(x)[1]
    else
      visitId <- "visitId"
  } else
    stopifnot(visitId %in% names(x))
  stopifnot(is.character(visitId))
  stopifnot(length(visitId) == 1)

  stopifnot(ncol(x) == 18)
  weights <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
               2, 2, 2, 2,
               3, 6, 6)


  if (applyHierarchy) {
    x$DM <- x$DM & !x$DMcx
    x$LiverMild <- x$LiverMild & !x$LiverSevere
    x$Cancer <- x$Cancer & !x$Mets
  } else {
    stopifnot(!any(x$DM & x$DMcx))
    stopifnot(!any(x$LiverMild & x$LiverSevere))
    stopifnot(!any(x$Cancer & x$Mets))
  }
  m <- as.matrix(x[, names(x) %nin% visitId])
  rowSums(m * weights)
}

#' @title count icd9 codes or comorbidities for each patient
#' @rdname icd9Count
#' @description \code{icd9Count} takes a data frame with a column for
#'   \code{visitId} and another for ICD-9 code, and returns the number of
#'   distinct codes for each patient.
#'
#'   The visitId field is typically the first column. If there is no column
#'   called \code{visitId} and \code{visitId} is not specified, the first column
#'   is used.
#' @details TODO: optionally check each code is valid before counting.
#' @param x data frame with one row per patient, and a true/false or 1/0 flag
#'   for each column. By default, the first column is the patient identifier and
#'   is not counted. If \code{visitId} is not specified, the first column is
#'   used.
#' @template visitid
#' @param return.df single logical, if \code{TRUE}, return the result as a data
#'   frame with the first column being the \code{visitId}, and the second being
#'   the count. If \code{visitId} was a factor or named differently in the
#'   input, this is preserved.
#' @return vector of the count of comorbidities for each patient. This is
#'   sometimes used as a metric of comorbidity load, instead of, or inaddition
#'   to metrics like the Charlson Comorbidity Index (aka Charlson Score)
#' @examples
#'   mydf <- data.frame(visitId = c("r", "r", "s"),
#'                    icd9 = c("441", "412.93", "044.9"))
#'   icd9Count(mydf, return.df = TRUE)
#'   icd9Count(mydf)
#'
#'   cmb <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE)
#'   icd9CountComorbidBin(cmb)
#'
#'   wide <- data.frame(visitId = c("r", "s", "t"),
#'                    icd9_1 = c("0011", "441", "456"),
#'                    icd9_2 = c(NA, "442", NA),
#'                    icd9_3 = c(NA, NA, "510"))
#'   icd9CountWide(wide)
#'   # or:
#'   library(magrittr)
#'   wide %>% icd9WideToLong %>% icd9Count
#' @export
icd9Count <- function(x, visitId = NULL, return.df = FALSE) {
  stopifnot(is.data.frame(x))
  if (is.null(visitId)) {
    if (!any(names(x) == "visitId"))
      visitId <- names(x)[1]
    else
      visitId <- "visitId"
  } else
    stopifnot(visitId %in% names(x))
  stopifnot(is.character(visitId))
  stopifnot(length(visitId) == 1)

  res <- aggregate(x[names(x) %nin% visitId],
                   by = x[visitId],
                   FUN = length)
  names(res)[names(res) %nin% visitId] <- "icd9Count"
  if (return.df) return(res)
  res[["icd9Count"]]
}

#' @rdname icd9Count
#' @description \code{icd9CountComorbidBin} differs from the other counting
#'   functions in that it counts _comorbidities_, not individual diagnoses. It
#'   accepts any data frame with either logicals or zero/non-zero contents, with
#'   a single column for visitId. No checks are made to see whether visitId is
#'   duplicated.
#' @export
icd9CountComorbidBin <- function(x, visitId = NULL, return.df = FALSE) {
  stopifnot(is.data.frame(x))
  if (is.null(visitId)) {
    if (!any(names(x) == "visitId"))
      visitId <- names(x)[1]
    else
      visitId <- "visitId"
  } else
    stopifnot(visitId %in% names(x))
  stopifnot(is.character(visitId))
  stopifnot(length(visitId) == 1)

  res <- apply(x[, names(x) %nin% visitId],
               MARGIN = 1,
               FUN = sum)
  names(res) <- x[[visitId]]
  if (return.df) return(cbind(x[visitId], "icd9Count" = res))

  res
}

#' @rdname icd9Count
#' @description For \code{icd9Count}, it is assumed that all the columns apart
#'   from \code{vistiId} represent actual or possible ICD-9 codes. Duplicate
#'   \code{visitId}s are repeated as given and aggregated.
#' @param aggregate, single logical, default is FALSE. If TRUE, the length (or
#'   rows) of the output will no longer match the input, but duplicate visitIds
#'   will be counted together.
#' @export
icd9CountWide <- function(x,
                          visitId = NULL,
                          return.df = FALSE,
                          aggregate = FALSE) {
  stopifnot(is.data.frame(x))
  stopifnot(is.logical(return.df))
  stopifnot(is.logical(aggregate))
  if (is.null(visitId)) {
    if (!any(names(x) == "visitId"))
      visitId <- names(x)[1]
    else
      visitId <- "visitId"
  } else
    stopifnot(visitId %in% names(x))
  stopifnot(is.character(visitId))
  stopifnot(length(visitId) == 1)

  res <- apply(x[names(x) %nin% visitId], 1, function(x) sum(!is.na(x)))
  names(res) <- x[[visitId]]
  if (!aggregate) {
    if (return.df)
      return(cbind(x[visitId], "count" = res))
    else
      return(res)
  }
  rdf <- cbind(x[visitId], "count" = res)
  rdfagg <- aggregate(rdf["count"], by = rdf[visitId], FUN = sum)
  if (return.df) return(rdfagg)
  vec <- rdfagg[["count"]]
  names(vec) <- rdfagg[[visitId]]
  vec
}
