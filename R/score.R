
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
                         stringsAsFactors = getOption("stringsAsFactors"),
                         ...)
  UseMethod("icd9Charlson")

#' @describeIn icd9Charlson default method for icd9Charlson
#' @export
icd9Charlson.default <- function(x, visitId = NULL,
                                 return.df = FALSE,
                                 stringsAsFactors = getOption("stringsAsFactors"),
                                 ...)
  stop("icd9Charlson requires a matrix or data frame of comorbidities")

#' @describeIn icd9Charlson Charlson scores from comorbidity matrix
#' @export
icd9Charlson.matrix <- function(x, visitId = NULL,
                                return.df = FALSE,
                                stringsAsFactors = getOption("stringsAsFactors"),
                                ...) {
  stop("todo")
}

#' @describeIn icd9Charlson Charlson scores from comorbidity data frame
#' @export
icd9Charlson.data.frame <- function(x, visitId = NULL,
                                    return.df = FALSE,
                                    stringsAsFactors = getOption("stringsAsFactors"),
                                    ...) {
  checkmate::assertDataFrame(x, min.rows = 1, min.cols = 2, col.names = "named")
  checkmate::assertCharacter(visitId, any.missing = FALSE, max.len = 1)
  checkmate::assertFlag(return.df)
  visitId <- getVisitId(x, visitId)
  tmp <- icd9ComorbidQuanDeyo(x, visitId, applyHierarchy = TRUE,
                              return.df = TRUE, ...)
  res <- icd9CharlsonComorbid(tmp, visitId = visitId, applyHierarchy = FALSE)

  # TODO someday it might be nice (like with comorbid.R) to recreate a factor
  # with the same levels for visitId if this is what is given to us.
  if (!return.df) return(res)
  out <- cbind(names(res),
               data.frame("Charlson" = unname(res)),
               stringsAsFactors = stringsAsFactors)
  names(out)[1] <- visitId
  out
}

#' @rdname icd9Charlson
#' @param applyHierarchy single logical value, default is FALSE. If TRUE, will
#'   drop DM if DMcx is present, etc.
#' @export
icd9CharlsonComorbid <- function(x, visitId = NULL, applyHierarchy = FALSE) {
  visitId <- getVisitId(x, visitId)
  stopifnot(ncol(x) - is.data.frame(x) == 17)
  weights <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
               2, 2, 2, 2,
               3, 6, 6)

  if (applyHierarchy) {
    x[,"DM"] <- x[, "DM"] & !x[, "DMcx"]
    x[, "LiverMild"] <- x[, "LiverMild"] & !x[, "LiverSevere"]
    x[, "Cancer"] <- x[, "Cancer"] & !x[, "Mets"]
  } else {
    stopifnot(!any(x[, "DM"] & x[, "DMcx"]))
    stopifnot(!any(x[, "LiverMild"] & x[, "LiverSevere"]))
    stopifnot(!any(x[, "Cancer"] & x[, "Mets"]))
  }
  if (is.data.frame(x)) {
    visitIdNames <- x[[visitId]]
    x <- as.matrix(x[, names(x) %nin% visitId])
    rownames(x) <- visitIdNames
  }
  rowSums(t(t(x) * weights))
}

#' @title count ICD codes or comorbidities for each patient
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
#'   cmb <- icd9ComorbidQuanDeyo(mydf, isShort = FALSE, return.df = TRUE)
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
  visitId <- getVisitId(x, visitId)
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
  visitId <- getVisitId(x, visitId)
visitId <- getVisitId(x, visitId)
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
  visitId <- getVisitId(x, visitId)
  checkmate::assertFlag(return.df)
  checkmate::assertFlag(aggregate)

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
