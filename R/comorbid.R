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
#' @template isShort
#' @param isShortMapping Same as isShort, but applied to \code{icd9Mapping}
#'   instead of \code{icd9df}. All the codes in a mapping should be of the same
#'   type, i.e. short or decimal.
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
                         isShortMapping = icd9GuessIsShort(icd9Mapping),
                         return.df = FALSE) {
  # TODO: allow factors for icd9df fields
  checkmate::checkDataFrame(icd9df, min.cols = 2)
  checkmate::checkList(icd9Mapping, types = "character", any.missing = FALSE, min.len = 1)
  #visitId <- as.character(visitId)
  checkmate::checkString(icd9Field)
  checkmate::checkLogical(isShort, any.missing = FALSE, len = 1)
  checkmate::checkLogical(isShortMapping, any.missing = FALSE, len = 1)
  stopifnot(visitId %in% names(icd9df), icd9Field %in% names(icd9df))

  if (!isShort)
    icd9df[[icd9Field]] <- icd9DecimalToShort(icd9df[[icd9Field]])

  icd9Mapping <- lapply(icd9Mapping, asCharacterNoWarn)

  if (!isShortMapping)
    icd9Mapping <- lapply(icd9Mapping, icd9DecimalToShort)

  # new stragegy is to start with a factor for the icd codes in icd9df, recode
  # (and drop superfluous) icd codes in the mapping, then do very fast match on
  # integer without need for N, V or E distinction. Char to factor conversion in
  # R is very fast.

  if (!is.factor(icd9df[[icd9Field]]))
    icd9df[[icd9Field]] <- as.factor(icd9df[[icd9Field]])

  # we need to convert to string and group these anyway, and much easier and pretty quick to do it here:
  icd9VisitWasFactor <- is.factor(icd9df[[visitId]])
  if (icd9VisitWasFactor) ivLevels <- levels(icd9df[[visitId]])
  icd9df[[visitId]] <- asCharacterNoWarn(icd9df[[visitId]])

  # again, R is very fast at creating factors from a known set of levels
  icd9Mapping <- lapply(icd9Mapping, function(x) {
    f <- factor(x, levels(icd9df[[icd9Field]]))
    f[!is.na(f)]
  })

  # now we also benefit from only have factor levels in the mapping which appear
  # in the diagnoses we are going to be examining. From the C++ perspective, we
  # can now do pure integer matching for icd9 codes. Only string manip becomes
  # (optionally) defactoring the visitId for the matrix row names.

  # return via call to the C++ function:
  #icd9ComorbidShort(icd9df, icd9Mapping, visitId, icd9Field)
  if (return.df) {
    mat <- icd9ComorbidShortCpp(icd9df, icd9Mapping, visitId, icd9Field)
    if (icd9VisitWasFactor)
      rownm <- factor(x = rownames(mat), levels = ivLevels)
    else
      rownm <- rownames(mat)
    df.out <- cbind(rownm, as.data.frame(mat), stringsAsFactors = icd9VisitWasFactor)
    names(df.out)[1] <- visitId
    # perhaps leave (duplicated) rownames which came from the matrix:
    rownames(df.out) <- NULL
    return(df.out)
  }
  icd9ComorbidShortCpp(icd9df, icd9Mapping, visitId, icd9Field)
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbidShort <- function(...) {
  icd9Comorbid(..., isShort = TRUE)
}

#' @rdname icd9Comorbid
#' @param abbrevNames  single locical value that defaults to \code{TRUE}, in
#'   which case the ishorter human-readable names stored in e.g.
#'   \code{ahrqComorbidNamesAbbrev} are applied to the data frame column names.
#' @param applyHierarchy single logical value that defaults to \code{TRUE}, in
#'   which case the hierarchy defined for the mapping is applied. E.g. in
#'   Elixhauser, you can't have uncomplicated and complicated diabetes both
#'   flagged.
#' @export
icd9ComorbidAhrq <- function(..., abbrevNames = TRUE,
                             applyHierarchy = TRUE) {

  cbd <- icd9Comorbid(..., icd9Mapping = icd9::ahrqComorbid)
  if (applyHierarchy) {

    # Use >0 rather than logical - apparently faster, and future proof against
    # change to binary from logical values in the matirx.
    cbd[cbd[, "Mets"] > 0, "Tumor"] <- FALSE
    cbd[cbd[, "DMcx"] > 0, "DM"] <- FALSE
    cbd[, "HTN"] <- (cbd[, "HTN"] + cbd[, "HTNcx"]) > 0

    cbd <- cbd[, -which(rownames(cbd) %nin% "HTNcx")] # drop HTNcx

    if (abbrevNames)
      colnames(cbd)[-1] <- icd9::ahrqComorbidNamesAbbrev
    else
      colnames(cbd)[-1] <- icd9::ahrqComorbidNames
  } else {
    if (abbrevNames)
      colnames(cbd)[-1] <- icd9::ahrqComorbidNamesHtnAbbrev
    else
      colnames(cbd)[-1] <- icd9::ahrqComorbidNamesHtn
  }
  cbd
}

#' @rdname icd9Comorbid
#' @description For Charlson/Deyo comorbidities, strictly speaking, there is no
#'   dropping of more e.g. uncomplicated DM if complicated DM exists, however,
#'   this is probaably useful, in general and is essential when calculating the
#'   Charlson score.
#' @export
icd9ComorbidQuanDeyo <- function(..., abbrevNames = TRUE,
                                 applyHierarchy = TRUE) {
  cbd <- icd9Comorbid(..., icd9Mapping = icd9::quanDeyoComorbid)
  if (applyHierarchy) {
    # Use >0 rather than logical - apparently faster, and future proof against
    # change to binary from logical values in the matirx.
    cbd[cbd[, "Mets"] > 0, "Cancer"] <- FALSE
    cbd[cbd[, "DMcx"] > 0, "DM"] <- FALSE
    cbd[cbd[, "LiverSevere"] > 0, "LiverMild"] <- FALSE
  }
  if (abbrevNames)
    names(cbd)[-1] <- icd9::charlsonComorbidNamesAbbrev
  else
    names(cbd)[-1] <- icd9::charlsonComorbidNames

  cbd
}

#' @rdname icd9Comorbid
#' @export
icd9ComorbidQuanElix <- function(..., abbrevNames = TRUE,
                                 applyHierarchy = TRUE) {
  cbd <- icd9Comorbid(..., icd9Mapping = icd9::quanElixComorbid)
  if (applyHierarchy) {
    cbd[cbd[, "Mets"] > 0, "Tumor"] <- FALSE
    cbd[cbd[, "DMcx"] > 0, "DM"] <- FALSE
    # combine HTN
    cbd[, "HTN"] <- (cbd[, "HTN"] + cbd[, "HTNcx"]) > 0
    cbd[, "HTNcx"] <- NULL

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
icd9ComorbidElix <- function(..., abbrevNames = TRUE, applyHierarchy = TRUE) {
  cbd <- icd9Comorbid(..., icd9Mapping = icd9::elixComorbid)
  if (applyHierarchy) {
    cbd[cbd[, "Mets"] > 0, "Tumor"] <- FALSE
    cbd[cbd[, "DMcx"] > 0, "DM"] <- FALSE
    cbd[, "HTN"] <- (cbd[, "HTN"] + cbd[, "HTNcx"]) > 0
    cbd[, "HTNcx"] <- NULL
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

#' @rdname icd9Comorbid
#' @param ... arguments passed to the corresponding function from the alias.
#'   E.g. all the arguments passed to \code{icd9ComorbiditiesAhrq} are passed on
#'   to \code{icd9ComorbidAhrq}
#' @export
icd9Comorbidities <- function(...) icd9Comorbid(...)

#' @rdname icd9Comorbid
#' @export
icd9ComorbiditiesAhrq <- function(...) icd9ComorbidAhrq(...)

#' @rdname icd9Comorbid
#' @export
icd9ComorbiditiesElixHauser <- function(...) icd9ComorbidElix(...)

#' @rdname icd9Comorbid
#' @export
icd9ComorbiditiesQuanDeyo <- function(...) icd9ComorbidQuanDeyo(...)

#' @rdname icd9Comorbid
#' @export
icd9ComorbiditiesQuanElixhauser <- function(...) icd9ComorbidQuanElix(...)

#' @title show the difference between two comorbidity mappings
#' @description Compares two comorbidity:icd9 code mappings. The results are
#'   returned invisibly as a list. Only those comorbidities with (case
#'   sensitive) overlapping names are compared.
#' @param x list of character vectors
#' @param y list of character vectors
#' @param names character vector of the comorbidity names
#' @param x.names character vector of the comorbidity names from \code{x} to
#'   compare
#' @param y.names character vector of the comorbidity names from \code{y} to
#'   compare
#' @param show single logical value. The default is \code{TRUE} which causes a
#'   report to be printed.
#' @param explain single logical value. The default is \code{TRUE} which means
#'   the differing codes are attempted to be reduced to their parent codes, in
#'   order to give a more succinct summary.
#' @examples
#' icd9DiffComorbid(elixComorbid, ahrqComorbid, "CHF")
#' \dontrun{
#' # give full report on all comorbidities for these mappings
#' icd9DiffComorbid(elixComorbid, ahrqComorbid)
#' }
#' @return A list, each item of which is another list containing the
#'   intersections and both asymmetric differences.
#' @export
icd9DiffComorbid <- function(x, y, names = NULL, x.names = NULL, y.names = NULL,
                             show = TRUE, explain = TRUE) {
  checkmate::checkList(x, min.len = 1, any.missing = FALSE)
  checkmate::checkList(y, min.len = 1, any.missing = FALSE,
                       types = c("character", "numeric", "integer"))
  checkmate::checkLogical(show, any.missing = FALSE, len = 1)
  checkmate::checkLogical(explain, any.missing = FALSE, len = 1)
  stopifnot(all(x.names %in% names(x)), all(y.names %in% names(y)))

  lapply(x, function(z) stopifnot(is.character(z)))
  lapply(y, function(z) stopifnot(is.character(z)))

  if (!is.null(names) && (!is.null(x.names) | !is.null(y.names)))
    stop("if 'names' is specified, 'x.names' and 'y.names' should not be")

  if (!is.null(names)) {
    x.names <- names
    y.names <- names
  }
  if (is.null(x.names)) x.names <- names(x)
  if (is.null(y.names)) y.names <- names(y)

  common.names <- intersect(x.names, y.names)

  x.title <- deparse(substitute(x))
  y.title <- deparse(substitute(y))

  out <- list();

  for (n in common.names) {
    both <- intersect(x[[n]], y[[n]])
    only.x <- setdiff(x[[n]], y[[n]])
    only.y <- setdiff(y[[n]], x[[n]])
    out[[n]] <- list(both = both, only.x = only.x, only.y = only.y)
    if (show) {
      cat(sprintf("Comorbidity %s: ", n))
      if (length(both) == 0) {
        cat("no common codes. ")
      }
      if (length(only.x) == 0 && length(only.y) == 0) {
        cat("match.\n")
        next
      }
      if (length(only.x) > 0) {
        cat(sprintf("\n%s has %d codes not in %s. First few are: ",
                    x.title, length(only.x), y.title))
        lapply(icd9Explain(only.x, doCondense = TRUE, brief = TRUE, warn = FALSE)[1:5],
               function(s) if (!is.na(s)) cat(sprintf("'%s' ", s)))

        #lapply(only.x[1:5], function(s) if (!is.na(s)) cat(sprintf("%s ", s)))
      }
      if (length(only.y) > 0) {
        cat(sprintf("\n%s has %d codes not in %s. First few are: ",
                    y.title, length(only.y), x.title))
        lapply(icd9Explain(only.y, doCondense = TRUE, brief = TRUE, warn = FALSE)[1:5],
               function(s) if (!is.na(s)) cat(sprintf("'%s' ", s)))
        #lapply(only.y[1:5], function(s) if (!is.na(s)) cat(sprintf("%s ", s)))
      }
      cat("\n")
    }
  }
  if (show) {
    cmb_only_x <- setdiff(x.names, y.names)
    cmb_only_y <- setdiff(y.names, x.names)

    if (length(cmb_only_x) > 0) {
      cat(sprintf("Comorbidities only defined in %s are: ", x.title))
      lapply(cmb_only_x, function(s) cat(sprintf("%s ", s)))
      cat("\n")
    }

    if (length(cmb_only_y) > 0) {
      cat(sprintf("Comorbidities only defined in %s are: ", y.title))
      lapply(cmb_only_y, function(s) cat(sprintf("%s ", s)))
      cat("\n")
    }
  }
  invisible(out)
}
