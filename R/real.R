#' Check whether ICD-9 codes exist
#'
#' This is different from syntactic validity: it looks up codes in the canonical
#' lists published by the CMS.
#'
#' The source data from CMS, and code to generate the R data are included in the
#' source tree of this package.
#' @param x vector if ICD codes to test whether defined in certain ICD code list
#' @template short_code
#' @template billable
#' @template dotdotdot
#' @return logical vector
#' @export
is_defined <- function(x, short_code = guess_short(x), ...) {
  UseMethod("is_defined")
}

#' @describeIn is_defined Check whether ICD-9 code are defined. (ICD-9-CM by
#'   default)
#' @export
#' @keywords internal
is_defined.icd9 <- function(x, short_code = guess_short(x),
                            billable = FALSE, ...) {
  stopifnot(is.factor(x) || is.character(x))
  stopifnot(is.logical(short_code), is.logical(billable))
  if (!short_code) x <- decimal_to_short.icd9(x)
  if (billable)
    is_billable.icd9cm(short_code = TRUE, as_char_no_warn(x))
  else
    x %in% icd.data::icd9cm_hierarchy[["code"]]
}

#' @describeIn is_defined Same for ICD-10-CM
#' @param nomatch integer value, passed to \code{match} default is 0. Setting
#'   this to \code{NA_integer_} would stop NA values being treated as undefined.
#' @param leaf New synonym for 'billable', which will be deprecated.
#' @export
#' @keywords internal
is_defined.icd10cm <- function(
  x,
  short_code = guess_short(x),
  billable = FALSE,
  leaf = billable,
  nomatch = 0L,
  ...
) {
  stopifnot(is.factor(x) || is.character(x))
  stopifnot(is.logical(short_code), is.logical(leaf))
  if (!short_code) x <- decimal_to_short(x)
  if (leaf)
    is_billable.icd10cm(x, short_code = short_code)
  else
    match(x, icd.data::icd10cm2016[["code"]], nomatch = nomatch, ...) > 0L
}

#' @describeIn is_defined Same for ICD-10, temporarily using ICD-10-CM until
#'   ICD-10 WHO is available in this package
#' @export
#' @keywords internal
is_defined.icd10 <- function(x, short_code = guess_short(x),
                             billable = FALSE, ...) {
  is_defined.icd10cm(x = x, short_code = short_code, billable = billable, ...)
}

#' @describeIn is_defined default method which will guess the ICD version (9
#'   vs 10, maybe WHO vs CM or other in the future) and dispatches again on that
#'   type.
#' @export
#' @keywords internal
is_defined.default <- function(x, short_code = guess_short(x), ...) {
  switch(
    guess_version(x, short_code = short_code),
    "icd9" = is_defined.icd9(x, short_code = short_code, ...),
    "icd10" = is_defined.icd10(x, short_code = short_code, ...),
    stop("ICD version not known")
  )
}

#' Select only defined ICD codes
#'
#' Return only those codes which are heading or leaf (billable), specifying
#' whether codes are all short-form or all decimal-form
#' @param x input vector or factor, possibly with an ICD class
#' @param short_code logical value, whether short-form ICD code
#' @template billable
#' @export
get_defined <- function(x, short_code = guess_short(x), billable = FALSE) {
  UseMethod("get_defined")
}

#' @export
#' @keywords internal
get_defined.default <- function(x, short_code = guess_short(x), ...) {
  icd_ver <- guess_version(x)
  if (icd_ver != "icd9")
    stop("testing whether ICD codes are defined is currently only implemented for ICD-9-CM")
  x[is_defined.icd9(x, short_code, ...)]
}

#' @export
#' @keywords internal
get_defined.icd9 <- function(x, short_code = guess_short(x), billable = FALSE) {
  x[is_defined.icd9(x, short_code = short_code, billable = billable)]
}

#' Determine whether codes are billable leaf-nodes
#'
#' Codes provided are compared to the most recent edition of the CMS list of
#' billable codes, or another edition if specified.
#' @param x input vector to test
#' @template short_code
#' @param icd9cm_edition single character string, default is "32" which is the
#'   latest release from CMS. Currently anything from "23" to "32" is accepted.
#'   Not numeric because there are possible cases with non-numeric names, e.g.
#'   revisions within one year, although none currently implemented.
#' @param icd10cm_edition single character string. ICD-10-CM editions are
#'   currently just the year of release.
#' @return logical vector of same length as input
#' @export
is_billable <- function(x, short_code = guess_short(x), ...) {
  UseMethod("is_billable")
}

#' @describeIn is_billable Which of the given ICD-9 codes are leaf nodes in
#'   ICD-9-CM. Currently assumes ICD-9 codes are ICD-9-CM
#' @export
#' @keywords internal
is_billable.icd9 <- function(x, short_code = guess_short(x),
                             icd9cm_edition = icd9cm_latest_edition(), ...) {
  is_billable.icd9cm(x = x, short_code = short_code, icd9cm_edition = icd9cm_edition)
}

#' @describeIn is_billable Which of the given ICD-10 codes are leaf nodes in
#'   ICD-10-CM. Currently assumes ICD-10 codes are ICD-10-CM
#' @param icd10cm_edition single character string. ICD-10-CM editions are
#'   currently just the year of release.
#' @export
#' @keywords internal
is_billable.icd10cm <- function(x, short_code = guess_short(x), icd10cm_edition = "2016", ...) {
  if (icd10cm_edition != "2016")
    stop("curretly only ICD-10-CM 2016 edition can be used to check for billable codes")
  if (!short_code)
    x <- decimal_to_short(x)

  x %in% icd.data::icd10cm2016[icd.data::icd10cm2016[["billable"]] == 1, "code"]
}

#' @describeIn is_billable Which of the given ICD-10 codes are leaf nodes in
#'   ICD-10-CM. Currently assumes ICD-10 codes are ICD-10-CM
#' @export
#' @keywords internal
is_billable.icd10 <- function(x, short_code = guess_short(x), icd10cm_edition = "2016", ...) {
  is_billable.icd10cm(x = x, short_code = short_code, icd10cm_edition = icd10cm_edition)
}

#' @describeIn is_billable Which of the given ICD-9 codes are leaf nodes in
#'   ICD-9-CM
#' @param nomatch integer value, passed to \code{match} default is 0.
#' @export
#' @keywords internal
is_billable.icd9cm <- function(
  x,
  short_code = guess_short(x),
  icd9cm_edition = icd9cm_latest_edition(),
  nomatch = 0L,
  ...) {
  stopifnot(is.atomic(x), is.logical(short_code))
  stopifnot(is.character(icd9cm_edition), length(icd9cm_edition) == 1L)
  stopifnot(is.integer(nomatch))
  if (!short_code)
    x <- decimal_to_short.icd9(x)
  match(x, icd.data::icd9cm_billable[[icd9cm_edition]][["code"]],
        nomatch = nomatch, ...) > 0L
}

#' @describeIn is_billable Which of the given ICD codes are leaf nodes in
#'   ICD version (9 vs 10) guessed from the codes themselves.
#' @export
#' @keywords internal
is_billable.default <- function(x, short_code = guess_short(x), ...) {
  switch(
    guess_version(x, short_code = short_code),
    "icd9" = is_billable.icd9(x, short_code = short_code, ...),
    "icd10" = is_billable.icd10(x, short_code = short_code, ...),
    stop("Unknown ICD version.")
  )
}

#' Get billable ICD codes
#'
#' Get billable ICD codes, implicitly, this refers to an ICD
#'   implementation which is specialized for a country, typically for billing,
#'   e.g. ICD-9-CM in the USA.
#' @param x input vector of ICD codes
#' @template short_code
#' @template invert
#' @param icd9cm_edition e.g. "32", not ICD-9 vs ICD-10
#' @export
get_billable <- function(...) {
  UseMethod("get_billable")
}

#' @describeIn get_billable Get billable ICD codes, guessing whether ICD-9
#'   or ICD-10, and code short vs decimal type.
#' @export
#' @keywords internal
get_billable.default <- function(x, short_code = guess_short(x), ...) {
  switch(
    guess_version(x, short_code = short_code),
    "icd9" = get_billable.icd9(x, short_code = short_code, ...),
    "icd10" = get_billable.icd10(x, short_code = short_code, ...),
    stop("Unknown ICD version.")
  )
}

#' @describeIn get_billable Get billable ICD-9-CM codes
#' @export
#' @keywords internal
get_billable.icd9cm <- function(
  x,
  short_code = guess_short(x),
  invert = FALSE,
  icd9cm_edition = icd9cm_latest_edition(),
  ...
) {
  stopifnot(is.atomic(x), is.logical(short_code), is.logical(invert))
  stopifnot(is.character(icd9cm_edition), length(icd9cm_edition) == 1L)
  # would be nicer to dispatch on short_code type here.
  icd9cm_get_billable(x = x,
                      short_code = short_code,
                      invert = invert,
                      icd9cm_edition = icd9cm_edition)
}

#' @describeIn get_billable Get billable ICD-9 codes, which is currently
#'   implemented assuming ICD-9-CM
#' @export
#' @keywords internal
get_billable.icd9 <- function(...)
  get_billable.icd9cm(...)

#' Get billable ICD-9-CM codes
#'
#' Return only those codes which are leaf codes in the hierarchy. In contrast to
#' \emph{defined} codes, these are considered \emph{billable} and not super-sets
#' of other codes. The majority have sub-decimal classifications, but some are
#' simply three-digit codes. This particular function is currently restricted to
#' ICD-9-CM codes.
#'
#' @param x data, e.g. character vector, \code{icd9}, \code{icd9cm}
#' @template short_code
#' @template invert
#' @keywords internal
#' @noRd
icd9cm_get_billable <- function(
  x,
  short_code = guess_short(x),
  invert = FALSE,
  icd9cm_edition = icd9cm_latest_edition()
) {
  stopifnot(is.atomic(x), is.logical(short_code), is.logical(invert))
  stopifnot(is.character(icd9cm_edition), length(icd9cm_edition) == 1L)
  x <- as.short_diag(as.icd9cm(x), short_code)
  x[is_billable.icd9cm(x, short_code = short_code, icd9cm_edition = icd9cm_edition) != invert]
}

#' @describeIn get_billable Get billable, i.e. leaf nodes from ICD-10-CM
#' @export
#' @keywords internal
#' @noRd
get_billable.icd10cm <- function(x, short_code = guess_short(x),
                                 invert = FALSE, icd10cm_edition = "2016",
                                 ...) {
  stopifnot(is.atomic(x), is.logical(short_code), is.logical(invert))
  stopifnot(is.character(icd10cm_edition), length(icd10cm_edition) == 1L)
  x <- as.short_diag(as.icd10cm(x), short_code)
  x[is_billable.icd10cm(unclass(x), short_code = short_code,
                        icd10cm_edition = icd10cm_edition) != invert]
}

#' @describeIn get_billable Get billable, i.e. leaf nodes from ICD-10-CM
#' @export
#' @keywords internal
#' @noRd
get_billable.icd10 <- function(x, short_code = guess_short(x),
                               invert = FALSE, icd10cm_edition = "2016", ...) {
  get_billable.icd10cm(x = x,
                       short_code = short_code,
                       invert = invert,
                       icd10cm_edition = icd10cm_edition)
}
