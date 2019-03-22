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
                            billable = FALSE, leaf = billable, ...) {
  stopifnot(is.factor(x) || is.character(x))
  stopifnot(is.logical(short_code), is.logical(billable))
  if (!short_code) x <- decimal_to_short.icd9(x)
  if (billable) {
    is_leaf.icd9cm(short_code = TRUE, as_char_no_warn(x))
  } else {
    x %in% icd.data::icd9cm_hierarchy[["code"]]
  }
}

#' @describeIn is_defined Same for ICD-10-CM
#' @param leaf New synonym for 'billable', which will be deprecated.
#' @export
#' @keywords internal
is_defined.icd10cm <- function(x,
                               short_code = guess_short(x),
                               billable = FALSE,
                               leaf = billable,
                               ...) {
  stopifnot(is.factor(x) || is.character(x))
  stopifnot(is.logical(short_code), is.logical(leaf))
  if (!short_code) x <- decimal_to_short(x)
  i <- icd_data_icd10cm_active()
  if (leaf) {
    is_leaf.icd10cm(x, short_code = short_code)
  } else {
    match(x,
      i[["code"]],
      nomatch = 0L, ...
    ) > 0L
  }
}

#' @describeIn is_defined Same for ICD-10, temporarily using ICD-10-CM until
#'   ICD-10 WHO is available in this package
#' @export
#' @keywords internal
is_defined.icd10 <- function(x,
                             short_code = guess_short(x),
                             billable = FALSE,
                             leaf = billable,
                             ...) {
  is_defined.icd10cm(x = x, short_code = short_code, leaf = leaf, ...)
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
get_defined <- function(x,
                        short_code = guess_short(x),
                        billable = FALSE,
                        leaf = billable) {
  UseMethod("get_defined")
}

#' @export
#' @keywords internal
get_defined.default <- function(x, short_code = guess_short(x), ...) {
  icd_ver <- guess_version(x)
  if (icd_ver != "icd9") {
    stop("testing whether ICD codes are defined is currently only implemented for ICD-9-CM")
  }
  x[is_defined.icd9(x, short_code, ...)]
}

#' @export
#' @keywords internal
get_defined.icd9 <- function(x,
                             short_code = guess_short(x),
                             billable = FALSE,
                             leaf = billable) {
  x[is_defined.icd9(x, short_code = short_code, leaf = leaf)]
}

#' Determine whether codes are billable leaf-nodes
#'
#' Codes provided are compared to the most recent edition of the CMS list of
#' billable codes, or another edition if specified.
#' @param x input vector to test
#' @template short_code
#' @template dotdotdot
#' @return logical vector of same length as input
#' @export
is_leaf <- function(x, short_code = guess_short(x), ...) {
  UseMethod("is_leaf")
}

#' @describeIn is_leaf Which of the given ICD-9 codes are leaf nodes in
#'   ICD-9-CM. Currently assumes ICD-9 codes are ICD-9-CM
#' @export
#' @keywords internal
is_leaf.icd9 <- function(x,
                         short_code = guess_short(x),
                         ...) {
  is_leaf.icd9cm(
    x = x,
    short_code = short_code
  )
}

#' @describeIn is_leaf Which of the given ICD-10 codes are leaf nodes in
#'   ICD-10-CM. Currently assumes ICD-10 codes are ICD-10-CM
#' @export
#' @keywords internal
is_leaf.icd10cm <- function(x,
                            short_code = guess_short(x),
                            ...) {
  if (!short_code) {
    x <- decimal_to_short(x)
  }
  # Workaround until next icd.data is on CRAN
  ia <- icd_data_icd10cm_active()
  leaf_name <- ifelse("leaf" %in% names(ia), "leaf", "billable")
  x %in% ia[ia[[leaf_name]] == 1L, "code"]
}

#' @describeIn is_leaf Which of the given ICD-10 codes are leaf nodes in
#'   ICD-10-CM. Currently assumes ICD-10 codes are ICD-10-CM
#' @export
#' @keywords internal
is_leaf.icd10 <- function(x,
                          short_code = guess_short(x),
                          ...) {
  is_leaf.icd10cm(x = x, short_code = short_code)
}

#' @describeIn is_leaf Which of the given ICD-9 codes are leaf nodes in
#'   ICD-9-CM
#' @export
#' @keywords internal
is_leaf.icd9cm <- function(x,
                           short_code = guess_short(x),
                           ...) {
  stopifnot(is.atomic(x), is.logical(short_code))
  if (!short_code) {
    x <- decimal_to_short.icd9(x)
  }
  m <- match(x,
    icd.data::icd9cm_hierarchy$code,
    nomatch = NA_integer_,
    ...
  )
  res <- rep_len(FALSE, length(x))
  not_na <- !is.na(m)
  res[not_na] <- icd.data::icd9cm_hierarchy[m[not_na], "billable"]
  res
}

#' @describeIn is_leaf Which of the given ICD codes are leaf nodes in
#'   ICD version (9 vs 10) guessed from the codes themselves.
#' @export
#' @keywords internal
is_leaf.default <- function(x, short_code = guess_short(x), ...) {
  switch(
    guess_version(x, short_code = short_code),
    "icd9" = is_leaf.icd9(x, short_code = short_code, ...),
    "icd10" = is_leaf.icd10(x, short_code = short_code, ...),
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
#' @template dotdotdot
#' @export
get_leaf <- function(...) {
  UseMethod("get_leaf")
}

#' @describeIn get_leaf Get billable ICD codes, guessing whether ICD-9
#'   or ICD-10, and code short vs decimal type.
#' @export
#' @keywords internal
get_leaf.default <- function(x, short_code = guess_short(x), ...) {
  switch(
    guess_version(x, short_code = short_code),
    "icd9" = get_leaf.icd9(x, short_code = short_code, ...),
    "icd10" = get_leaf.icd10(x, short_code = short_code, ...),
    stop("Unknown ICD version.")
  )
}

#' @describeIn get_leaf Get billable ICD-9-CM codes
#' @export
#' @keywords internal
get_leaf.icd9cm <- function(x,
                            short_code = guess_short(x),
                            invert = FALSE,
                            ...) {
  stopifnot(is.atomic(x), is.logical(short_code), is.logical(invert))
  x <- as.short_diag(as.icd9cm(x), short_code)
  x[is_leaf.icd9cm(x, short_code = short_code) != invert]
}

#' @describeIn get_leaf Get billable ICD-9 codes, which is currently
#'   implemented assuming ICD-9-CM
#' @export
#' @keywords internal
get_leaf.icd9 <- function(...)
  get_leaf.icd9cm(...)

#' @describeIn get_leaf Get billable/leaf nodes from ICD-10-CM
#' @export
#' @keywords internal
#' @noRd
get_leaf.icd10cm <- function(x,
                             short_code = guess_short(x),
                             invert = FALSE,
                             ...) {
  stopifnot(is.atomic(x), is.logical(short_code), is.logical(invert))
  x <- as.short_diag(as.icd10cm(x), short_code)
  x[is_leaf.icd10cm(unclass(x), short_code = short_code) != invert]
}

#' @describeIn get_leaf Get leaf nodes from ICD-10, currently defaults to ICD-10-CM for historic reasons.
#' @export
#' @keywords internal
#' @noRd
get_leaf.icd10 <- function(x,
                           short_code = guess_short(x),
                           invert = FALSE,
                           ...) {
  get_leaf.icd10cm(
    x = x,
    short_code = short_code,
    invert = invert
  )
}


#' Check whether a code is billable according to ICD-9-CM or ICD-10-CM
#'
#' Using the equivalent \code{\link[=is_leaf]{is_leaf()}} is preferred.
#' @inheritParams is_leaf
#' @seealso \code{\link[=get_leaf]{get_leaf()}}
#' @export
is_billable <- is_leaf
#' @describeIn is_billable Prefer 'leaf' to 'billable' for generality.
#' @export
is_billable.icd9 <- is_leaf.icd9cm
#' @describeIn is_billable Prefer 'leaf' to 'billable' for generality.
#' @export
is_billable.icd9cm <- is_leaf.icd9cm
#' @describeIn is_billable Prefer 'leaf' to 'billable' for generality.
#' @export
is_billable.icd10 <- is_leaf.icd10
#' @describeIn is_billable Prefer 'leaf' to 'billable' for generality.
#' @export
is_billable.icd10cm <- is_leaf.icd10cm
#' @describeIn is_billable Prefer 'leaf' to 'billable' for generality.
#' @export
is_billable.default <- is_leaf.default

#' Get the subset of codes that are billable according to ICD-9-CM or ICD-10-CM
#'
#' Using the equivalent \code{\link[=get_leaf]{get_leaf()}} is preferred.
#' @inheritParams get_leaf
#' @seealso \code{\link[=is_leaf]{is_leaf()}}
#' @export
get_billable <- get_leaf
#' @describeIn get_billable Prefer 'leaf' to 'billable' for generality.
#' @export
get_billable.icd9 <- get_leaf.icd9
#' @describeIn get_billable Prefer 'leaf' to 'billable' for generality.
#' @export
get_billable.icd9cm <- get_leaf.icd9cm
#' @describeIn get_billable Prefer 'leaf' to 'billable' for generality.
#' @export
get_billable.icd10 <- get_leaf.icd10
#' @describeIn get_billable Prefer 'leaf' to 'billable' for generality.
#' @export
get_billable.icd10cm <- get_leaf.icd10cm
#' @describeIn get_billable Prefer 'leaf' to 'billable' for generality.
#' @export
get_billable.default <- get_leaf.default
