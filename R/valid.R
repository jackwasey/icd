utils::globalVariables(
  c(
    "re_icd10_any",
    "re_icd10_decimal",
    "re_icd10_short",
    "re_icd10cm_any",
    "re_icd10cm_decimal",
    "re_icd10cm_short",
    "re_icd10_major",
    "re_icd10cm_major",
    "re_icd10_major_bare",
    "re_icd10cm_major_bare",
    "re_icd9_any",
    "re_icd9_short",
    "re_icd9_decimal_strict_bare",
    "re_icd9_decimal_bare",
    "re_icd9_decimal",
    "re_icd9_any_e",
    "re_icd9_any_v",
    "re_icd9_any_n",
    "re_icd9_short_e",
    "re_icd9_short_v",
    "re_icd9_short_n",
    "re_icd9_decimal_e",
    "re_icd9_decimal_v",
    "re_icd9_decimal_n",
    "re_icd9_decimal_e_strict_bare",
    "re_icd9_decimal_v_strict_bare",
    "re_icd9_decimal_n_strict_bare",
    "re_icd9_decimal_e_bare",
    "re_icd9_decimal_v_bare",
    "re_icd9_decimal_n_bare",
    "re_icd9_minor_e",
    "re_icd9_minor_nv",
    "re_icd9_major_strict_bare",
    "re_icd9_major_strict",
    "re_icd9_major_bare",
    "re_icd9_major",
    "re_icd9_major_e_strict",
    "re_icd9_major_e",
    "re_icd9_major_v_strict",
    "re_icd9_major_v",
    "re_icd9_major_n_strict",
    "re_icd9_major_n"
  )
)

# nolint start

# Update regular expression to limit by start and end, with or without white
# space
justify_re <- function(x, whitespace_ok = FALSE) {
  stopifnot(is.character(x), length(x) == 1)
  stopifnot(is.logical(whitespace_ok), length(whitespace_ok) == 1)
  if (whitespace_ok) {
    paste0("^[[:space:]]*", x, "[[:space:]]*$")
  } else {
    paste0("^", x, "$")
  }
}

# Allow white space
jws <- function(x)
  justify_re(x, whitespace_ok = TRUE)

# contain any | options within a regular expression, applies to ICD codes
# without ^ and $
wrap_re <- function(x)
  paste0("(?:", x, ")")

#' Put ICD validation regular expressions in the \code{icd:::} name space
#' @param env target environment to save the data
#' @keywords internal sysdata data
#' @noRd
set_re_globals <- function(env = parent.frame()) {
  re_icd9_major_n <- "[[:digit:]]{1,3}"
  re_icd9_major_n_strict <- "[[:digit:]]{3}"
  re_icd9_major_v <- "[Vv](?:0[1-9]|[1-9][[:digit:]]?)"
  re_icd9_major_v_strict <- "V(?:0[1-9]|[1-9][[:digit:]])"
  re_icd9_major_e <- "[Ee][[:digit:]]{1,3}"
  re_icd9_major_e_strict <- "E[[:digit:]]{3}"
  re_icd9_major <- paste0(c(
    jws(re_icd9_major_n),
    jws(re_icd9_major_v),
    jws(re_icd9_major_e)
  ),
  collapse = "|"
  )
  re_icd9_major_bare <- wrap_re(paste0(c(
    re_icd9_major_n,
    re_icd9_major_v,
    re_icd9_major_e
  ),
  collapse = "|"
  ))
  re_icd9_major_strict <- paste0(c(
    jws(re_icd9_major_n_strict),
    jws(re_icd9_major_v_strict),
    jws(re_icd9_major_e_strict)
  ),
  collapse = "|"
  )
  re_icd9_major_strict_bare <- wrap_re(paste0(c(
    re_icd9_major_n_strict,
    re_icd9_major_v_strict,
    re_icd9_major_e_strict
  ),
  collapse = "|"
  ))
  re_icd9_minor_nv <- "[[:digit:]]{1,2}"
  re_icd9_minor_e <- "[[:digit:]]{1}"
  re_icd9_decimal_n_bare <- paste0(
    re_icd9_major_n, "(?:\\.(?:",
    re_icd9_minor_nv, ")?)?"
  )
  re_icd9_decimal_v_bare <- paste0(
    re_icd9_major_v, "(?:\\.(?:",
    re_icd9_minor_nv, ")?)?"
  )
  re_icd9_decimal_e_bare <- paste0(
    re_icd9_major_e, "(?:\\.(?:",
    re_icd9_minor_e, ")?)?"
  )
  re_icd9_decimal_n_strict_bare <- paste0(
    re_icd9_major_n_strict, "(?:\\.(?:",
    re_icd9_minor_nv, ")?)?"
  )
  re_icd9_decimal_v_strict_bare <- paste0(
    re_icd9_major_v_strict, "(?:\\.(?:",
    re_icd9_minor_nv, ")?)?"
  )
  re_icd9_decimal_e_strict_bare <- paste0(
    re_icd9_major_e_strict, "(?:\\.(?:",
    re_icd9_minor_e, ")?)?"
  )
  re_icd9_decimal_n <- jws(re_icd9_decimal_n_bare)
  re_icd9_decimal_v <- jws(re_icd9_decimal_v_bare)
  re_icd9_decimal_e <- jws(re_icd9_decimal_e_bare)
  re_icd9_short_n <- jws(
    paste0(re_icd9_major_n, "(?:", re_icd9_minor_nv, ")?")
  )
  re_icd9_short_v <- jws(
    paste0(re_icd9_major_v, "(?:", re_icd9_minor_nv, ")?")
  )
  re_icd9_short_e <- jws(
    paste0(re_icd9_major_e, "(?:", re_icd9_minor_e, ")?")
  )
  re_icd9_any_n <- jws(
    paste0(re_icd9_major_n, "\\.?(?:", re_icd9_minor_nv, ")?")
  )
  re_icd9_any_v <- jws(
    paste0(re_icd9_major_v, "\\.?(?:", re_icd9_minor_nv, ")?")
  )
  re_icd9_any_e <- jws(
    paste0(re_icd9_major_e, "\\.?(?:", re_icd9_minor_e, ")?")
  )
  re_icd9_decimal <- paste0(c(
    re_icd9_decimal_n,
    re_icd9_decimal_v,
    re_icd9_decimal_e
  ),
  collapse = "|"
  )
  re_icd9_decimal_bare <- paste0(c(
    re_icd9_decimal_n_bare,
    re_icd9_decimal_v_bare,
    re_icd9_decimal_e_bare
  ),
  collapse = "|"
  )
  re_icd9_decimal_strict_bare <- paste0(c(
    re_icd9_decimal_n_strict_bare,
    re_icd9_decimal_v_strict_bare,
    re_icd9_decimal_e_strict_bare
  ),
  collapse = "|"
  )
  re_icd9_short <- paste0(c(
    re_icd9_short_n,
    re_icd9_short_v,
    re_icd9_short_e
  ),
  collapse = "|"
  )
  re_icd9_any <- paste0(c(
    re_icd9_any_n,
    re_icd9_any_v,
    re_icd9_any_e
  ),
  collapse = "|"
  )
  re_icd10cm_major_bare <- "[[:alpha:]][[:digit:]][[:alnum:]]"
  # use slightly broader definition for generic:
  re_icd10_major_bare <- re_icd10cm_major_bare
  re_icd10cm_major <- jws(re_icd10cm_major_bare)
  re_icd10_major <- jws(re_icd10_major_bare)
  re_icd10cm_short <- paste0(re_icd10_major_bare, "[[:alnum:]]{0,4}")
  re_icd10cm_decimal <- paste0(re_icd10_major_bare, "(?:\\.[[:alnum:]]{0,4})?")
  re_icd10cm_any <- paste0(re_icd10_major_bare, "\\.?[[:alnum:]]{0,4}")
  re_icd10_short <- re_icd10cm_short
  re_icd10_decimal <- re_icd10cm_decimal
  re_icd10_any <- re_icd10cm_any
  cur_env <- environment()
  for (re in ls(envir = cur_env, pattern = "re_.+"))
    assign(re, get(re, envir = cur_env), envir = env)
  list(
    re_icd10_any,
    re_icd10_decimal,
    re_icd10_short,
    re_icd10cm_any,
    re_icd10cm_decimal,
    re_icd10cm_short,
    re_icd10_major,
    re_icd10cm_major,
    re_icd10_major_bare,
    re_icd10cm_major_bare,
    re_icd9_any,
    re_icd9_short,
    re_icd9_decimal_strict_bare,
    re_icd9_decimal_bare,
    re_icd9_decimal,
    re_icd9_any_e,
    re_icd9_any_v,
    re_icd9_any_n,
    re_icd9_short_e,
    re_icd9_short_v,
    re_icd9_short_n,
    re_icd9_decimal_e,
    re_icd9_decimal_v,
    re_icd9_decimal_n,
    re_icd9_decimal_e_strict_bare,
    re_icd9_decimal_v_strict_bare,
    re_icd9_decimal_n_strict_bare,
    re_icd9_decimal_e_bare,
    re_icd9_decimal_v_bare,
    re_icd9_decimal_n_bare,
    re_icd9_minor_e,
    re_icd9_minor_nv,
    re_icd9_major_strict_bare,
    re_icd9_major_strict,
    re_icd9_major_bare,
    re_icd9_major,
    re_icd9_major_e_strict,
    re_icd9_major_e,
    re_icd9_major_v_strict,
    re_icd9_major_v,
    re_icd9_major_n_strict,
    re_icd9_major_n
  )
}
# and put these in the package namespace
set_re_globals()
# nolint end

#' Check whether ICD-9 codes are syntactically valid
#'
#' These functions check whether the given ICD codes look correct, but do not
#' check whether they have actual definitions in any particular ICD scheme. For
#' that, see \code{\link{is_defined}}.
#' @section Three-digit validation: \code{icd9_is_valid_major} validates just
#'   the 'major' three-digit part of an ICD-9 code. This can in fact be provided
#'   as a numeric, since there is no ambiguity. Numeric-only codes should be one
#'   to three digits, V codes are followed by one or two digits, and E codes
#'   always by three digits between 800 and 999.
#' @details Leading zeroes in the decimal form are not ambiguous. Although
#'   integer ICD-9 codes could be intended by the user, there is a difference
#'   between 100, 100.0, 100.00. Therefore a warning is given if a numeric value
#'   is provided.
#' @section \code{NA} values: Currently, there is a limitation on NA values.
#'   Calling with \code{NA} (which is a logical vector of length one by default)
#'   fails, because it is not a string. This is rarely of significance in real
#'   life, since the NA will be part of a character vector of codes, and will
#'   therefore be cast already to \code{NA_character}
#' @section Class: S3 class of on object in R is just a vector. Attributes are
#'   lost with manipulation, with the exception of class: therefore, elements of
#'   the class vector are used to describe features of the data. If these are
#'   not present, the user may specify (e.g. decimal vs short_code type, ICD-9
#'   vs ICD-10 WHO), but if they are, the correct functions are called without
#'   any guess work. There is overlap between sets with combinations of
#'   \code{short_code} or \code{decimal_code}, and ICD-9 or ICD-10 codes, so
#'   guessing is never going to be perfect.
#' @param x An ICD-9 or 10 code. If the class is set to \code{'icd9'},
#'   \code{'icd10'}, \code{'icd10cm'} etc then perform appropriate validation.
#' @template mjr
#' @template whitespace_ok
#' @template dotdotdot
#' @seealso \url{http://www.stata.com/users/wgould/icd9/icd9.hlp}
#'   \url{http://www.sascommunity.org/wiki/Validate_the_format_of_ICD-9_codes}
#' @family ICD9 validation
#' @return logical vector with \code{TRUE} or \code{FALSE} for each ICD code
#'   provided according to its validity
#' @examples
#' is_valid(as.icd9(c(
#'   "", "1", "22", "333", "4444", "123.45", "V",
#'   "V2", "V34", "V567", "E", "E1", "E70", "E"
#' )))
#' # internal function:
#' icd:::is_valid_major(c(
#'   "", "1", "22", "333", "4444", "123.45", "V",
#'   "V2", "V34", "V567", "E", "E1", "E70", "E"
#' ))
#' @export
is_valid <- function(x, ...)
  UseMethod("is_valid")

icd_valid_worker <- function(x, whitespace_ok, regex, regex_no_ws = regex) {
  assert_flag(whitespace_ok)
  if (length(x) == 0) return(logical())
  assert_fac_or_char(x)
  if (whitespace_ok) {
    na_to_false(grepl(jws(regex), x, perl = TRUE))
  } else {
    na_to_false(grepl(justify_re(regex_no_ws), x, perl = TRUE))
  }
}

#' @describeIn is_valid_major Test whether an ICD code is of major type,
#'   which at present assumes ICD-9 format. Converts to character than calls
#'   \code{is_valid.character}
#' @export
#' @keywords internal
is_valid.default <- function(x, short_code = guess_short(x), ...) {
  switch(
    guess_version.character(as_char_no_warn(x), short_code = short_code),
    "icd9" = is_valid.icd9(x, short_code = short_code, ...),
    "icd10" = is_valid.icd10(x, short_code = short_code, ...),
    stop("ICD type not known")
  )
}

#' @describeIn is_valid Test whether generic ICD-10 code is valid
#' @export
#' @keywords internal
is_valid.icd10 <- function(x, short_code = guess_short(x),
                           whitespace_ok = TRUE, ...) {
  assert_flag(short_code)
  # SOMEDAY: check whether code has 'year' attribute. This is maybe more for
  # testing 'realness' start with a broad regular expression
  if (short_code) {
    grepl(pattern = justify_re(re_icd10_short), trimws(x), perl = TRUE)
  } else {
    grepl(pattern = justify_re(re_icd10_decimal), trimws(x), perl = TRUE)
  }
}

#' @describeIn is_valid Test whether generic ICD-10 code is valid
#' @export
#' @keywords internal
is_valid.icd9 <- function(x, short_code = guess_short(x),
                          whitespace_ok = TRUE, ...) {
  if (short_code) {
    icd9_is_valid_short(x, whitespace_ok = whitespace_ok)
  } else {
    icd9_is_valid_decimal(x, whitespace_ok = whitespace_ok)
  }
}

icd9_is_valid_decimal <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd9_decimal)

icd9_is_valid_short <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd9_short)

icd9_is_valid_short_n <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd9_short_n)

icd9_is_valid_short_v <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd9_short_v)

icd9_is_valid_short_e <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd9_short_e)

icd9_is_valid_decimal_n <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd9_decimal_n)

icd9_is_valid_decimal_v <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd9_decimal_v)

icd9_is_valid_decimal_e <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd9_decimal_e)

#' Test whether an ICD code is major
#'
#' Codes without real or implied decimal place return \code{TRUE}
#' @param x vector of ICD codes
#' @return logical vector of same length as input, with \code{TRUE} when a code
#'   is a major (not necessarily a real one)
#' @keywords internal
is_valid_major <- function(x, whitespace_ok = TRUE) {
  assert_flag(whitespace_ok)
  UseMethod("is_valid_major")
}

#' @describeIn is_valid_major Test whether an ICD code is of major type,
#'   either ICD-9 or ICD-10
#' @export
#' @keywords internal
is_valid_major.default <- function(x, whitespace_ok = TRUE) {
  is_valid_major.icd9(x, whitespace_ok) |
    is_valid_major.icd10(x, whitespace_ok)
}

#' @describeIn is_valid_major Test whether an ICD-9 code is of major type.
#' @export
#' @keywords internal
is_valid_major.icd9 <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd9_major, re_icd9_major_bare)

#' @describeIn is_valid_major Test whether an ICD-9 code is of major type.
#' @export
#' @keywords internal
is_valid_major.icd10 <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd10_major, re_icd10_major_bare)

#' @rdname is_valid_major
#' @keywords internal
icd9_is_valid_major_n <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd9_major_n)

#' @rdname is_valid_major
#' @keywords internal
icd9_is_valid_major_v <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd9_major_v)

#' @rdname is_valid_major
#' @keywords internal
icd9_is_valid_major_e <- function(x, whitespace_ok = TRUE)
  icd_valid_worker(x, whitespace_ok, re_icd9_major_e)

#' @describeIn is_valid Validate a mapping of ICD codes to comorbidities.
#' @export
#' @keywords internal
is_valid.comorbidity_map <- function(x, short_code, ...) {
  assert_list(x,
    types = "character",
    min.len = 1, unique = TRUE, names = "named"
  )
  assert_flag(short_code)
  all(unlist(
    lapply(x, FUN = function(y) is_valid(y, short_code = short_code)),
    use.names = FALSE
  ))
}

#' invalid subset of decimal or short_code ICD-9 codes
#'
#' Given vector of short_code or decimal ICD-9 codes, return (in the
#'   same format) those codes which are valid or invalid. Useful for generating
#'   error messages with the faulty codes if validation fails.
#' @param x input vector of ICD codes
#' @template short_code
#' @keywords manip
#' @family ICD-9 validation
#' @export
get_valid <- function(x, short_code = guess_short(x))
  UseMethod("get_valid")

#' @describeIn get_valid get valid ICD codes from character vector, guessing ICD
#'   version
#' @export
#' @keywords internal
get_valid.character <- function(x, short_code = guess_short(x)) {
  switch(
    guess_version.character(x, short_code = short_code),
    "icd9" = get_valid.icd9(x, short_code = short_code),
    "icd10" = get_valid.icd10(x, short_code = short_code),
    stop("ICD type not known")
  )
}

#' @describeIn get_valid Get valid ICD-9 codes
#' @export
#' @keywords internal
get_valid.icd9 <- function(x, short_code = guess_short(x))
  x[is_valid.icd9(x, short_code = short_code)]

#' @describeIn get_valid Get valid ICD-10 codes, currently based on ICD-10-CM
#'   structure, which is a super-set of WHO ICD-10
#' @export
#' @keywords internal
get_valid.icd10 <- function(x, short_code = guess_short(x)) {
  x[is_valid.icd10(x, short_code = short_code)]
}

#' @describeIn get_valid Get valid ICD-10-CM codes
#' @export
#' @keywords internal
get_valid.icd10cm <- function(x, short_code = guess_short(x))
  x[is_valid.icd10(x, short_code = short_code)]

#' Get invalid ICD codes
#'
#' Returns subset of codes which are not in valid short_code or
#'   decimal format.
#' @export
get_invalid <- function(...)
  UseMethod("get_invalid")

#' @describeIn get_invalid Default method when ICD version or short versus
#'   decimal not known.
#' @export
#' @keywords internal
get_invalid.default <- function(x, short_code = guess_short(x), ...) {
  switch(
    guess_version.character(as_char_no_warn(x), short_code = short_code),
    "icd9" = get_invalid.icd9(x, short_code = short_code),
    "icd10" = get_invalid.icd10(x, short_code = short_code),
    stop("ICD type not known")
  )
}

#' @describeIn get_invalid Get invalid ICD-9 codes from vector of codes
#' @param x vector of ICD codes, or list of vectors of ICD codes forming a
#'   comorbidity map
#' @export
#' @keywords internal
get_invalid.icd9 <- function(x, short_code = guess_short(x), ...)
  x[!is_valid.icd9(x, short_code = short_code)]

#' @describeIn get_invalid Get invalid ICD-10 codes from vector of codes
#' @export
#' @keywords internal
get_invalid.icd10 <- function(x, short_code = guess_short(x), ...) {
  # this seems like boilerplate code, but avoids infinite recursion
  x[!is_valid.icd10(x, short_code = short_code)]
}

#' @describeIn get_invalid Get invalid elements of a comorbidity map
#' @export
#' @keywords internal
get_invalid.comorbidity_map <- function(x, short_code = guess_short(x), ...) {
  class(x) <- class(x)[class(x) != "comorbidity_map"]
  Filter(length, lapply(x, FUN = get_invalid, short_code = short_code))
}

#' Get major part of an ICD code
#'
#' Returns major component of codes, i.e. the part before the decimal,
#' or where the decimal would be.
#' @keywords internal
#' @noRd
get_major <- function(x)
  UseMethod("get_major")

#' @describeIn get_major Get major part of an ICD-10 code
#' @details For ICD-10, this is an initial implementation. If speed needed, then
#'   can re-use C++ ICD-9 version: just grabbing the first three characters,
#'   after all, and this is much easier in ICD-10 then ICD-9
#' @keywords internal
#' @export
#' @noRd
get_major.icd10 <- function(x)
  substr(trimws(x), 1L, 3L)

#' Check whether a code is major
#' @param icd character vector of ICD codes.
#' @keywords internal
#' @noRd
is_major <- function(x)
  UseMethod("is_major")

#' @describeIn is_major Default method which guesses version
#' @keywords internal
#' @export
#' @noRd
is_major.default <- function(x) {
  switch(
    guess_version(x),
    "icd9" = is_major.icd9(x),
    "icd9cm" = is_major.icd9(x),
    "icd10" = is_major.icd10(x),
    "icd10cm" = is_major.icd10(x),
    stop("ICD version not known")
  )
}

#' @describeIn is_major check whether a code is an ICD-10 major
#' @keywords internal
#' @export
#' @noRd
is_major.icd10 <- function(x) {
  assert_character(x)
  # if not know whether ICD-10-CM, then use broader definition
  is_major.icd10cm(x)
}

#' @describeIn is_major check whether a code is an ICD-10-CM major.
#'   Currently uses \code{stringr} which uses \code{stringi} which should be
#'   quite fast, but does suffer from handling Unicode, locales, etc.
#' @keywords internal
#' @export
#' @noRd
is_major.icd10cm <- function(x) {
  assert_character(x)
  grepl(jws(re_icd10cm_major), trimws(x), perl = TRUE)
}

#' @describeIn is_major check whether a code is an ICD-9 major
#' @keywords internal
#' @export
#' @noRd
is_major.icd9 <- function(x) {
  nchar(x) - icd9_is_e(x) < 4
}

#' Do ICD-9 codes belong to numeric, V or E sub-types?
#'
#' For each code, return \code{TRUE} if numeric or \code{FALSE} if a
#'   V or E code.
#' @param x vector of strings or factor to test
#' @return logical vector
#' @keywords internal
#' @noRd
icd9_is_n <- function(x)
  icd9_is_n_rcpp(as_char_no_warn(x))

#' @describeIn icd9_is_n are the given codes V type?
#' @keywords internal
#' @noRd
icd9_is_v <- function(x)
  icd9_is_v_rcpp(as_char_no_warn(x))

#' @describeIn icd9_is_n are the given codes E type?
#' @keywords internal
#' @noRd
icd9_is_e <- function(x)
  icd9_is_e_rcpp(as_char_no_warn(x))
