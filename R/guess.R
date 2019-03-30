#' Guess version of ICD codes
#'
#' The guess is indeed a guess and can be wrong. There are some codes which
#' could be either ICD-9 or ICD-10. The current implementation doesn't check
#' whether the codes exist in any definitions (ICD-9 CM or WHO, for example),
#' just whether they are valid.
#'
#' Currently, ambiguous codes are guessed true or false, with no indication of
#' uncertainty. Possible solutions are adding an attribute, warning, or
#' optionally throwing an error.
#'
#' @param x input data
#' @template short_code
#' @param n number of elements or rows to sample
#' @export
#' @keywords internal
guess_version <- function(x, short_code, ...)
  UseMethod("guess_version")

#' @keywords internal
#' @export
guess_version.icd9 <- function(x, short_code, ...) "icd9" # nocov

#' @keywords internal
#' @export
guess_version.icd9cm <- function(x, short_code, ...) "icd9cm" # nocov

#' @keywords internal
#' @export
guess_version.icd10 <- function(x, short_code, ...) "icd10" # nocov

#' @keywords internal
#' @export
guess_version.icd10cm <- function(x, short_code, ...) "icd10cm" # nocov

#' @keywords internal
#' @export
guess_version.icd10who <- function(x, short_code, ...) "icd10who" # nocov

#' @describeIn guess_version Guess version of ICD codes in a factor
#' @export
#' @keywords internal
#' @noRd
guess_version.factor <- function(x, short_code = NULL, ...) {
  guess_version.character(as_char_no_warn(x), short_code = short_code, ...)
}

get_icd_valid_percent <- function(x, short_code = NULL, n = 100) {
  if (is.list(x)) {
    stopifnot(length(x) == 1)
    x <- x[[1]]
  }
  y <- as_char_no_warn(x)[1:min(n, length(x))]
  if (!is.null(short_code)) {
    if (short_code) {
      i9 <- sum(is_valid.icd9(y, short_code = TRUE), na.rm = TRUE)
      i10 <- sum(is_valid.icd10(y, short_code = TRUE), na.rm = TRUE)
    } else {
      i9 <- sum(is_valid.icd9(y, short_code = FALSE), na.rm = TRUE)
      i10 <- sum(is_valid.icd10(y, short_code = FALSE), na.rm = TRUE)
    }
  } else {
    i9 <- max(
      sum(is_valid.icd9(y, short_code = TRUE), na.rm = TRUE),
      sum(is_valid.icd9(y, short_code = FALSE), na.rm = TRUE)
    )
    i10 <- max(
      sum(is_valid.icd10(y, short_code = TRUE), na.rm = TRUE),
      sum(is_valid.icd10(y, short_code = FALSE), na.rm = TRUE)
    )
  }
  nfac <- 100 / min(100, length(x))
  list(icd9 = i9 * nfac, icd10 = i10 * nfac)
}

get_icd_defined_percent <- function(x, short_code = NULL, n = 100) {
  y <- as_char_no_warn(x)[1:min(n, length(x))]
  if (!is.null(short_code)) {
    if (short_code) {
      i9 <- sum(is_defined.icd9(y, short_code = TRUE), na.rm = TRUE)
      i10 <- sum(is_defined.icd10(y, short_code = TRUE), na.rm = TRUE)
    } else {
      i9 <- sum(is_defined.icd9(y, short_code = FALSE), na.rm = TRUE)
      i10 <- sum(is_defined.icd10(y, short_code = FALSE), na.rm = TRUE)
    }
  } else {
    i9 <- max(
      sum(is_defined.icd9(y, short_code = TRUE), na.rm = TRUE),
      sum(is_defined.icd9(y, short_code = FALSE), na.rm = TRUE)
    )
    i10 <- max(
      sum(is_defined.icd10(y, short_code = TRUE), na.rm = TRUE),
      sum(is_defined.icd10(y, short_code = FALSE), na.rm = TRUE)
    )
  }
  nfac <- 100 / min(100, length(x))
  list(icd9 = i9 * nfac, icd10 = i10 * nfac)
}

#' @describeIn guess_version Guess version of ICD codes in character vector
#' @export
#' @keywords internal
#' @noRd
guess_version.character <- function(x, short_code = NULL, ...) {
  assert_character(x)
  stopifnot(is.null(short_code) ||
    (is.logical(short_code) && length(short_code) == 1L))
  dots <- list(...)
  n <- dots[["n"]]
  if (is.null(n)) {
    n <- 100
  }
  pc <- get_icd_valid_percent(x, short_code, n)
  if (pc$icd9 >= pc$icd10) {
    "icd9"
  } else {
    "icd10"
  }
}

#' @describeIn guess_version Guess version of ICD codes in a field in a
#'   \code{data.frame}
#' @template icd_name
#' @keywords internal
#' @export
#' @noRd
guess_version.data.frame <- function(x, short_code = NULL,
                                     icd_name = NULL, ...) {
  if (is.null(icd_name)) {
    icd_name <- get_icd_name(x)
  }
  guess_version(x[[icd_name[1]]])
}

#' Guess version of ICD and update class
#'
#' Guesses the version of given ICD codes and sets the class of the
#'   returned data according to the guess.
#' @param x vector of ICD codes of some type
#' @template short_code
#' @return the input data with appropriate ICD class set
#' @keywords internal
#' @noRd
guess_version_update <- function(x, short_code = guess_short(x)) {
  # could either return a method from the guess version function (nice and
  # functional), use the returned string as a function name to invoke, or switch
  # on the string. Just adding the class is bad, e.g. would miss icd10cm if
  # added
  ver <- guess_version(x, short_code)
  if (ver == "icd9") {
    icd9(x)
  } else if (ver == "icd10") {
    icd10(x)
  } # use the fast version, not as.icd10
  else {
    stop("unknown type returned when guessing ICD version")
  }
}

#' Guess the ICD version (9 or 10) from a pair of codes
#'
#' Designed for guessing version of codes given to delineate a range
#' @param start ICD code
#' @param end ICD code
#' @template short_code
#' @return Type of codes as single character \code{"icd9"} or \code{"icd10"}, or
#'   error if conflicting results
#' @keywords internal
#' @noRd
guess_pair_version <- function(start, end, short_code = NULL) {
  start_guess <- guess_version.character(
    as_char_no_warn(start),
    short_code = short_code
  )
  end_guess <- guess_version.character(
    as_char_no_warn(end),
    short_code = short_code
  )
  if (start_guess != end_guess) {
    stop(
      "Cannot expand range because ICD code version cannot be guessed from ",
      start, " and ", end,
      ". Either specify the classes, e.g. icd9(\"100.4\"), or call the
       S3 method directly, e.g. expand_range.icd9"
    )
  }
  start_guess
}

#' guess icd-9 or icd-10 or other type, and switch to call the given function
#' @param x data frame of patient data
#' @param funs named list of functions to call, with the names being at a
#'   minimum 'icd9' and 'icd10', and 'icd9cm' or 'icd10cm' being used first if
#'   provided.
#' @param ... further arguments, perhaps including \code{icd_name}
#' @noRd
#' @keywords internal
switch_ver_cmb <- function(x, funs, ...) {
  dots <- list(...)
  icd_name <- if ("icd_name" %in% names(dots)) dots[["icd_name"]]
  n <- names(funs)
  stopifnot(all(c("icd9", "icd10") %in% n))
  ver <- guess_version.data.frame(x, icd_name = icd_name)
  if ("icd9cm" %in% n) {
    return(funs[["icd9cm"]](x, ...))
  }
  if ("icd10cm" %in% n) {
    return(funs[["icd10cm"]](x, ...))
  }
  if (ver %in% icd9_classes) {
    return(funs[["icd9"]](x, ...))
  }
  if (ver %in% icd10_classes) {
    return(funs[["icd10"]](x, ...))
  }
  stop("could not guess the ICD version using icd_name = ", icd_name)
}
