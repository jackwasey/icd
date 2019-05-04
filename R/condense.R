#' Condense ICD-9 code by replacing complete families with parent codes
#'
#' These functions take a set of ICD codes, and look for parent ICD codes in the
#' hierarchy. If all the children of a parent are found, then the returned
#' vector will just contain the parents of those codes. Any additional codes
#' which did not have all the siblings present are retained. This can be thought
#' of as the inverse operation to \code{children}.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template short_code
#' @template dotdotdot
#' @examples
#' kids <- children("V40")
#' kids
#' condense(kids, defined = TRUE)
#' kids_with_undefined <- children("V40", defined = FALSE)
#' head(kids_with_undefined)
#' length(kids_with_undefined)
#' condense(kids, defined = FALSE)
#'
#' # what happens if we have additional codes?
#' # (condense will warn if we don't explicitly ask to condense
#' # based on defined or possible codes.)
#' condense(c(kids, "41100"), warn = FALSE)
#'
#' # a good use is to summarise the contents of a comorbidity:
#' icd9_map_elix$CHF
#' condense(icd9_map_elix$CHF, warn = FALSE)
#' # explaining big groups of codes takes advantage of this:
#' explain_code(icd9_map_elix$CHF, condense = TRUE, warn = FALSE)
#' @family ICD-9 ranges
#' @keywords manip
#' @export
condense <- function(x, short_code = guess_short(x),
                     defined = NULL, warn = TRUE, ...) {
  UseMethod("condense")
}

#' @export
#' @keywords internal
condense.icd9 <- function(x, short_code = guess_short(x),
                          defined = NULL, warn = TRUE, ...) {
  assert_fac_or_char(x)
  assert_flag(short_code)
  stopifnot(is.null(defined) ||
    (is.logical(defined) && length(defined) == 1L))
  assert_flag(warn)
  if (short_code) {
    icd9_condense_short(x, defined = defined, warn = warn, ...)
  } else {
    icd9_condense_decimal(x, defined = defined, warn = warn, ...)
  }
}

# TODO: condense.icd10, especially thinking of the long lists of ICD-10 codes in
# PCCC maps.

#' @describeIn condense Condense a set of ICD codes, guessing ICD version
#'   from input data
#' @export
#' @keywords internal manip
condense.character <- function(x, short_code = guess_short(x),
                               defined = NULL, ...) {
  ver <- guess_version.character(x, short_code = short_code)
  if (ver %in% icd9_classes) {
    if (is.null(short_code)) short_code <- guess_short(x)
    condense.icd9(x, short_code = short_code, defined = defined, ...)
  } else if (ver %in% icd10_classes) {
    if (is.null(short_code)) short_code <- guess_short(x)
    stop("condense.icd10 not implemented yet")
  } else {
    stop("Unknown ICD type")
  }
}

#' @describeIn condense Condense a factor of ICD codes
#' @export
#' @keywords internal manip
condense.factor <- function(x,
                            short_code = guess_short(x),
                            defined = NULL,
                            ...) {
  condense.character(levels(x),
    short_code = short_code,
    defined = defined,
    ...
  )
}

#' @rdname condense
#' @keywords internal manip
icd9_condense_decimal <- function(x, defined = NULL, warn = TRUE,
                                  keep_factor_levels = FALSE) {
  short_to_decimal.icd9(
    icd9_condense_short(
      decimal_to_short.icd9(x),
      defined = defined, warn = warn, keep_factor_levels = keep_factor_levels
    )
  )
}

#' Condense short ICD-9 codes
#' @template warn
#' @param keep_factor_levels single logical value, default \code{FALSE}. If
#'   \code{TRUE}, will reuse the factor levels from the input data for the
#'   output data. This only applies if a factor is given for the input codes.
#' @keywords internal manip
#' @noRd
icd9_condense_short <- function(x,
                                defined = NULL,
                                warn = TRUE,
                                keep_factor_levels = FALSE) {
  stopifnot(is.null(defined) ||
    (is.logical(defined) && length(defined) == 1L))
  assert_flag(warn)
  assert_flag(keep_factor_levels)
  icd9Levels <- levels(x) # NULL if not a factor
  # we can convert back to factor later. Lots of scope for errors by handling
  # factors and character vectors in this function, so keep simple with
  # character only.
  x <- as_char_no_warn(x)
  i9w <- unique(get_valid.icd9(x, short_code = TRUE))
  if (is.null(defined)) {
    if (all(is_defined.icd9(i9w, short_code = TRUE))) {
      defined <- TRUE
      if (warn) {
        message("'defined' not given, but all codes are indeed defined")
      }
    } else {
      defined <- FALSE
      if (warn) {
        warning("'defined' not given, but not all codes are defined")
      }
    }
  }
  if (warn && defined && !all(is_defined.icd9(x, short_code = TRUE))) {
    x <- get_defined.icd9(x, short_code = TRUE)
    warning(
      "only defined values requested, but some undefined ICD-9 code(s)",
      " were given, so dropping them"
    )
  }
  # any major codes are automatically in output (not condensing higher than
  # three digit code) and all their children can be removed from the work list
  out <- majors <- i9w[are_major <- is_major.icd9(i9w)]
  i9w <- i9w[!are_major]
  i9w <- i9w[i9w %nin% children.icd9(majors,
    short_code = TRUE,
    defined = defined
  )]
  fout <- c()
  four_digit_parents <- unique(substr(i9w, 0, 4))
  for (fp in four_digit_parents) {
    # onlyBillable at 5th level is same as onlyReal
    test_kids <- children.icd9(fp,
      defined = defined,
      short_code = TRUE, billable = FALSE
    )
    if (length(test_kids) > 0 && all(test_kids %in% c(fp, i9w))) {
      # if ((length(test_kids) > 1) || (fp %in% i9w)) {
      fout <- c(fout, fp)
      i9w <- i9w[-which(i9w %in% test_kids)]
      # }
    }
  }
  # 'out' now has original major codes, 'fout' has parent four digit codes. Now
  # see whether any groups of four digit codes comprise all the four digit codes
  # in any parent (or original) major. We test the original majors again to
  # catch where non-billable four digit codes were omitted, but otherwise all
  # the children are complete. This replaces the old 'onlyBillable' flag which
  # was annoying.

  # set new variable so we don't change the thing we are looping over...
  major_parents <- unique(get_major.icd9(c(out, fout, i9w), short_code = TRUE))
  for (mp in major_parents) {
    test_kids <- children.icd9(mp, short_code = TRUE, defined = defined)
    # we've done these already:
    test_kids <- test_kids[nchar(test_kids) < (5L + icd9_is_e(mp))]
    test_kids <- test_kids[-which(test_kids == mp)]
    if (length(test_kids) > 0 && all(test_kids %in% c(out, fout, i9w))) {
      out <- c(out, mp)
      fout <- fout[-which(fout %in% test_kids)]
    }
  }
  out <- unique(sort.icd9(c(out, fout, i9w), short_code = TRUE))
  if (!is.null(icd9Levels)) {
    if (keep_factor_levels) {
      out <- factor(out, icd9Levels)
    } else {
      out <- factor(out)
    }
  }
  if (defined) {
    get_defined.icd9(out, short_code = TRUE)
  } else {
    out
  }
}
