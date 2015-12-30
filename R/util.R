# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

#' Trim leading and trailing whitespace from a single string
#'
#' \code{NA} is accepted (and returned as \code{NA_character_})
#' @param x character vector of length one
#' @return character vector of length one
#' @keywords internal
strim <- function(x) {
  assertString(x, na.ok = TRUE)
  if (!is.na(x[1]))
    .Call("icd9_strimCpp", PACKAGE = get_pkg_name(), as.character(x))
  else
    return(NA_character_)
}

#' Trim leading and trailing whitespace
#'
#' \code{NA} is accepted and returned, probably as \code{NA_character_}
#' @details TODO: replace with str_trim but test speed first.
#' @param x character vector
#' @return character vector
#' @keywords internal
trim <- function(x) {
  nax <- is.na(x)
  x[!nax] <- .Call("icd9_trimCpp", PACKAGE = get_pkg_name(), as.character(x[!nax]))
  x
}

asCharacterNoWarn <- function(x) {
  if (is.character(x)) return(x)
  old <- options(warn = -1)
  on.exit(options(old))
  if (is.integer(x)) fastIntToStringRcpp(x)
  if (is.factor(x)) return(levels(x)[x])
  as.character(x)
}

"%nin%" <- function(x, table)
  match(x, table, nomatch = 0) == 0

#' Strip character(s) from character vector
#'
#' TODO: replace with str_replace
#' @param x character vector
#' @param pattern passed to \code{gsub} default is " "
#' @param useBytes passed to gsub, default is the slightly quicker \code{TRUE}
#' @return character vector of same length as input
#' @keywords internal
strip <- function(x, pattern = " ", useBytes = TRUE)
  gsub(pattern = pattern, replacement = "", x = x,
       fixed = TRUE, useBytes = useBytes)

#' @title encode TRUE as 1, and FALSE as 0 (integers)
#' @description when saving data as text files for distribution, printing large
#'   amounts of text containing TRUE and FALSE is inefficient. Convert to binary
#'   takes more R memory, but allows more compact output
#' @param x dataframe which may contain logical fields
#' @return data frame without logical fields
#' @keywords internal manip
logical_to_binary <- function(x) {
  stopifnot(is.data.frame(x) || is.matrix(x))

  if (is.matrix(x)) {
    assertMatrix(x, min.rows = 1, min.cols = 1)
    return(as.logical(x))
  } else if (is.data.frame(x))
    assertDataFrame(x, min.rows = 1, min.cols = 1)
  else
    stop("need a matrix or data.frame")

  logical_fields <- names(x)[sapply(x, is.logical)]
  if (is.na(logical_fields) || length(logical_fields) == 0) return(x)

  #update just the logical fields with integers
  x[, logical_fields] <-
    vapply(
      X         = x[, logical_fields],
      FUN       = function(y) ifelse(y, 1L, 0L),
      FUN.VALUE = integer(length = dim(x)[1])
    )
  x
}

#' Match strings to get named vector
#'
#' match a character vector against a regex with a pair of parentheses,
#' returning named vector
#' @param swap logical scalar, whether to swap the names and values. Default is
#'   not to swap, so the first match becomes the name.
#' @param warn_pattern logical, if the pattern has multiple parentheses,
#'   optinally don't warn if we are forced to choose just a pair of the sub-matches
#' @keywords internal
str_pair_match <- function(string, pattern, swap = FALSE, dropEmpty = FALSE, pos = c(1, 2), warn_pattern = TRUE, ...) {
  assertCharacter(string, min.len = 1)
  assertString(pattern)
  assertFlag(swap)
  assertIntegerish(pos, len = 2, lower = 1, any.missing = FALSE)

  string %>% str_match_all(pattern) -> res_matches

  if (warn_pattern && identical(pos, c(1, 2)) && length(res_matches[[1]]) > 3) {
    warning("the pair matching has multiple options, so choosing the first
            (incomplete) match. Either turn off this warning, or set 'pos' to
            specify positions to detect. If positions 1 and 2 are needed, do not
            specify explicity, or set 2, 1 and swap. The last, which is in
            position ", length(res_matches[[1]]))
    pos <- length(res_matches[[1]])
  }
  # with str_match_all, the first match is a redundant complete match of the
  # whole pattern, so pos + 1 here:
  lapply(res_matches, FUN = `[`, pos + 1) -> res

  outNames <- vapply(X = res,
                     FUN = "[",
                     FUN.VALUE = character(1),
                     ifelse(swap, 2, 1))
  stopifnot(all(!is.na(outNames)))

  out <- vapply(X = res,
                FUN = "[",
                FUN.VALUE = character(1),
                ifelse(swap, 1, 2))
  stopifnot(all(!is.na(out)))

  names(out) <- outNames
  out
}

get_visit_name <- function(x, visit_name = NULL) {
  UseMethod("get_visit_name")
}

.visit_name_guesses <- c("visit.?Id", "patcom", "encounter.?id", "enc.?id",
                         "in.*enc", "out.*enc", "encounter", "visit", "enc")

get_visit_name.data.frame <- function(x, visit_name = NULL) {
  assertDataFrame(x, min.cols = 1, col.names = "named")

  if (is.null(visit_name)) {
    for (guess in .visit_name_guesses) {
      guess_matched <- grep(guess, names(x), ignore.case = TRUE, value = TRUE)
      if (length(guess_matched) == 1) {
        visit_name <- guess_matched
        break
      }
    }
    # if still null, then guess the name of the first column
    if (is.null(visit_name))
      visit_name <- names(x)[1]
  }
  assertString(visit_name)
  stopifnot(visit_name %in% names(x))
  visit_name
}

get_visit_name.matrix <- function(x, visit_name = NULL) {
  stop("matrices of comorbidity data are expected to be of logical type, and have row names corresponding to the visit or patient.")
}

# guess which field contains the (only) ICD code, in order of preference
# case-insensitive regex. If there are zero or multiple matches, we move on down
# the list, meaning some later possibilities are more or less specific regexes
# than earlier ones.
get_icd_name <- function(x, icd_name = NULL) {
  guesses <- c("icd.?(9|10)", "icd.?(9|10).?Code", "icd", "diagnos", "diag.?code", "diag", "i(9|10)")
  assertDataFrame(x, min.cols = 1, col.names = "named")
  if (is.null(icd_name)) {
    for (guess in guesses) {
      guess_matched <- grep(guess, names(x), ignore.case = TRUE, value = TRUE)
      if (length(guess_matched) == 1) {
        icd_name <- guess_matched
        break
      }
    }
    if (is.null(icd_name))
      # still NULL so fallback to second column
      icd_name <- names(x)[2]
    # Could look at contents of the data frame, although this evaluates a
    # promise on potentially a big data frame, so could be costly
  }
  assertString(icd_name)
  stopifnot(icd_name %in% names(x))
  icd_name
}

#' Latest ICD-9-CM edition
#'
#' Returns a single character value with the number of the latests edition, currently \strong{32}.
#'
#' Implemented as a function to give flexibility to calculate this, or use an option override.
#' @keywords internal
icd9cm_latest_edition <- function() "32"

#' @title swap names and values of a vector
#' @description swap names and values of a vector
#' @param x named vector
#' @return vector
#' @keywords internal
swapNamesWithVals <- function(x) {
  assertVector(x, strict = TRUE, any.missing = FALSE, names = "named")
  new_names <- unname(x)
  x <- names(x)
  names(x) <- new_names
  x
}

# mimic the R CMD check test
getNonASCII <- function(x)
  x[isNonASCII(x)]

isNonASCII <- function(x)
  is.na(iconv(x, from = "latin1", to = "ASCII"))

utils::globalVariables(c("do_slow_tests", "do_online_tests"))

skip_slow_tests <- function(msg = "skipping slow test") {
  if (!exists("do_slow_tests") || !do_slow_tests)
    testthat::skip(msg)
}

skip_online_tests <- function(msg = "skipping online test") {
  if (!exists("do_online_tests") || !do_online_tests)
    testthat::skip(msg)
}

skip_flat_icd9_avail <- function(ver = "31",
                                 msg = paste("skipping test because flat file
                                             ICD-9-CM sources not available for
                                             version: ", ver)) {
  dat <- data_sources[data_sources$version == ver, ]
  fn_short_orig <- dat$short_filename

  f_info_short <- unzip_to_data_raw(dat$url,
                                    file_name = fn_short_orig,
                                    offline = TRUE)
  if (is.null(f_info_short))
    skip_online_tests(msg)

}
#' Fast Factor Generation
#'
#' This function generates factors more quickly, by leveraging
#' \code{fastmatch::\link{fmatch}}. The speed increase for ICD-9 codes is about
#' 33% reduction for 10 million codes.
#'
#' \code{NaN}s are converted to \code{NA} when used on numerics. Extracted from
#' https://github.com/kevinushey/Kmisc.git
#'
#' These feature from base R are missing: \code{exclude = NA, ordered =
#' is.ordered(x), nmax = NA}
#' @author Kevin Ushey, adapted by Jack Wasey
#' @param x An object of atomic type \code{integer}, \code{numeric},
#'   \code{character} or \code{logical}.
#' @param levels An optional character vector of levels. Is coerced to the same
#'   type as \code{x}. By default, we compute the levels as
#'   \code{sort(unique.default(x))}.
#' @param labels A set of labels used to rename the levels, if desired.
#' @param na.last If \code{TRUE} and there are missing values, the last level is
#'   set as \code{NA}; otherwise; they are removed.
#' @examples
#' \dontrun{
#' pts <- icd9:::randomUnorderedPatients(1e7)
#' u <- unique.default(pts$icd9)
#' # this shows that stringr (which uses stringi) sort takes 50% longer than
#' # built-in R sort.
#' microbenchmark::microbenchmark(sort(u), stringr::str_sort(u))
#'
#' # this shows that \code{factor_} is about 50% faster than \code{factor} for
#' # big vectors of strings
#'
#' # without sorting is much faster:
#' microbenchmark::microbenchmark(factor(pts$icd9),
#'                                # factor_(pts$icd9),
#'                                factor_nosort(pts$icd9),
#'                                times = 25)
#' }
#' @details I don't think there is any requirement for factor levels to be
#'   sorted in advance, especially not for ICD-9 codes where a simple
#'   alphanumeric sorting will likely be completely wrong.
#' @keywords internal manip
factor_nosort <- function(x, levels = NULL, labels = levels) {
  # sort may be pre-requisite for fastmatch
  if (is.factor(x)) return(x)
  if (is.null(levels)) levels <- unique.default(x)
  suppressWarnings(f <- fastmatch::fmatch(x, levels))
  levels(f) <- as.character(labels)
  class(f) <- "factor"
  f
}

#' @title sort short-form icd9 codes
#' @description Sorts lists of numeric only, V or E codes. Note that a simple
#'   numeric sort does not work for ICD-9 codes, since "162" > "1620", and also
#'   V codes precede E codes.
#' @details Implementation used fast built-in sort, then shuffles the E codes to
#'   the end.
#' @param x vector of ICD codes to sort
#' @template short_code
#' @template dotdotdot
#' @return sorted vector of ICD-9 codes. Numeric, then E codes, then V codes.
#' @keywords manip
#' @export
icd_sort <- function(x, ...)
  UseMethod("icd_sort")

#' @describeIn icd_sort Sort ICD-10 codes, note that setting \code{short} is
#'   unnecessary and ignored.
#' @export
icd_sort.icd10 <- function(x, short_code = NULL, ...) {
  # ignore short, it doesn't matter
  sort(x)
}

#' @describeIn icd_sort sort ICD-9 codes respecting numeric, then V, then E
#'   codes, and accounting for leading zeroes
#' @export
icd_sort.icd9 <- function(x, short_code = icd_guess_short(x), ...) {
  assert(checkFactor(x), checkCharacter(x))
  assertFlag(short_code)

  if (short_code)
    x[icd9_order_short(x)]
  else
    x[icd9_order_short(icd_decimal_to_short.icd9(x))]
}

icd9_order_short <- function(x) {
  y <- x[order(icd9_add_leading_zeroes.icd_short_code(x))]
  match(
    y[c(which(icd9_is_n(y)), which(icd9_is_v(y)), which(icd9_is_e(y)))],
    x)
}

#' wrapper for \code{.Deprecated}
#'
#' Don't show warnings when testing deprecated code. I don't really want
#' deprecated warnings in any testing, with testthat directly, or in CRAN
#' checks.
#' @param ... arguments passed to \code{.Deprecated}
#' @keywords internal
icd_deprecated <- function(...) {
  # if NOT_CRAN is set, it means we are running testthat!
  #if (identical(Sys.getenv("NOT_CRAN"), "true"))
  #  return()

  test_mode <- unlist(sys.calls()) %>% str_detect("test") %>% any

  if (!test_mode)
    .Deprecated(...)

}
