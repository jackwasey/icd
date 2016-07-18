# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

#' Trim leading and trailing white space from a single string
#'
#' \code{NA} is accepted (and returned as \code{NA_character_})
#' @param x character vector of length one
#' @return character vector of length one
#' @keywords internal character
strim <- function(x) {
  assert_string(x, na.ok = TRUE)
  if (!is.na(x[1]))
    .Call("icd_strimCpp", PACKAGE = "icd", as.character(x))
  else
    return(NA_character_)
}

#' Trim leading and trailing white space
#'
#' \code{NA} is accepted and returned, probably as \code{NA_character_}
#' @param x character vector
#' @return character vector
#' @keywords internal
#' @examples
#' \dontrun{
#' s <- random_string(250);
#' microbenchmark::microbenchmark(trim(s), trimws(s)) # trimws from R version 3.2
#' }
trim <- function(x) {
  nax <- is.na(x)
  x[!nax] <- .Call("icd_trimCpp", PACKAGE = "icd", as.character(x[!nax]))
  x
}

#' convert to character vector without warning
#' @param x vector, typically numeric or a factor
#' @return character vector
#' @keywords internal
as_char_no_warn <- function(x) {
  if (is.character(x))
    return(x)
  old <- options(warn = -1)
  on.exit(options(old))
  if (is.integer(x))
    fastIntToStringRcpp(x)
  if (is.factor(x))
    levels(x)[x]
  else
    as.character(x)
}

"%nin%" <- function(x, table) {
  match(x, table, nomatch = 0) == 0
}

#' Strip character(s) from character vector
#'
#' \code{gsub} is probably quicker than \code{stringr}/\code{stringi}. For
#' comorbidity processing, this package prefers the faster \link{base}
#' functions, whereas \code{stringr} is used for tasks which are not time
#' critical, e.g. parsing source data to be included in the distributed
#' \code{icd} package.
#' @param x character vector
#' @param pattern passed to \code{gsub} default is " "
#' @param use_bytes single logical passed to \code{base::gsub}, default is the
#'   slightly quicker \code{TRUE}
#' @return character vector of same length as input
#' @keywords internal
#' @examples
#' \dontrun{
#' x <- random_string(25000);
#' microbenchmark::microbenchmark(
#'   gsub(x = x, pattern = "A", replacement = "", fixed = TRUE, useBytes = TRUE),
#'   gsub(x = x, pattern = "A", replacement = "", fixed = TRUE, useBytes = TRUE, perl = TRUE),
#'   gsub(x = x, pattern = "A", replacement = ""),
#'   stringr::str_replace_all(x, "A", "")
#'   )
#' }
strip <- function(x, pattern = " ", use_bytes = TRUE)
  gsub(pattern = pattern, replacement = "", x = x,
       fixed = TRUE, useBytes = use_bytes)

#' Encode \code{TRUE} as 1, and \code{FALSE} as 0 (integers)
#'
#' When saving data as text files for distribution, printing large amounts of
#' text containing \code{TRUE} and \code{FALSE} is inefficient. Convert to
#' binary takes more R memory, but allows more compact output
#' @param x \code{data.frame} which may contain logical fields
#' @return \code{data.frame} without logical fields
#' @keywords internal manip logical
logical_to_binary <- function(x) {
  stopifnot(is.data.frame(x) || is.matrix(x))

  if (is.matrix(x)) {
    assert_matrix(x, min.rows = 1, min.cols = 1)
    mode(x) <- "integer"
    return(x)
  }

  assert_data_frame(x, min.rows = 1, min.cols = 1)

  logical_fields <- names(x)[sapply(x, is.logical)]
  if (is.na(logical_fields) || length(logical_fields) == 0)
    return(x)

  # update just the logical fields with integers
  x[, logical_fields] <-
    vapply(
      X         = x[, logical_fields],
      FUN       = function(y) ifelse(y, 1L, 0L),
      FUN.VALUE = integer(length = dim(x)[1])
    )
  x
}

#' Match pairs of strings to get named vector
#'
#' Match a character vector against a regular expression with at least two
#' parenthetic groupings, returning named vector.
#' @param string vector of strings
#' @param pattern vector of regular expression which should match exactly two
#'   strings for each element in \code{stringr}. If \code{pos} is specified,
#'   this rule is relaxed.
#' @param pos optional pair of integers with positions of the first and second
#'   desired matches, when multiple matches are made by the regular expression
#' @param swap logical scalar, whether to swap the names and values. Default is
#'   not to swap, so the first match becomes the name.
#' @keywords internal
str_pair_match <- function(string, pattern, pos, swap = FALSE, ...) {
  assert_character(string, min.len = 1)
  assert_string(pattern)
  assert_flag(swap)
  pos_missing <- missing(pos)
  if (pos_missing)
    pos <- c(1, 2)
  assert_integerish(pos, len = 2, lower = 1, any.missing = FALSE)

  string %>% str_match(pattern) -> res_matches

  if (pos_missing && ncol(res_matches) > 3)
    stop("the pair matching has three or more results but needed two.
          Use (?: to have a non-grouping regular expression parenthesis")

  # with str_match, the first column is a redundant complete match of the
  # whole pattern, so pos + 1 here:

  out_names <- res_matches[, ifelse(swap, 2, 1) + 1]
  if (any(is.na(out_names))) {
    stop("didn't match some rows:", string[is.na(out_names)],
         call. = FALSE)
  }

  out <- res_matches[, ifelse(swap, 1, 2) + 1]
  stopifnot(all(!is.na(out)))
  names(out) <- out_names
  out
}

#' Get or guess the name of the visit ID column
#'
#' The guess depends on the data, working through a list of likely candidates.
#' If the visit ID is known, it should be specified, ideally instead of calling
#' this function, but if unavoidable, using the \code{visit_name} parameter.
#' @param x input data, typically a data frame
#' @template visit_name
#' @keywords internal
get_visit_name <- function(x, visit_name = NULL) {
  UseMethod("get_visit_name")
}

.visit_name_guesses <- c("visit.?Id", "patcom", "encounter.?id", "enc.?id",
                         "in.*enc", "out.*enc", "encounter", "visit", "enc")

get_visit_name.data.frame <- function(x, visit_name = NULL) {
  assert_data_frame(x, min.cols = 1, col.names = "named")

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
  assert_string(visit_name)
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
  guesses <- c("icd.?(9|10)", "icd.?(9|10).?Code", "icd", "diagnos", "diag.?code", "diag", "i(9|10)", "code")
  assert_data_frame(x, min.cols = 1, col.names = "named")
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
  assert_string(icd_name)
  stopifnot(icd_name %in% names(x))
  icd_name
}

#' Latest ICD-9-CM edition
#'
#' Returns a single character value with the number of the latest edition,
#' currently \strong{32}.
#'
#' Implemented as a function to give flexibility to calculate this, or use an
#' option override.
#'
#' @keywords internal
icd9cm_latest_edition <- function() "32"

#' swap names and values of a vector
#'
#' Swap names and values of a vector. Non-character values are implicitly
#' converted to names.
#' @param x named vector
#' @return vector, with values being the names of the input vector, and names
#' being the previous values.
#' @keywords internal
swap_names_vals <- function(x) {
  assert_vector(x, strict = TRUE, any.missing = FALSE, names = "named")
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

#' Fast Factor Generation
#'
#' This function generates factors more quickly, by leveraging
#' \code{fastmatch}. The speed increase for ICD-9 codes is about
#' 33% reduction for 10 million codes.
#'
#' \code{NaN}s are converted to \code{NA} when used on numeric values. Extracted
#' from https://github.com/kevinushey/Kmisc.git
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
#' pts <- icd:::random_unordered_patients(1e7)
#' u <- unique.default(pts$code)
#' # this shows that stringr (which uses stringi) sort takes 50% longer than
#' # built-in R sort.
#' microbenchmark::microbenchmark(sort(u), str_sort(u))
#'
#' # this shows that \code{factor_} is about 50% faster than \code{factor} for
#' # big vectors of strings
#'
#' # without sorting is much faster:
#' microbenchmark::microbenchmark(factor(pts$code),
#'                                # factor_(pts$code),
#'                                factor_nosort(pts$code),
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
  suppressWarnings(f <- fmatch(x, levels))
  levels(f) <- as.character(labels)
  class(f) <- "factor"
  f
}

#' Fast find which \code{x} are \emph{not} in \code{table}
#'
#' Uses \code{\link[fastmatch]{fmatch}} which creates hash table, and re-uses if
#' \code{table} is re-used.
#' @param x vector
#' @param table vector
#' @keywords internal
`%fin%` <- function(x, table) {
  fmatch(x, table, nomatch = 0L) > 0L
}

#' wrapper for \code{.Deprecated}
#'
#' Don't show warnings when using deprecated code. This allows people to
#' continue using their \code{icd9} code without any modifications, and without
#' flooding the user with warnings.
#' @param ... arguments passed to \code{.Deprecated}
#' @examples
#' \dontrun{
#' # turn off all deprecated warnings for this package
#' options("icd.warn_deprecated" = FALSE)
#' }
#' @keywords internal
icd_deprecated <- function(...) {
  opt <- getOption("icd.warn_deprecated")
  if (!(is.null(opt) || !opt))
    .Deprecated(...)
}

#' Parse a (sub)chapter text description with parenthesised range
#'
#' @param x vector of descriptions followed by ICD code ranges
#' @return list of two-element character vectors, the elements being named
#'   'start' and 'end'.
#' @name chapter_to_desc_range
#' @keywords internal manip
.chapter_to_desc_range <- function(x, re_major) {
  assert_character(x, min.len = 1)
  assert_string(re_major)

  re_code_range <- paste0("(.*)[[:space:]]?\\((",
                          re_major, ")-(",
                          re_major, ")\\)"
  )
  re_code_single <- paste0("(.*)[[:space:]]?\\((", re_major, ")\\)")
  mr <- str_match_all(x, re_code_range)
  ms <- str_match_all(x, re_code_single)
  okr <- vapply(mr, length, integer(1)) == 4
  oks <- vapply(ms, length, integer(1)) == 3

  if (!all(okr || oks))
    stop("Problem matching\n", x[!(okr || oks)], call. = FALSE)
  m <- ifelse(okr, mr, ms)
  out <- lapply(m, function(y) c(start = y[[3]], end = y[[length(y)]]))
  names(out) <- vapply(m, function(y) y[[2]] %>% str_to_title %>% str_trim,
                       FUN.VALUE = character(1))
  out
}

#' @rdname chapter_to_desc_range
#' @keywords internal
chapter_to_desc_range.icd9 <- function(x) {
  .chapter_to_desc_range(x, re_major = re_icd9_major_bare)
}

#' @rdname chapter_to_desc_range
#' @keywords internal
chapter_to_desc_range.icd10 <- function(x) {
  .chapter_to_desc_range(x, re_major = re_icd10_major_bare)
}

na_to_false <- function(x) {
  assert_logical(x)
  x[is.na(x)] <- FALSE
  x
}

named_list <- function(...) {
  x <- list(...)
  names(x) <- as.character(match.call()[-1])
  x
}

fmatch <- function(x, table, nomatch = NA_integer_, incomparables = NULL) {
  .Call("fmatch", PACKAGE = "icd", x, table, nomatch, incomparables)
}

# allows R 3.1 to work
dir.exists <- function(paths) {
  x <- file.info(paths)$isdir
  !is.na(x) & x
}
