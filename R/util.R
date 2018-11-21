# Copyright (C) 2014 - 2018  Jack O. Wasey
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

# save this in package environment so it doesn't need to be done on the fly
.have_regexec_perl <- "perl" %in% names(as.list(regexec))

#' Trim leading and trailing white space from a single string
#'
#' \code{NA} is accepted (and returned as \code{NA_character_})
#' @param x character vector of length one
#' @return character vector of length one
#' @keywords internal character
strim <- function(x) {
  assert_string(x, na.ok = TRUE)
  if (!is.na(x[1]))
    .Call("_icd_strimCpp", PACKAGE = "icd", as.character(x))
  else
    return(NA_character_)
}

#' Trim leading and trailing white space
#'
#' \code{NA} is accepted and returned, probably as \code{NA_character_}
#' @param x character vector
#' @return character vector
#' @keywords internal manip
trim <- function(x) {
  nax <- is.na(x)
  x[!nax] <- .Call("_icd_trimCpp", PACKAGE = "icd", as.character(x[!nax]))
  x
}

"%nin%" <- function(x, table)
  match(x, table, nomatch = 0) == 0

#' @describeIn match_rcpp Use faster matching for %in%
#' @keywords internal
"%fin%" <- function(x, table)
  fin(x, table)

#' @describeIn match_rcpp Use faster matching for %nin%
#' @keywords internal
"%fnin%" <- function(x, table)
  !fin(x, table)

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
strip <- function(x, pattern = " ", use_bytes = TRUE)
  gsub(pattern = pattern, replacement = "", x = x,
       fixed = TRUE, useBytes = use_bytes)

#' Encode \code{TRUE} as 1, and \code{FALSE} as 0 (integers)
#'
#' When saving data as text files for distribution, printing large amounts of
#' text containing \code{TRUE} and \code{FALSE} is inefficient. Convert to
#' binary takes more R memory, but allows more compact output
#' @param x \code{data.frame} which may contain logical fields
#' @examples
#' mat <- matrix(sample(c(TRUE, FALSE), size = 9, replace = TRUE), nrow = 3)
#' mat
#' icd:::logical_to_binary(mat)
#' icd:::binary_to_logical(icd:::logical_to_binary(mat))
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
  logical_fields <- names(x)[vapply(x, is.logical, logical(1))]
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

#' @describeIn logical_to_binary Convert integer columns to logical values
binary_to_logical <- function(x) {
  stopifnot(is.data.frame(x) || is.matrix(x))
  if (is.matrix(x)) {
    assert_matrix(x, min.rows = 1, min.cols = 1)
    mode(x) <- "logical"
    return(x)
  }
  assert_data_frame(x, min.rows = 1, min.cols = 1)
  integer_fields <- names(x)[vapply(x, is.integer, logical(1))]
  if (is.na(integer_fields) || length(integer_fields) == 0)
    return(x)

  # update just the logical fields with integers
  x[, integer_fields] <-
    vapply(
      X         = x[, integer_fields],
      FUN       = function(y) ifelse(y, TRUE, FALSE),
      FUN.VALUE = logical(length = dim(x)[1])
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
  assert_character(string, min.len = 1L)
  assert_string(pattern, min.chars = 5L)
  assert_flag(swap)
  pos_missing <- missing(pos)
  if (pos_missing)
    pos <- c(1L, 2L)
  else
    assert_integerish(pos, len = 2L, lower = 1L, any.missing = FALSE)
  res <- lapply(string,
                function(x) unlist(
                  regmatches(x, m = regexec(pattern = pattern, text = x, ...))
                )[-1]
  )
  res <- res[vapply(res, function(x) length(x) != 0, logical(1))]
  res <- do.call(rbind, res)
  if (pos_missing && ncol(res) > max(pos))
    stop("the pair matching has three or more ress but needed two.
          Use (?: to have a non-grouping regular expression parenthesis")
  out_names <- res[, ifelse(swap, 2L, 1L)]
  if (any(is.na(out_names)))
    stop("didn't match some rows:", string[is.na(out_names)],
         call. = FALSE)
  out <- res[, ifelse(swap, 1L, 2L)]
  stopifnot(all(!is.na(out)))
  setNames(out, out_names)
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

#' @keywords internal
#' @export
get_visit_name.data.frame <- function(x, visit_name = NULL) {
  assert_data_frame(x, min.cols = 1, col.names = "named")
  visit_name_guesses <- c("visit.?Id", "patcom", "encounter.?id", "enc.?id",
                          "in.*enc", "out.*enc", "encounter", "visit", "enc")
  if (is.null(visit_name)) {
    for (guess in visit_name_guesses) {
      guess_matched <- grep(guess, names(x), ignore.case = TRUE, value = TRUE)
      if (length(guess_matched) == 1) {
        visit_name <- guess_matched
        break
      }
    }
    if (is.null(visit_name))
      visit_name <- names(x)[1]
  }
  assert_string(visit_name)
  stopifnot(visit_name %in% names(x))
  visit_name
}

#' @keywords internal
#' @export
get_visit_name.matrix <- function(x, visit_name = NULL)
  stop("matrices of comorbidity data are expected to be of logical type, ",
       "and have row names corresponding to the visit or patient.")

#' get the name of a \code{data.frame} column which is most likely to contain
#' the ICD codes
#'
#' guess which field contains the (only) ICD code, in order of preference, the
#' column name has an icd code class, case-insensitive regular expressions of
#' commonly used names for ICD code fields, a single column has more than 10%
#' valid ICD codes. If the result is not specified by class, or exactly with
#' \code{icd_name} being given, we confirm there are at least some valid ICD
#' codes in there
#' @param x data frame
#' @param icd_name usually \code{NULL} but if specified, will be checked it is
#'   valid (i.e. a character vector of length one, which is indeed a name of one
#'   of \code{x}'s columns) and returned unchanged
#' @param multi If \code{TRUE}, allow multiple ICD field names to be returned.
#' @keywords internal
get_icd_name <- function(x, icd_name = NULL, valid_codes = TRUE,
                         defined_codes = FALSE, multi = FALSE) {
  # TODO: change this error message, no longer true!
  if (is.icd_wide_data(x))
    stop("Unable to infer the name of a single ICD field name from wide data, which has multiple ICD fields. ",
         "Comorbidity calculations require 'long' format data, 'wide' data should be converted to 'long' ",
         "using 'wide_to_long'. ",
         "If the data is indeed 'long' format, remove the class 'icd_wide_data' and ",
         "use 'as.icd_long_data' to set the correct class. See '?icd_long_data' for help.")
  if (!is.null(icd_name)) {
    stopifnot(icd_name %in% names(x))
    return(icd_name)
  }
  icd_name <- guess_icd_col_by_name(x, valid_codes = valid_codes,
                        defined_codes = defined_codes)
  if (is.null(icd_name)) {
    icd_name = character()
    for (n in names(x)) {
      pc <- get_icd_defined_percent(x[[n]])
      if (pc$icd9 > 25 || pc$icd10 > 25) {
        icd_name <- c(icd_name, n)
      }
    }
  }
  if (nrow(x) < 2 || (!valid_codes && !defined_codes))
    return(icd_name)
  pc <- if (defined_codes)
    get_icd_defined_percent(x[[icd_name]]) # TODO vectorize this function
  else
    get_icd_valid_percent(x[[icd_name]])
  if (pc$icd9 < 10 && pc$icd10 < 10)
    stop("identified field with ICD codes as: '", icd_name,
         "' but fewer than 10% of codes are valid ICD-9 or ICD-10. ",
         "If this really is a valid column, identify the field containing ",
         "ICD codes in the input data using 'icd_name=\"my_icd_field\"' or ",
         "set the class using something like",
         " x[[icd_name]] <- as.icd9[[x[[icd_name]]")
  icd_name
}

#' get candidate column(s) from wide or long data frame frame, using hints
#' @examples
#' wide_df <- data.frame(a = letters,
#'                       dx0 = icd9_map_elix$CHF[1:26],
#'                       dx1 = icd9_map_elix$PVD[1:26],
#'                       dx2 = icd9_map_elix$HTN[1:26])
#' icd:::guess_icd_col_by_name(wide_df)
#' wide_dc <- data.frame(a = letters,
#'                       dx0 = as.icd9cm(icd9_map_elix$CHF[1:26]),
#'                       dx1 = as.icd9cm(icd9_map_elix$PVD[1:26]),
#'                       dx2 = as.icd9cm(icd9_map_elix$HTN[1:26]),
#'                       stringsAsFactors = FALSE)
#' icd:::guess_icd_col_by_name(wide_dc)
#' @return Zero, one or many names of columns likely to contain ICD codes based
#'   on the column names.
#' @keywords internal
guess_icd_col_by_name <- function(x, valid_codes = TRUE,
                                  defined_codes = FALSE) {
  guesses <- c("icd.?(9|10)", "icd.?(9|10).?Code", "icd",
               "diagnos", "diag.?code", "diag", "dx", "i(9|10)", "code")
  assert_data_frame(x, min.cols = 1, col.names = "named")
  # if one column exactly has a class like icd9, then we're done.
  cls <- lapply(x, class)
  clg <- vapply(cls, function(z) any(z %in% icd_version_classes), logical(1))
  if (any(clg)) return(names(x)[clg])
  guessed <- lapply(guesses, grep, x = names(x), ignore.case = TRUE, value = TRUE)
  guess_counts <- vapply(guessed, length, integer(1))
  guesses_logical <- as.logical(guess_counts)
  if (sum(guesses_logical) == 1) {
    return(unlist(guessed[guesses_logical]))
  }
  best_guess <- which(guess_counts == max(guess_counts))
  if (length(best_guess) == 1)
    return(guessed[[best_guess]])
  return(NULL)
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

#' mimic the \code{R CMD check} test
#'
#' \code{R CMD check} is quick to tell you where \code{UTF-8} characters are not
#' encoded, but gives no way of finding out which or where
#' @examples
#' \dontrun{
#' sapply(icd9cm_hierarchy, icd:::get_non_ASCII)
#' icd:::get_encodings(icd9cm_hierarchy)
#' sapply(icd9cm_billable, icd:::get_non_ASCII)
#' sapply(icd9cm_billable, icd:::get_encodings)
#' }
#' @keywords internal
get_non_ASCII <- function(x)
  x[is_non_ASCII(as_char_no_warn(x))]

#' @rdname get_non_ASCII
#' @keywords internal
is_non_ASCII <- function(x)
  is.na(iconv(as_char_no_warn(x), from = "latin1", to = "ASCII"))

#' @rdname get_non_ASCII
#' @keywords internal
get_encodings <- function(x) {
  stopifnot(is.list(x) || is.data.frame(x))
  sapply(x, function(y) unique(Encoding(as_char_no_warn(y))))
}

#' Parse a (sub)chapter text description with parenthesised range
#'
#' @param x vector of descriptions followed by ICD code ranges
#' @return list of two-element character vectors, the elements being named
#'   'start' and 'end'.
#' @name chapter_to_desc_range
#' @keywords internal manip
.chapter_to_desc_range <- function(x, re_major) {
  assert_character(x, min.len = 1L)
  assert_string(re_major)

  re_code_range <- paste0("(.*)[[:space:]]?\\((",
                          re_major, ")-(",
                          re_major, ")\\)"
  )
  re_code_single <- paste0("(.*)[[:space:]]?\\((", re_major, ")\\)")
  mr <- str_match_all(x, re_code_range)
  ms <- str_match_all(x, re_code_single)
  okr <- vapply(mr, length, integer(1)) == 4L
  oks <- vapply(ms, length, integer(1)) == 3L

  if (!all(okr || oks))
    stop("Problem matching\n", x[!(okr || oks)], call. = FALSE)
  m <- ifelse(okr, mr, ms)
  out <- lapply(m, function(y) c(start = y[[3]], end = y[[length(y)]]))
  names(out) <- vapply(m, function(y) trim(to_title_case(y[[2]])),
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

#' make a list using input argument names as names
#' @param ... arguments whose names become list item names, and values become
#'   the values in the list
#' @keywords internal
named_list <- function(...) {
  x <- list(...)
  names(x) <- as.character(match.call()[-1])
  x
}

# allows R 3.1 to work. TODO: obsolete
dir.exists <- function(paths) {
  x <- file.info(paths)$isdir
  !is.na(x) & x
}

#' return all matches for regular expression
#' @keywords internal manip
str_match_all <- function(string, pattern, ...) {
  string <- as.character(string)
  regmatches(x = string, m = regexec(pattern = pattern, text = string, ...))
}

#' \code{stringr} does this, but here we have a small amount of base R code
#' @keywords internal
str_extract <- function(string, pattern, ...) {
  vapply(regmatches(string, m = regexec(pattern = pattern, text = string, ...)),
         FUN = `[[`, 1, FUN.VALUE = character(1L))
}

capitalize_first <- function(x) {
  trim(paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))))
}

to_title_case <- function(x) {
  for (split_char in c(" ", "-", "[")) {
    s <- strsplit(x, split_char, fixed = TRUE)[[1]]
    x <- paste(toupper(substring(s, 1L, 1L)), substring(s, 2L),
               sep = "", collapse = split_char)
  }
  x
}
