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

#' Convert ICD9 codes between formats and structures.
#'
#' ICD-9 codes are represented in \emph{short} and \emph{decimal} forms. The
#' short form has up to 5 digits, or V or E followed by up to four digits. The
#' decimal form has a decimal point to delimit the top-level (henceforth
#' \emph{major}) category, and the \emph{minor} part containing the subsidiary
#' classifications.
#'
#' @section Structures: Structures used in this package are: \itemize{ \item
#'   vector (usually character vectors) of \emph{short} or \emph{long} codes
#'   \item list, containing two elements with equal length vectors called
#'   \code{major} \code{minor}. Yes, this is very like a data frame, but we do
#'   not need the overhead of structuring it that way. \item two vectors of
#'   separated major and minor parts }
#' @template major
#' @template minor
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @param parts data.frame with major and minor fields. This can be given
#'   instead of major and minor vectors
#' @param minorEmpty vector of length one, to be used in place of minor part of
#'   zero. Defaults to ""
#' @template short_code
#' @return Character vector or list. Deliberately returns zero-padded major,
#'   because otherwise we are creating ambiguous codes (even if we know what we
#'   mean)
#' @family ICD-9 convert
#' @name convert
NULL

#' convert the chapter headings to lists of codes
#'
#' the chapter headings can be converted into the full set of their children,
#' and then used to look-up which chapter, sub-chapter, or 'major' a given code
#' belongs. Always returns a map with short-form ICD-9 codes. These can be
#' converted in bulk with \code{lapply} and \code{icd9ShortToDecimal}.
#' @param x Either a chapter list itself, or the name of one, e.g.
#'   \code{icd9_sub_chapters}
#' @family ICD-9 convert
#' @keywords internal manip
icd9_chapters_to_map <- function(x) {
  if (is.character(x) && exists(x))
    x <- get(x)
  assert_list(x, types = "character", any.missing = FALSE, min.len = 1, names = "unique")
  ranges <- names(x)
  map <- list()
  for (r in ranges) {
    map[[r]] <- icd_expand_range.icd9(x[[r]][1], x[[r]][2], short_code = TRUE, defined = FALSE)
  }
  map
}

#' Convert ICD data from wide to long format
#'
#' Reshaping data is a common task, and is made easier here by knowing more about
#' the underlying structure of the data. This function wraps the
#' \code{\link[stats]{reshape}} function with specific behavior and checks
#' related to ICD codes. Empty strings and NA values will be dropped, and
#' everything else kept. No validation of the ICD codes is done.
#'
#' @param x \code{data.frame} in wide format, i.e. one row per patient, and
#'   multiple columns containing ICD codes, empty strings or NA.
#' @template visit_name
#' @param icd_labels vector of column names in which codes are found. If NULL,
#'   all columns matching the regular expression \code{icd_regex} will be
#'   included.
#' @template icd_name
#' @param icd_regex vector of character strings containing a regular expression to
#'   identify ICD-9 diagnosis columns to try (case-insensitive) in order.
#'   Default is \code{c("icd", "diag", "dx_", "dx")}
#' @return \code{data.frame} with visit_name column named the same as input, and
#'   a column named by \code{icd.name} containing all the non-NA and non-empty
#'   codes found in the wide input data.
#' @examples
#'   widedf <- data.frame(visit_name = c("a", "b", "c"),
#'     icd9_01 = c("441", "4424", "441"),
#'     icd9_02 = c(NA, "443", NA))
#'   icd_wide_to_long(widedf)
#' @family ICD-9 convert
#' @export
icd_wide_to_long <- function(x,
                             visit_name = get_visit_name(x),
                             icd_labels = NULL,
                             icd_name = "icd_code",
                             icd_regex = c("icd", "diag", "dx_", "dx")) {
  assert_data_frame(x, min.rows = 1, min.cols = 2)
  assert_string(visit_name)
  assert(checkmate::checkNull(icd_labels),
         checkmate::checkCharacter(icd_labels, min.len = 1))
  assert_string(icd_name)
  assert_character(icd_regex, min.chars = 1, any.missing = FALSE, min.len = 1)

  if (is.null(icd_labels)) {
    re <- length(icd_regex)
    while (re > 0) {
      icd_labels <- grep(rev(icd_regex)[re], names(x), ignore.case = TRUE, value = TRUE)
      if (length(icd_labels)) break
      re <- re - 1
    }
  }
  checkmate::assert_character(icd_labels, any.missing = FALSE, min.chars = 1,
                              min.len = 1, max.len = ncol(x) - 1)
  stopifnot(all(icd_labels %in% names(x)))

  # could definitely do this with non-base functions, but this is quick enough
  # and reliable.
  res <- stats::reshape(x,
                        direction = "long",
                        varying = icd_labels,
                        idvar = visit_name,
                        timevar = NULL,
                        v.names = icd_name)

  res <- res[!is.na(res[[icd_name]]), ]
  res <- res[nchar(as_char_no_warn(res[[icd_name]])) > 0, ]
  res <- res[order(res[[visit_name]]), ]
  rownames(res) <- NULL
  res
}

#' Convert ICD data from long to wide format
#'
#' This is more complicated than reshape or \code{reshape2::dcast} allows. This
#' is a reasonably simple solution using built-in functions.
#' @param x data.frame of long-form data, one column for visit_name and one for
#'   ICD code
#' @template visit_name
#' @template icd_name
#' @param prefix character, default \code{icd_} to prefix new columns
#' @param min_width, single integer, if specified, writes out this many columns
#'   even if no patients have that many codes. Must be greater than or equal to
#'   the maximum number of codes per patient.
#' @param aggr single logical value, if TRUE (the default) will take more time
#'   to find out-of-order visit_names, and combine all the codes for each unique
#'   visit_name. If \code{FALSE}, then out-of-order visit_names will result in a
#'   row in the output data per contiguous block of identical visit_names.
#' @param return_df single logical value, if \code{TRUE}, return a data frame
#'   with a field for the visit_name. This may be more convenient, but the
#'   default of \code{FALSE} gives the more natural return data of a matrix with
#'   row names being the visit IDs from \code{visit_name}s.
#' @examples
#'   longdf <- data.frame(visit_name = c("a", "b", "b", "c"),
#'     icd9 = c("441", "4424", "443", "441"))
#'   icd_long_to_wide(longdf)
#'   icd_long_to_wide(longdf, prefix = "ICD10_")
#' @family ICD-9 convert
#' @keywords manip
#' @export
icd_long_to_wide <- function(x,
                             visit_name = get_visit_name(x),
                             icd_name = get_icd_name(x),
                             prefix = "icd_",
                             min_width = 0,
                             aggr = TRUE,
                             return_df = FALSE) {

  assert_data_frame(x, col.names = "unique")
  visit_name <- as_char_no_warn(visit_name)
  icd_name <- as_char_no_warn(icd_name)
  assert_string(prefix, min.chars = 1)
  assert_count(min_width, na.ok = FALSE)
  assert_flag(aggr)
  assert_flag(return_df)

  # we're now going to return a matrix
  icd9_name_was_factor <- is.factor(x[[visit_name]])
  if (icd9_name_was_factor) iv_levels <- levels(x[[visit_name]])
  x[[visit_name]] <- as_char_no_warn(x[[visit_name]])
  x[[icd_name]] <- as_char_no_warn(x[[icd_name]])
  if (return_df) {
    mat <- icd_long_to_wide_cpp(x, visit_name, icd_name, aggr)
    if (icd9_name_was_factor)
      rownm <- factor(x = rownames(mat), levels = iv_levels)
    else
      rownm <- rownames(mat)
    df.out <- cbind(rownm, as.data.frame(unname(mat)), stringsAsFactors = icd9_name_was_factor)
    names(df.out)[1] <- visit_name
    # perhaps leave (duplicated) rownames which came from the matrix:
    rownames(df.out) <- NULL
    nc <- ncol(df.out) - 1
    if (nc < min_width) {
      df.out <- cbind(df.out, matrix(rep(NA, min_width - nc), nrow = 1))
      nc <- min_width
    }
    names(df.out)[-1] <- paste(prefix, sprintf("%03d", 1:nc), sep = "")
    df.out
  } else {
    icd_long_to_wide_cpp(x, visit_name, icd_name, aggr)
  }
}

#' convert comorbidity data frame from matrix
#'
#' convert matrix of comorbidities into data frame, preserving visit_name
#' information
#' @param x Matrix of comorbidities, with row and columns names defined
#' @param visit_name Single character string with name for new column in output
#'   data frame. Everywhere else, \code{visit_name} describes the input data,
#'   but here it is for output data.
#' @template stringsAsFactors
#' @examples
#' longdf <- icd_long_data(
#'             visit_id = c("a", "b", "b", "c"),
#'             icd9 = as.icd9(c("441", "4424", "443", "441")))
#' mat <- icd9_comorbid_elix(longdf)
#' class(mat)
#' typeof(mat)
#' rownames(mat)
#' df.out <- icd_comorbid_mat_to_df(mat)
#' stopifnot(is.data.frame(df.out))
#' # output data frame has a factor for the visit_name column
#' stopifnot(identical(rownames(mat), as.character(df.out[["visit_id"]])))
#' df.out[, 1:4]
#' # when creating a data frame like this, stringsAsFactors uses
#' # the system-wide option you may have set e.g. with
#' # options("stringsAsFactors" = FALSE).
#' is.factor(df.out[["visit_id"]])
#' @export
icd_comorbid_mat_to_df <- function(x, visit_name = "visit_id",
                                   stringsAsFactors = getOption("stringsAsFactors")) { # nolint
  assert_matrix(x, min.rows = 1, min.cols = 1, row.names = "named", col.names = "named")
  assert_string(visit_name)
  assert_flag(stringsAsFactors) # nolint
  out <- data.frame(rownames(x), x, stringsAsFactors = stringsAsFactors) # nolint
  names(out)[1] <- visit_name
  rownames(out) <- NULL
  out
}

#' convert comorbidity matrix to data frame
#'
#' convert matrix of comorbidities into data frame, preserving visit_name
#' information
#' @param x data frame, with a \code{visit_name} column (not necessarily first),
#'   and other columns with flags for comorbidities, as such column names are
#'   required.
#' @template visit_name
#' @template stringsAsFactors
#' @examples
#' longdf <- icd_long_data(
#'             visit = c("a", "b", "b", "c"),
#'             icd9 = c("441", "4424", "443", "441")
#'             )
#' cmbdf <- icd9_comorbid_elix(longdf, return_df = TRUE)
#' class(cmbdf)
#' rownames(cmbdf)
#' mat.out <- icd_comorbid_df_to_mat(cmbdf)
#' stopifnot(is.matrix(mat.out))
#' mat.out[, 1:4]
#' @export
icd_comorbid_df_to_mat <- function(x, visit_name = get_visit_name(x),
                                   stringsAsFactors = getOption("stringsAsFactors")) { # nolint
  assert_data_frame(x, min.rows = 1, min.cols = 2, col.names = "named")
  assert_string(visit_name)
  assert_flag(stringsAsFactors) # nolint

  out <- as.matrix.data.frame(x[-which(names(x) == visit_name)])
  rownames(out) <- x[[visit_name]]
  out
}

#' Convert ICD codes from short to decimal forms
#'
#' Convert codes between short and decimal forms
#' @param x ICD codes
#' @export
icd_short_to_decimal <- function(x) {
  UseMethod("icd_short_to_decimal")
}

#' @describeIn icd_short_to_decimal convert ICD codes of unknown type from short
#'   to decimal format
#' @export
#' @keywords internal
icd_short_to_decimal.default <- function(x) {
  switch(
    icd_guess_version.character(as_char_no_warn(x), short_code = TRUE),
    "icd9" = icd_short_to_decimal.icd9(x),
    "icd10" = icd_short_to_decimal.icd10(x),
    stop("ICD type not known")
  )
}

#' @describeIn icd_short_to_decimal convert ICD-9 codes from short to decimal
#'   format
#' @export
#' @keywords internal
icd_short_to_decimal.icd9 <- function(x) {
  icd9(as.icd_decimal_diag(icd9_short_to_decimal_cpp(x)))
}

#' @describeIn icd_short_to_decimal convert ICD-10 codes from short to decimal
#'   format
#' @export
#' @keywords internal
icd_short_to_decimal.icd10 <- function(x) {
  x <- trim(x)
  # todo: these could/should be seperate functions
  out <- str_sub(x, 0, 3) # majors
  minors <- str_sub(x, 4)
  out[minors != ""] <- paste0(out, ".", minors)
  icd10(as.icd_decimal_diag(out)) # not as.icd10
}

#' @describeIn icd_short_to_decimal convert ICD-10-CM code from short to decimal
#'   format
#' @export
#' @keywords internal
icd_short_to_decimal.icd10cm <- function(x) {
  icd_short_to_decimal.icd10(x) %>% as.icd10cm
}

#' Convert Decimal format ICD codes to short format
#'
#' This usually just entails removing the decimal point, but also does some
#' limited validation and tidying up.
#'
#' @param x ICD codes
#' @export
#' @keywords internal
icd_decimal_to_short <- function(x) {
  UseMethod("icd_decimal_to_short")
}

#' @export
#' @keywords internal
icd_decimal_to_short.icd9 <- function(x) {
  if (is.factor(x)) {
    levels(x) <- icd9(as.icd_short_diag(icd9_decimal_to_short_cpp(levels(x))))
    return(x)
  }
  icd9(as.icd_short_diag(icd9_decimal_to_short_cpp(x)))
}

#' @export
#' @keywords internal
icd_decimal_to_short.icd10 <- function(x) {
  if (is.factor(x)) {
    levels(x) <- gsub("\\.", "", levels(x))
    return(icd10(as.icd_short_diag(x)))
  }
  icd10(as.icd_short_diag(gsub("\\.", "", x))) # not as.icd10
}

#' @export
#' @keywords internal
icd_decimal_to_short.icd10cm <- function(x) {
  as.icd10cm(icd_decimal_to_short.icd10(x))
}

#' @describeIn icd_decimal_to_short Guess ICD version and convert decimal to
#'   short format
#' @export
#' @keywords internal
icd_decimal_to_short.default <- function(x) {
  if (is.factor(x)) {
    levels(x) <- as.icd_short_diag(trim(gsub("\\.", "", levels(x))))
    return(x)
  }
  as.icd_short_diag(trim(gsub("\\.", "", x)))
}

#' Convert decimal ICD codes to component parts
#' @export
#' @keywords internal
icd_decimal_to_parts <- function(x, minor_empty = "") {
  UseMethod("icd_decimal_to_parts")
}

#' Convert short format ICD codes to component parts
#' @export
#' @keywords internal
icd_short_to_parts <- function(x, minor_empty = "") {
  UseMethod("icd_short_to_parts")
}

#' @describeIn icd_short_to_parts Convert short format ICD-9 code to parts
#' @export
#' @keywords internal manip
icd_short_to_parts.icd9 <- function(x, minor_empty = "") {
  # Cannot specify default values in both header and C++ function body, so use a
  # shim here.
  .Call("icd_icd9ShortToPartsCpp", PACKAGE = "icd", x, minor_empty)
}

#' @describeIn icd_decimal_to_parts Convert decimal ICD-9 code to parts
#' @export
#' @keywords internal manip
icd_decimal_to_parts.icd9 <- function(x, minor_empty = "") {
  .Call("icd_icd9DecimalToPartsCpp", PACKAGE = "icd", x, minor_empty)
}

#' @describeIn icd_short_to_parts Convert short format ICD code to parts,
#'   guessing whether ICD-9 or ICD-10
#' @export
#' @keywords internal manip
icd_short_to_parts.character <- function(x, minor_empty = "") {
  # Cannot specify default values in both header and C++ function body, so use a
  # shim here.
  switch(
    icd_guess_version(x, short_code = TRUE),
    "icd9" = .Call("icd_icd9ShortToPartsCpp", PACKAGE = "icd", x, minor_empty),
    "icd10" = icd_short_to_parts.icd10(x, minor_empty = minor_empty),
    stop("Unknown ICD version guessed from input")
  )
}

#' @describeIn icd_short_to_parts Convert decimal ICD code to parts, guessing
#'   ICD version
#' @export
#' @keywords internal manip
icd_decimal_to_parts.character <- function(x, minor_empty = "") {
  switch(
    icd_guess_version(x, short_code = FALSE),
    "icd9" = .Call("icd_icd9DecimalToPartsCpp", PACKAGE = "icd", x, minor_empty),
    "icd10" = icd_decimal_to_parts.icd10(x, minor_empty = minor_empty),
    stop("Unknown ICD version guessed from input")
  )
}
