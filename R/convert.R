# Copyright (C) 2014 - 2016  Jack O. Wasey
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

#' @name convert
#' @title Convert ICD9 codes between formats and structures.
#' @description ICD-9 codes are represented in \emph{short} and \emph{decimal} forms.
#' The short form has up to 5 digits, or V or E followed by up to four digits. The decimal form
#' has a dedcimal point to delimit the top-level (henceforth \emph{major}) category, and the
#' \emph{minor} part containing the subsidiary classifications.
#'
#' @section Structures: Structures used in this package are:
#' \itemize{
#'   \item vector (usually character vectors) of \emph{short} or \emph{long} codes
#'   \item list, containing two elements with equal length vectors called \code{major}
#'   \code{minor}. Yes, this is very like a data frame, but we do not need the overhead
#'   of structuring it that way.
#'   \item two vectors of separated major and minor parts
#'   }
#' @template major
#' @template minor
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @param parts data.frame with major and minor fields. This can be given
#'   instead of major and minor vectors
#' @param minorEmpty vector of length one, to be used in place of
#' minor part of zero. Defaults to ""
#' @template short_code
#' @return Character vector or list. Deliberately returns zero-padded major, because
#'   otherwise we are creating ambiguous codes (even if we know what we mean)
#' @family ICD-9 convert
NULL

#' @title convert the chapter headings to lists of codes
#' @description the chapter headings can be converted into the full set of their
#'   children, and then used to look-up which chapter, sub-chapter, or 'major' a
#'   given code belongs. Always returns a map with short-form icd-9 codes. These
#'   can be converted en masse with \code{lapply} and \code{icd9ShortToDecimal}.
#' @param x Either a chapter list itself, or the name of one, e.g.
#'   icd9ChaptersSub
#' @family ICD-9 convert
#' @keywords internal manip
icd9ChaptersToMap <- function(x) {
  if (is.character(x) && exists(x))
    x <- get(x)
  assertList(x, types = "character", any.missing = FALSE, min.len = 1, names = "unique")
  ranges <- names(x)
  map <- list()
  for (r in ranges) {
    map[[r]] <- icd_expand_range.icd9(x[[r]][1], x[[r]][2], short_code = TRUE, defined = FALSE)
  }
  map
}

#' @title convert ICD data from wide to long format
#' @description This is different enough to \code{dcast} in \code{reshape2} that
#'   it needs writing again specifically for ICD codes. This function packages
#'   the core \code{reshape} function. Empty strings and NA values will be
#'   dropped, and everything else kept. No validation of the ICD codes is done.
#' @param x \code{data.frame} in wide format, i.e. one row per patient, and
#'   multiple columns containing ICD codes, empty strings or NA.
#' @template visit_name
#' @param icd_labels vector of column names in which codes are found. If NULL,
#'   all columns matching icd or ICD will be included.
#' @param icd_name character vector length one containing the new column name for
#'   the ICD codes, defaults to "icdCode"
#' @param icd_regex vector of character strings containg a regex to identify ICD-9 diagnosis
#'   columns to try (case-insensitive) in order. Default is \code{c("icd", "diag", "dx_", "dx")}
#' @return data frame with visit_name column named the same as input, and a column
#'   named by \code{icd.name} containing all the non-NA and non-empty codes
#'   found in the wide input data.
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
                             icd_name = "icdCode",
                             icd_regex = c("icd", "diag", "dx_", "dx")) {
  assertDataFrame(x, min.rows = 1, min.cols = 2)
  assertString(visit_name)
  assert(checkNull(icd_labels), checkCharacter(icd_labels))
  assertString(icd_name)
  assertCharacter(icd_regex, min.chars = 1, any.missing = FALSE, min.len = 1)

  #TODO: make S3

  if (is.null(icd_labels)) {
    re <- length(icd_regex)
    while (re > 0) {
      icd_labels <- grep(rev(icd_regex)[re], names(x), ignore.case = TRUE, value = TRUE)
      if (length(icd_labels)) break
      re <- re - 1
    }
  }
  checkmate::assertCharacter(icd_labels, any.missing = FALSE, min.chars = 1,
                             min.len = 1, max.len = ncol(x) - 1)
  stopifnot(all(icd_labels %in% names(x)))

  res <- stats::reshape(x,
                        direction = "long",
                        varying = icd_labels,
                        idvar = visit_name,
                        timevar = NULL,
                        v.names = icd_name)

  res <- res[!is.na(res[[icd_name]]), ]
  res <- res[nchar(asCharacterNoWarn(res[[icd_name]])) > 0, ]
  res <- res[order(res[[visit_name]]), ]
  rownames(res) <- NULL
  res
}

#' @title convert ICD data from long to wide format
#' @description This is more complicated than reshape or reshape2::dcast allows.
#'   This is a reasonably simple solution using built-in functions.
#' @param x data.frame of long-form data, one column for visit_name and one
#'   for ICD code
#' @template visit_name
#' @template icd_name
#' @param prefix character, default "icd_" to prefix new columns
#' @param min_width, single integer, if specified, writes out this many columns
#'   even if no patients have that many codes. Must be greater than or equal to
#'   the maximum number of codes per patient.
#' @param aggr single logical value, if TRUE (the default) will take more
#'   time to find out-of-order visit_names, and combine all the codes for each
#'   unique visit_name. If \code{FALSE}, then out-of-order visit_names will result in
#'   a row in the output data per contiguous block of identical visit_names.
#' @param return_df single logical value, if \code{TRUE}, return a data frame
#'   with a field for the visit_name. This may be more convenient, but the default
#'   of \code{FALSE} gives the more natural return data of a matrix with
#'   rownames being the visit_names.
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

  #  icd_name <- get_icd_name(x, icd_name)
  #  visit_name <- get_visit_name(x, visit_name)

  assertDataFrame(x, col.names = "named")
  assertString(prefix)
  assertCount(min_width, na.ok = FALSE)
  assertFlag(aggr)
  assertFlag(return_df)

  # we're now going to return a matrix
  icd9VisitWasFactor <- is.factor(x[[visit_name]])
  if (icd9VisitWasFactor) ivLevels <- levels(x[[visit_name]])
  x[[visit_name]] <- asCharacterNoWarn(x[[visit_name]])
  x[[icd_name]] <- asCharacterNoWarn(x[[icd_name]])
  if (return_df) {
    mat <- icd9LongToWideCpp(x, visit_name, icd_name, aggr)
    if (icd9VisitWasFactor)
      rownm <- factor(x = rownames(mat), levels = ivLevels)
    else
      rownm <- rownames(mat)
    df.out <- cbind(rownm, as.data.frame(unname(mat)), stringsAsFactors = icd9VisitWasFactor)
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
    icd9LongToWideCpp(x, visit_name, icd_name, aggr)
  }
}

#' @title convert comorbidity data frame from matrix
#' @description convert matrix of comorbidities into data frame, preserving visit_name
#'   information
#' @param x Matrix of comorbidities, with row and columns names defined
#' @param visit_name Single character string with name for new column in output
#'   data frame. Everywhere else, \code{visit_name} describes the input data,
#'   but here it is for output data.
#' @template stringsAsFactors
#' @examples
#' library(magrittr) # as ever, optional, but tidy
#' longdf <- data.frame(visit_id = c("a", "b", "b", "c"),
#'                      icd9 = icd9(c("441", "4424", "443", "441"))) %>% icd_long_data %>% icd9
#' mat <- icd_comorbid_elix(longdf)
#' class(mat)
#' typeof(mat)
#' rownames(mat)
#' df.out <- icd_comorbid_mat_to_df(mat)
#' stopifnot(is.data.frame(df.out))
#' # output data frame has a factor for the visit_name column
#' stopifnot(identical(rownames(mat), as.character(df.out[["visit_id"]])))
#' df.out[, 1:4]
#' # when creating a data frame like this, stringsAsFactors uses the system-wide option you have set.
#' is.factor(df.out[["visit_id"]])
#' @export
icd_comorbid_mat_to_df <- function(x, visit_name = "visit_id",
                                   stringsAsFactors = getOption("stringsAsFactors")) {
  assertMatrix(x, min.rows = 1, min.cols = 1, row.names = "named", col.names = "named")
  assertString(visit_name)
  assertFlag(stringsAsFactors)
  out <- data.frame(rownames(x), x, stringsAsFactors = stringsAsFactors)
  names(out)[1] <- visit_name
  rownames(out) <- NULL
  out
}

#' @title convert comorbidity matrix to data frame
#' @description convert matrix of comorbidities into data frame, preserving visit_name
#'   information
#' @param x data frame, with a \code{visit_name} column (not necessarily first),
#'   and other columns with flags for comorbidities, as such column names are
#'   required.
#' @template visit_name
#' @template stringsAsFactors
#' @examples
#' longdf <- icd9(icd_long_data(
#'             data.frame(visit = c("a", "b", "b", "c"),
#'                        icd9 = c("441", "4424", "443", "441"))))
#' cmbdf <- icd_comorbid_elix(longdf, return_df = TRUE)
#' class(cmbdf)
#' rownames(cmbdf)
#' mat.out <- icd_comorbid_df_to_mat(cmbdf)
#' stopifnot(is.matrix(mat.out))
#' mat.out[, 1:4]
#' @export
icd_comorbid_df_to_mat <- function(x, visit_name = get_visit_name(x),
                                   stringsAsFactors = getOption("stringsAsFactors")) {
  checkDataFrame(x, min.rows = 1, min.cols = 2, col.names = "named")
  checkString(visit_name)
  checkFlag(stringsAsFactors)
  # visit_name <- get_visit_name(x, visit_name)

  out <- as.matrix.data.frame(x[-which(names(x) == visit_name)])
  rownames(out) <- x[[visit_name]]
  out
}

#' Convert ICD from short to deimcal forms
#'
#' Convert codes between short and decimal forms
#' @param x ICD codes
#' @export
icd_short_to_decimal <- function(x) {
  UseMethod("icd_short_to_decimal")
}

#' @export
#' @keywords internal
icd_short_to_decimal.default <- function(x) {
  # we can only have a shot at guessing this if we first guess the version
  x %<>% icd_guess_version_update()
  icd_short_to_decimal(x)
}

#' @export
#' @keywords internal
icd_short_to_decimal.icd9 <- function(x) {
  icd9ShortToDecimalCpp(x) %>% icd_decimal_code %>% icd9
}

#' @export
#' @keywords internal
icd_short_to_decimal.icd10 <- function(x) {
  x %<>% str_trim
  # todo: these could/should be seperate functions
  majors <- str_sub(x, 0, 3)
  minors <- str_sub(x, 4)
  if (minors != "")
    paste0(majors, ".", minors) %>% icd_decimal_code %>% icd10
  else
    paste0(majors) %>% icd_decimal_code %>% icd10
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
  icd9DecimalToShortCpp(x) %>% icd_short_code(warn = FALSE) %>% icd9
}

#' @export
#' @keywords internal
icd_decimal_to_short.icd10 <- function(x) {
  x %>% str_replace("\\.", "") %>% icd_short_code(warn = FALSE) %>% icd10
}

#' @export
#' @keywords internal
icd_decimal_to_short.icd10cm <- function(x) {
  icd_decimal_to_short.icd10(x) %>% icd10cm
}

#' @export
#' @keywords internal
icd_decimal_to_short.icd10who <- function(x) {
  icd_decimal_to_short.icd10(x) %>% icd10who
}


#
# # @export
# # @keywords internal
# icd_decimal_to_short.list <- function(x) {
#   # todo, may need to replicate this elsewhere
#   lapply(x, icd_decimal_to_short)
# }

#' @describeIn icd_decimal_to_short Guess ICD version and convert decimal to
#'   short format
#' @export
#' @keywords internal
icd_decimal_to_short.default <- function(x) {
  str_trim(str_replace(x, "\\.", "")) %>% icd_short_code
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
  # Cannot specify default values in both header and C++ function body, so use a shim here.
  .Call("icd9_icd9ShortToPartsCpp", PACKAGE = get_pkg_name(), x, minor_empty)
}

#' @describeIn icd_decimal_to_parts Convert decimal ICD-9 code to parts
#' @export
#' @keywords internal manip
icd_decimal_to_parts.icd9 <- function(x, minor_empty = "") {
  .Call("icd9_icd9DecimalToPartsCpp", PACKAGE = get_pkg_name(), x, minor_empty)
}

#' @describeIn icd_short_to_parts Convert short format ICD-10 code to parts
#' @export
#' @keywords internal manip
icd_short_to_parts.icd10 <- function(x, minor_empty = "") {
  icd_short_to_parts.icd9(x, minor_empty)
}

#' @describeIn icd_decimal_to_parts Convert decimal ICD-9 code to parts
#' @export
#' @keywords internal manip
icd_decimal_to_parts.icd10 <- function(x, minor_empty = "") {
  icd_decimal_to_parts.icd9(x, minor_empty)
}

#' @describeIn icd_short_to_parts Convert short format ICD code to parts, assuming ICD-9 from character class. TODO: test this works for ICD-10
#' @export
#' @keywords internal manip
icd_short_to_parts.character <- function(x, minor_empty = "") {
  # Cannot specify default values in both header and C++ function body, so use a shim here.
  .Call("icd9_icd9ShortToPartsCpp", PACKAGE = get_pkg_name(), x, minor_empty)
}

#' @describeIn icd_short_to_parts Convert decimal ICD code to parts, assuming ICD-9 from character class. TODO: test this works for ICD-10
#' @export
#' @keywords internal manip
icd_decimal_to_parts.character <- function(x, minor_empty = "") {
  .Call("icd9_icd9DecimalToPartsCpp", PACKAGE = get_pkg_name(), x, minor_empty)
}
