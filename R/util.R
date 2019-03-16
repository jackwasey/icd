# save this in package environment so it doesn't need to be done on the fly
.have_regexec_perl <- "perl" %in% names(as.list(regexec))

#' Trim leading and trailing white space from a single string
#'
#' \code{NA} is accepted (and returned as \code{NA_character_})
#' @param x character vector of length one
#' @return character vector of length one
#' @keywords internal character
#' @noRd
strim <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  if (!is.na(x)) {
    strimCpp(as.character(x))
  } else {
    return(NA_character_)
  }
}

"%nin%" <- function(x, table)
  match(x, table, nomatch = 0L) == 0L

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
#' After benchmarking, \code{gsub} is probably quicker than
#' \code{stringr}/\code{stringi}. For comorbidity processing.
#' @param x character vector
#' @param pattern passed to \code{gsub} default is " "
#' @param use_bytes single logical passed to \code{base::gsub}, default is the
#'   slightly quicker \code{TRUE}
#' @return character vector of same length as input
#' @keywords internal
#' @noRd
strip <- function(x, pattern = " ", use_bytes = TRUE)
  gsub(
    pattern = pattern, replacement = "", x = x,
    fixed = TRUE, useBytes = use_bytes
  )

#' Encode \code{TRUE} as 1, and \code{FALSE} as 0 (integers)
#'
#' When saving data as text files for distribution, printing large amounts of
#' text containing \code{TRUE} and \code{FALSE} is inefficient. Converting to
#' binary allows more compact output to screen. Most functions in \R will
#' convert \code{TRUE} and \code{FALSE} logical flags to \code{1} and \code{0},
#' respectively.
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
  if (any(is.na(logical_fields)) || length(logical_fields) == 0) {
    return(x)
  }
  # update just the logical fields with integers
  x[, logical_fields] <-
    vapply(
      X = x[, logical_fields],
      FUN = function(y) ifelse(y, 1L, 0L),
      FUN.VALUE = integer(length = dim(x)[1])
    )
  x
}

#' @describeIn logical_to_binary Convert integer columns to logical values
binary_to_logical <- function(x) {
  stopifnot(is.data.frame(x) || is.matrix(x))
  if (is.matrix(x)) {
    stopifnot(is.matrix(x), nrow(x) > 0, ncol(x) > 0)
    mode(x) <- "logical"
    return(x)
  }
  stopifnot(nrow(x) > 0, ncol(x) > 0)
  integer_fields <- names(x)[vapply(x, is.integer, logical(1))]
  if (any(is.na(integer_fields)) || length(integer_fields) == 0) {
    return(x)
  }
  # update just the logical fields with integers
  x[, integer_fields] <-
    vapply(
      X = x[, integer_fields],
      FUN = function(y) ifelse(y, TRUE, FALSE),
      FUN.VALUE = logical(length = dim(x)[1])
    )
  x
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

#' @describeIn get_visit_name Guess or get visit/patient column from data frame
#' @keywords internal
get_visit_name.data.frame <- function(x, visit_name = NULL) {
  stopifnot(is.data.frame(x))
  stopifnot(is.null(visit_name) ||
    (is.character(visit_name) && length(visit_name) == 1L))
  visit_name_guesses <- c(
    "visit.?Id", "patcom", "encounter.?id", "enc.?id",
    "in.*enc", "out.*enc", "encounter", "visit", "^id$",
    "^enc"
  )
  if (is.null(visit_name)) {
    for (guess in visit_name_guesses) {
      guess_matched <- grep(guess, names(x), ignore.case = TRUE, value = TRUE)
      if (length(guess_matched) == 1) {
        visit_name <- guess_matched
        break
      }
    }
    if (is.null(visit_name)) {
      visit_name <- names(x)[1]
    }
  }
  assert_string(visit_name)
  stopifnot(visit_name %in% names(x))
  visit_name
}

#' @describeIn get_visit_name Give useful error message if matrix passed, as we
#'   assume it is a comorbidity matrix. It is possible you have a character
#'   matrix with all your patient data, and if so, please convert it to a
#'   `data.frame` and file an issue on github.
#' @keywords internal
get_visit_name.matrix <- function(x, visit_name = NULL)
  stop(
    "matrices of comorbidity data are expected to be of logical type, ",
    "and have row names corresponding to the visit or patient."
  )

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
get_icd_dx_name <- function(
                            x,
                            icd_name = NULL,
                            valid_codes = TRUE,
                            defined_codes = FALSE,
                            multi = FALSE) {
  if (!is.null(icd_name)) {
    stopifnot(all(icd_name %in% names(x)))
    return(icd_name)
  }
  if (any(grepl(pattern = "poa", icd_name, ignore.case = TRUE))) {
    warning("'POA' Present-on-arrival fields in 'icd_name'.")
  }
  icd_name <- guess_icd_col_by_class(x, pattern = icd_dx_not_generic)
  if (!is.null(icd_name)) return(icd_name)
  icd_pc_name <- guess_icd_col_by_class(x, pattern = icd_pc_not_generic)
  icd_generic <- guess_icd_col_by_class(x, pattern = c("icd9", "icd10"))
  if (is.null(icd_pc_name) && !is.null(icd_name)) return(icd_generic)
  icd_name <- guess_icd_col_by_name(x,
    valid_codes = valid_codes,
    defined_codes = defined_codes
  )
  if (is.null(icd_name)) {
    icd_name <- character()
    for (n in names(x)) {
      pc <- get_icd_defined_percent(x[[n]])
      if (pc$icd9 > 25 || pc$icd10 > 25) {
        icd_name <- c(icd_name, n)
      }
    }
  }
  if (nrow(x) < 2 || (!valid_codes && !defined_codes)) {
    return(icd_name)
  }
  pc <- if (defined_codes) {
    get_icd_defined_percent(x[icd_name[1]])
  } # TODO vectorize this function
  else {
    get_icd_valid_percent(x[icd_name[1]])
  }
  if (pc$icd9 < 10 && pc$icd10 < 10) {
    stop(
      "identified field with ICD codes as: '", icd_name,
      "' but fewer than 10% of codes are valid ICD-9 or ICD-10. ",
      "If this really is a valid column, identify the field containing ",
      "ICD codes in the input data using 'icd_name=\"my_icd_field\"' or ",
      "set the class using something like",
      " x[[icd_name]] <- as.icd9[[x[[icd_name]]"
    )
  }
  icd_name
}

#' @rdname get_icd_dx_name
#' @keywords internal
get_icd_name <- get_icd_dx_name

#' Uses the columns which contain ICD-9 or ICD-10-CM procedure codes
#'
#' Will also guess procedure codes from other national ICD versions, when
#' supported.
#' @param x Data frame in which to look for the procedure name columns
#' @param icd_name character vector of the column names containing the procedure
#'   codes. This is usually not known in advance, but if known, will be passed
#'   through.
#' @seealso \code{\link{get_icd_dx_name}}
#' @keywords internal
get_icd_pc_name <- function(x, icd_name = NULL) {
  if (!is.null(icd_name)) {
    stopifnot(all(icd_name %in% names(x)))
    return(icd_name)
  }
  if (any(grepl(pattern = "poa", icd_name, ignore.case = TRUE))) {
    warning("'POA' Present-on-arrival field name in 'icd_name'.")
  }
  icd_name <- guess_icd_col_by_class(x, pattern = icd_pc_not_generic)
  if (!is.null(icd_name)) return(icd_name)
  guess_icd_pc_col_by_name(x)
}

#' Get candidate column(s) from wide or long data frame frame, using hints
#' @examples
#' wide_df <- data.frame(
#'   a = letters,
#'   dx0 = icd9_map_elix$CHF[1:26],
#'   dx1 = icd9_map_elix$PVD[1:26],
#'   dx2 = icd9_map_elix$HTN[1:26]
#' )
#' icd:::guess_icd_col_by_name(wide_df)
#' wide_dc <- data.frame(
#'   a = letters,
#'   dx0 = as.icd9cm(icd9_map_elix$CHF[1:26]),
#'   dx1 = as.icd9cm(icd9_map_elix$PVD[1:26]),
#'   dx2 = as.icd9cm(icd9_map_elix$HTN[1:26]),
#'   stringsAsFactors = FALSE
#' )
#' icd:::guess_icd_col_by_name(wide_dc)
#' @return Zero, one or many names of columns likely to contain ICD codes based
#'   on the column names.
#' @keywords internal
guess_icd_col_by_name <- function(
                                  x,
                                  valid_codes = TRUE,
                                  defined_codes = FALSE,
                                  guesses = c(
                                    "icd.?(9|10)",
                                    "icd.?(9|10).?Code",
                                    "icd",
                                    "diagnos",
                                    "diag.?code",
                                    "diag",
                                    "dx",
                                    "i(9|10)",
                                    "code"
                                  ),
                                  class_pattern = icd_dx_not_generic) {
  stopifnot(is.data.frame(x))
  stopifnot(is.logical(valid_codes), length(valid_codes) == 1L)
  stopifnot(is.logical(defined_codes), length(defined_codes) == 1L)
  stopifnot(is.character(guesses))
  # if one column exactly has a class like icd9, then we're done.
  icd_name_by_class <- guess_icd_col_by_class(x, pattern = class_pattern)
  if (!is.null(icd_name_by_class)) return(icd_name_by_class)
  guessed <- lapply(guesses,
    grep,
    x = names(x),
    ignore.case = TRUE,
    value = TRUE
  )
  guess_counts <- vapply(guessed, length, integer(1))
  guesses_logical <- as.logical(guess_counts)
  if (sum(guesses_logical) == 1L) {
    return(unlist(guessed[guesses_logical]))
  }
  best_guess <- which(guess_counts == max(guess_counts))
  if (any(guess_counts > 0L) && length(best_guess) > 0L) {
    return(guessed[[best_guess[1]]])
  }
  NULL
}

guess_icd_pc_col_by_name <- function(
                                     x,
                                     valid_codes = TRUE,
                                     defined_codes = FALSE,
                                     guesses = c(
                                       "icd.?(9|10).?(proc|p).?(code|c)?",
                                       "icd.*pc",
                                       "proced.*",
                                       "proc.?code",
                                       "pc",
                                       "i(9|10).*pc",
                                       "pc.*i(9|10)",
                                       "proc"
                                     ),
                                     class_pattern = icd_dx_not_generic) {
  guess_icd_col_by_name(
    x = x,
    guesses = guesses,
    class_pattern = icd_pc_not_generic
  )
}

#' @describeIn guess_icd_col_by_name Just use the class of columns
#' @keywords internal
guess_icd_col_by_class <- function(x, pattern) {
  cls <- lapply(x, class)
  clg <- vapply(cls, function(z) any(z %in% pattern), logical(1))
  if (any(clg)) return(names(x)[clg])
  NULL
}

na_to_false <- function(x) {
  stopifnot(is.logical(x))
  x[is.na(x)] <- FALSE
  x
}

#' make a list using input argument names as names
#' @param ... arguments whose names become list item names, and whose values
#'   become the values in the list
#' @examples
#' a <- c(1, 2)
#' b <- c("c", "d")
#' stopifnot(
#'   identical(
#'     named_list(a, b),
#'     list(a = a, b = b)
#'   )
#' )
#' @noRd
#' @keywords internal
named_list <- function(...) {
  x <- list(...)
  names(x) <- as.character(match.call()[-1])
  x
}

# nocov start

#' \code{stringr} does this, but here we have a small amount of base R code
#' @noRd
#' @keywords internal
str_extract <- function(string, pattern, ...)
  vapply(regmatches(string, m = regexec(pattern = pattern, text = string, ...)),
    FUN = `[[`, 1, FUN.VALUE = character(1L)
  )

capitalize_first <- function(x)
  trimws(paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))))
# nocov end

#' Get the raw data directory
#'
#' Following Hadley Wickham recommendations in R Packages, this should be in
#' \code{inst/extdata}. \pkg{devtools} overrides \code{system.file}.
#' @noRd
#' @keywords internal
get_raw_data_dir <- function()
  system.file("extdata", package = "icd")

#' return all matches for regular expression
#' @noRd
#' @keywords internal manip
str_match_all <- function(string, pattern, ...) {
  string <- as.character(string)
  regmatches(x = string, m = regexec(pattern = pattern, text = string, ...))
}

# optional nice error message, could just fall back on icd.data::
req_icd_data <- function() {
  if (!icd_data_ver_ok()) {
    stop("Please install the 'icd.data' package to explain ICD codes.",
      call. = FALSE
    )
  }
}

icd_data_ver_ok <- function() {
  # Bug in R? Version of an already lodaded namespace is not checked if done via
  # requireNamespace, only loadNamespace.
  res <- requireNamespace("icd.data", quietly = TRUE)
  res <- res && getNamespaceVersion(
    asNamespace("icd.data")
  ) >=
    as.package_version("1.1")
  res
}

get_from_icd_data <- function(
                              name,
                              alt = NULL,
                              must_work = TRUE) {
  # this will try find lazy data first, then active bindings, functions
  out <- try(silent = TRUE, {
    base::getExportedValue(asNamespace("icd.data"), name)
  })
  if (!inherits(out, "try-error") && !is.null(out)) {
    return(out)
  }
  out <- try(silent = TRUE, {
    as.environment(getNamespace("icd.data"))[[name]]
  })
  if (!inherits(out, "try-error") && !is.null(out)) {
    return(out)
  }
  if (must_work) {
    stop("Unable to get '", name, "' from icd.data")
  }
  alt
}

stop_data_lt_1dot1 <- function() {
  stop("WHO and 2014, 2015, 2017, 2018 and 2019 ICD-CM data are only ",
    "available with icd.data >= 1.1 . Use
       install.packages(\"icd.data\")
       to install the latest version from CRAN.
       devtools::install_github(\"jackwasey/icd.data\")
       may be used until CRAN has version 1.1 .
       ",
    call. = FALSE
  )
}
