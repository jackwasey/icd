#' Explain ICD-9 and ICD-10 codes in English from decimal  (123.45 style),
#' Tabulates the decimal format alongside converted non-decimal format.
#'
#' Output is ordered in the same order as the input. A logical column
#' \code{ismajor} indicates if the code is a parent Category.
#'
#' If the code is both a valid ICD9 and a ICD10 the output will default the
#' descriptions to ICD10.  The code would otherwise have to be explicitly cast
#' to get ICD9 descriptions.
#'
#' A column for source year may be added in the future. Other changes may occur
#' this new feature gets testing and use.
#' @param x vector or other structure of ICD codes to explain in human language
#' @template short_code
#' @param condense single logical value which indicates whether to condense the
#'   given set of ICD-9 codes by replacing subsets of codes with 'parent' codes
#'   which exactly encompass certain subsets. E.g. If all cholera diagnoses are
#'   provided, only '001 - Cholera' needs to be displayed, not all sub-types.
#' @param brief single logical value, default is \code{FALSE}. If \code{TRUE},
#'   the short description from the canonical CMS descriptions will be used,
#'   otherwise the long description is used.
#' @param warn single logical value, default is \code{TRUE}, meaning that codes
#'   which do not correspond to diagnoses, or to three-digit codes, will trigger
#'   a warning.
#' @template dotdotdot
#' @return data frame with fields for ICD-9 code, name and description. The
#'   ordering is in the same order as input, including rows filled with NA for
#'   invalid input codes
#' @export
explain_table <- function(...)
  UseMethod("explain_table")

#' @describeIn explain_table explaining ICD codes from a character vector,
#'   guessing ICD version
#' @details If the input x is of mixed type it will choose to convert by
#' @export
#' @keywords internal
explain_table.default <- function(x, short_code = guess_short(x),
                                  condense = FALSE, brief = TRUE,
                                  warn = TRUE, ...) {
  ver <- guess_version(x, short_code = short_code)
  if (ver %in% icd9_classes) {
    return(
      explain_table.icd9cm(x,
        short_code = short_code, condense = condense,
        brief = brief, warn = warn, ...
      )
    )
  }
  if (ver %in% icd10_classes) {
    return(
      explain_table.icd10cm(x,
        short_code = short_code, condense = condense,
        brief = brief, warn = warn, ...
      )
    )
  }
  stop("Unknown ICD version in explain_table.default.
       Check the class of the input data, or call either
       explain_table.icd9 or explain_table.icd10 directly.")
}

#' @describeIn explain_table from vector of ICD-9 codes.
#' @export
#' @keywords internal
explain_table.icd9 <- function(...)
  explain_table.icd9cm(...)

#' @describeIn explain_table from vector of ICD-10 codes.
#' @export
#' @keywords internal
explain_table.icd10 <- function(...)
  explain_table.icd10cm(...)

#' set \code{short_to_decimal} attribute
#'
#' Does not convert between decimal and short codes. Calling
#' \code{short_to_decimal} should convert and set the attribute.
#' @keywords internal
shortcode_icd9 <- function(x, short_code = guess_short(x))
  if (!short_code) decimal_to_short.icd9(x) else x

#' @rdname shortcode_icd9
#' @keywords internal
shortcode_icd10 <- function(x, short_code = guess_short(x))
  if (!short_code) decimal_to_short.icd10(x) else x

#' generate table of ICD code explanations
#'
#' common code for generating full table of explanations of ICD codes
#' @author Ed Lee
#' @keywords internal
explain_table_worker <- function(x, hierarchy, short_code, condense,
                                 brief, warn, ...) {
  stopifnot(is.character(x) || is.factor(x))
  stopifnot(is.data.frame(hierarchy))
  stopifnot(is.logical(short_code))
  stopifnot(is.logical(condense))
  stopifnot(is.logical(brief))
  stopifnot(is.logical(warn))
  x <- as_char_no_warn(x)
  xs <- if (!short_code) decimal_to_short.icd9(x) else x
  exptable <- merge(data.frame(code = xs, stringsAsFactors = FALSE),
    hierarchy,
    all.x = TRUE
  )
  # merge has reordered...
  exptable[["is_major"]] <- exptable[["three_digit"]] == exptable[["code"]]
  exptable[["valid_icd9"]] <- is_valid.icd9(xs, short_code = TRUE)
  exptable[["valid_icd10"]] <- is_valid.icd10(xs, short_code = TRUE)
  if (condense) {
    condense_explain_table(exptable)
  } else {
    exptable[match(xs, exptable[["code"]]), ]
  }
}

#' @describeIn explain_table explain character vector of ICD1-10-CM codes
#' @author Ed Lee
#' @export
#' @keywords internal
explain_table.icd9cm <- function(x, short_code = guess_short(x),
                                 condense = FALSE, brief = TRUE,
                                 warn = TRUE, ...) {
  explain_table_worker(
    x = x, hierarchy = icd.data::icd9cm_hierarchy,
    short_code = short_code, condense = condense,
    brief = brief, warn = warn, ...
  )
}

#' @describeIn explain_table explain character vector of ICD1-10-CM codes
#' @author Ed Lee
#' @export
#' @keywords internal
explain_table.icd10cm <- function(x,
                                  short_code = guess_short(x),
                                  condense = FALSE,
                                  brief = TRUE,
                                  warn = TRUE, ...) {
  i <- get_from_icd_data("icd10cm_active", alt = icd.data::icd10cm2016)
  explain_table_worker(
    x = x,
    hierarchy = i,
    short_code = short_code,
    condense = condense,
    brief = brief,
    warn = warn,
    ...
  )
}

#' @describeIn explain_table explain character vector of ICD1-10-CM codes
#' @export
#' @keywords internal
explain_table.icd10who <- function(x,
                                   short_code = guess_short(x),
                                   condense = FALSE,
                                   brief = TRUE,
                                   warn = TRUE,
                                   ...) {
  req_icd_data()
  explain_table_worker(
    x = x, hierarchy = get_from_icd_data("icd10who2016"),
    short_code = short_code, condense = condense,
    brief = brief, warn = warn, ...
  )
}

#' condense \code{explain_table} output down to major codes
#'
#' if a major code appears in the code column, and any children of that major
#' code, the children are aggregated to a list and added to the major code row.
#' This does currently not 'condense' e.g. middle-order codes
#'
#' Unlike \code{explain_table}, preserving order doesn't make sense, since
#' rows anywhere in the list can be aggregated, thus altering order compared to
#' the input. Size of the output will also be different if any condensing was
#' done.
#' @keywords internal
#' @noRd
condense_explain_table <- function(x) {
  condensed_majors <- condense_explain_table_worker(x)
  if (nrow(condensed_majors) == 0) {
    x[["condensed_codes"]] <- x[["code"]]
    x[["condensed_num"]] <- 1L
    return(x)
  }
  # drop rows where major is present in the input
  x <- x[x[["three_digit"]] %nin% x[["code"]] | x$is_major, ]
  # add condensed merge existing major rows
  out <- merge(x, condensed_majors,
    by.x = "code",
    by.y = "three_digit",
    all.x = TRUE
  )
  # NA values are un-condensed, so just fill out:
  out[is.na(out$condensed_codes), "condensed_codes"] <-
    out[is.na(out$condensed_codes), "code"]
  out[is.na(out$condensed_num), "condensed_num"] <- 1L
  out
}

#' generate condensed code and condensed number columns
#'
#' @return details for rows which can be condensed
#' @keywords internal
#' @noRd
condense_explain_table_worker <- function(x) {
  # we can only condense when we have three_digit major
  x <- x[!is.na(x[["three_digit"]]), ]
  if (nrow(x) == 0) return(data.frame())
  condensed <- aggregate(x["code"],
    by = list(x[["three_digit"]]),
    paste, sep = ", ", collapse = ", "
  )
  # code column in result is now a factor, by default
  names(condensed) <- c("three_digit", "condensed_codes")
  condensed[["condensed_num"]] <-
    aggregate(x["code"], by = list(x[["three_digit"]]), length)[[2]]
  condensed[condensed[["condensed_num"]] != 1L, ]
}
