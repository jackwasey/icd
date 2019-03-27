#' Explain ICD-9 and ICD-10 codes in English
#'
#' Convert 'decimal' format (123.45 style) ICD-9 codes into the name and
#' description for human review there are official ICD9-CM data tables, not with
#' conversion to decimal notation, but to the textual format.
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
#' @param lang For WHO ICD-10 codes, the 2016 English and 2008 French
#'   translations are available. Use 'en' or 'fr' respectively. For ICD-10-CM
#'   codes, Dutch is also available, indicated by 'nl'. If \code{icd.data} 1.0
#'   is installed, English descriptions are returned.
#' @template dotdotdot
#' @examples
#' if (requireNamespace("icd.data", quietly = TRUE)) {
#' # by default, just show parent code and ignore children (428.0 not shown
#' # because 428 is present):
#' explain_code(icd9_map_ahrq$CHF[1:3])
#' # same without condensing the list. In this case, 428.0 is shown:
#' explain_code(icd9_map_ahrq$CHF[1:3], brief = TRUE)
#' # The first three in the ICD-10 equivalent are a little different:
#' explain_code(icd10_map_ahrq$CHF[1:3], brief = TRUE)
#' }
#' @return data frame, or list of data frames, with fields for ICD-9 code, name
#'   and description. There is no guarantee on the order of the returned
#'   descriptions. \code{explain_table} is designed to provide results in a
#'   reliable order (when not condensing codes, at least).
#' @export
explain_code <- function(...) {
  require_icd_data()
  UseMethod("explain_code")
}

#' @rdname explain_code
#' @details \code{explain_icd} is a synonym for \code{\link{explain_code}}.
#' @keywords internal
explain_icd <- function(...) {
  explain_code(...)
}

#' @describeIn explain_code Explain ICD codes from a character vector, guessing
#'   ICD version
#' @export
explain_code.default <- function(x,
                                 short_code = guess_short(x),
                                 condense = TRUE,
                                 brief = FALSE,
                                 warn = TRUE,
                                 ...) {
  switch(
    guess_version.character(as_char_no_warn(x),
      short_code = short_code
    ),
    "icd9" = explain_code.icd9(x,
      short_code = short_code,
      condense = condense,
      brief = brief,
      warn = warn,
      ...
    ),
    "icd10" = explain_code.icd10(x,
      short_code = short_code,
      brief = brief,
      ...
    ),
    stop("Unknown ICD version.")
  )
}

#' @describeIn explain_code Explain all ICD-9 codes in a list of vectors
#' @export
explain_code.list <- function(x, ...)
  lapply(x, explain_code, ...)

#' @describeIn explain_code explain character vector of ICD-9 codes.
#' @export
explain_code.icd9 <- function(...) {
  explain_code.icd9cm(...)
}

#' @describeIn explain_code explain character vector of ICD-9-CM codes
#' @export
explain_code.icd9cm <- function(x,
                                short_code = guess_short(x),
                                condense = TRUE,
                                brief = FALSE,
                                warn = TRUE,
                                ...) {
  if (is.numeric(x)) {
    warning(
      "data is in numeric format. This can easily lead to errors in ",
      "short or decimal codes, e.g. short_code code 1000: is it 10.00 ",
      "or 100.0; or decimal codes, e.g. 10.1 was supposed to be 10.10 ."
    )
    x <- as.character(x)
  }
  assert_fac_or_char(x)
  stopifnot(is.logical(short_code), length(short_code) == 1L)
  stopifnot(is.logical(condense), length(condense) == 1L)
  stopifnot(is.logical(brief), length(brief) == 1L)
  stopifnot(is.logical(warn), length(warn) == 1L)
  if (!short_code) {
    x <- decimal_to_short.icd9(x)
  }
  # if there are only defined codes, we should condense with this in mind:
  if (condense) {
    if (warn && !all(is_defined.icd9(x, short_code = TRUE))) {
      undefined <- x[!is_defined.icd9(x, short_code = TRUE)]
      warning("Some ICD codes are not 'defined' when trying",
        " to condense when explaining codes. ",
        "Consider using warn = FALSE or condense = FALSE. ",
        "Will drop these and continue. Examples: ",
        paste(undefined[seq(from = 1, to = min(5, length(undefined)))],
          collapse = " "
        ),
        call. = FALSE
      )
    }
    x <- condense.icd9(
      get_defined.icd9(x, short_code = TRUE),
      defined = TRUE, short_code = TRUE
    )
  }
  mj <- unique(get_major.icd9(x, short_code = TRUE))
  mjexplain <-
    names(icd.data::icd9_majors)[icd.data::icd9_majors %in% mj[mj %in% x]]
  # don't double count when major is also billable
  x <- x[x %nin% mj]
  desc_field <- ifelse(brief, "short_desc", "long_desc")
  res <- c(
    mjexplain,
    icd.data::icd9cm_hierarchy[
      icd.data::icd9cm_hierarchy[["code"]] %in% x, desc_field
    ]
  )
  if (length(res) != 0) {
    res
  } else {
    NA_character_
  }
}

#' @describeIn explain_code ICD-10-CM explanation, current a minimal
#'   implementation
#' @export
explain_code.icd10cm <- function(x,
                                 short_code = guess_short(x),
                                 condense = TRUE,
                                 brief = FALSE,
                                 warn = TRUE,
                                 ...) {
  stopifnot(is.atomic(x))
  stopifnot(is.logical(short_code), length(short_code) == 1L)
  stopifnot(is.logical(brief), length(brief) == 1L)
  if (!missing(condense)) {
    .NotYetUsed("condense", error = FALSE)
  }
  if (!missing(warn)) {
    .NotYetUsed("warn", error = FALSE)
  }
  if (!short_code) {
    x <- decimal_to_short.icd10(x)
  }
  # this is a alow linear lookup, but usually only
  # "explaining" one or a few codes at a time.
  i <- icd_data_icd10cm_active()
  i[
    i[["code"]] %in% unique(as_char_no_warn(x)),
    ifelse(brief, "short_desc", "long_desc")
  ]
}

#' @describeIn explain_code WHO ICD-10 explanation
#' @export
explain_code.icd10who <- function(x,
                                  short_code = guess_short(x),
                                  condense = TRUE,
                                  brief = NULL,
                                  warn = TRUE,
                                  lang = c("en", "fr"),
                                  ...) {
  if (!is.null(brief)) {
    message(
      "WHO ICD-10 does not have short or long descriptions, ",
      "so the argument `brief` is redundant"
    )
  }
  stopifnot(is.atomic(x))
  stopifnot(is.logical(short_code), length(short_code) == 1)
  lang <- match.arg(lang)
  if (!missing(condense)) {
    .NotYetUsed("condense", error = FALSE)
  }
  if (!missing(warn)) {
    .NotYetUsed("warn", error = FALSE)
  }
  if (!short_code) {
    x <- decimal_to_short.icd10(x)
  }
  # this is a slow linear lookup, but usually only
  # "explaining" one or a few codes at a time.
  i <- if (lang == "fr") {
    .idget("icd10who2008fr")
  } else {
    .idget("icd10who2016")
  }
  i[
    i[["code"]] %in% unique(as_char_no_warn(x)),
    "desc"
  ]
}

#' @describeIn explain_code ICD-10-FR explanation, initial implementation, subject to change
#' @export
explain_code.icd10fr <- function(x, ...) {
  explain_code_worker(x, "icd10fr2019")
}

#' @describeIn explain_code ICD-10-BE explanation, initial implementation, subject to change
#' @examples
#' # Belgian ICD-10 has three languages available
#' if (icd:::icd_data_ver_ok()) {
#' explain_code(as.icd10be("C20"))
#' # [1] "Malignant neoplasm of rectum"
#' explain_code(as.icd10be("C20"), lang = "en")
#' # [1] "Malignant neoplasm of rectum"
#' explain_code(as.icd10be("C20"), lang = "fr")
#' # [1] "néoplasme malin du rectum"
#' explain_code(as.icd10be("C20"), lang = "nl")
#' # [1] "maligne neoplasma van het rectum"
#' }
#' @export
explain_code.icd10be <- function(x,
                                 lang = c("fr", "nl", "en"),
                                 ...) {
  lang <- match.arg(lang)
  explain_code_worker(
    x = x,
    var_name = "icd10be2017",
    lang = lang,
    ...
  )
}

#' @describeIn explain_code ICD-10 explanation, falls back on ICD-10-CM until
#'   ICD-10 WHO copyright workaround is available
#' @export
explain_code.icd10 <- function(x,
                               short_code = guess_short(x),
                               condense = TRUE,
                               brief = FALSE,
                               warn = TRUE,
                               ...) {
  # don't pass on condense and warn until they are implemented
  explain_code.icd10cm(x = x, short_code = short_code, brief = brief, ...)
}

explain_code_worker <- function(x,
                                var_name,
                                var,
                                short_code = guess_short(x),
                                condense = TRUE,
                                brief = FALSE,
                                warn = TRUE,
                                lang = NULL,
                                ...) {
  stopifnot(is.atomic(x))
  stopifnot(missing(var_name) || missing(var))
  stopifnot(missing(var_name) || is.character(var_name))
  stopifnot(missing(var) || is.data.frame(var))
  stopifnot(is.logical(short_code), length(short_code) == 1L)
  stopifnot(is.logical(brief), length(brief) == 1L)
  if (!missing(condense)) {
    .NotYetUsed("condense", error = FALSE)
  }
  if (!missing(warn)) {
    .NotYetUsed("warn", error = FALSE)
  }
  if (!short_code) {
    x <- decimal_to_short.icd10(x)
  }
  # this is a alow linear lookup, but usually only
  # "explaining" one or a few codes at a time.
  short_str <- "short_desc"
  long_str <- "long_desc"
  if (!is.null(lang)) {
    short_str <- paste0(short_str, "_", lang)
    long_str <- paste0(long_str, "_", lang)
  }
  if (missing(var_name)) {
    i <- var
  } else {
  if (substring(var_name, 1, 4) != "get_")
    var_name <- paste0("get_", var_name)
    i <- .idget(var_name)()
  }
  i[
    i[["code"]] %in% unique(as_char_no_warn(x)),
    ifelse(brief, short_str, long_str)
  ]
}

icd9_expand_chapter_majors <- function(chap) {
  expand_range_major.icd9(
    icd.data::icd9_chapters[[chap]]["start"],
    icd.data::icd9_chapters[[chap]]["end"],
    defined = FALSE
  )
}

icd9_expand_sub_chapter_majors <- function(subchap) {
  expand_range_major.icd9(
    icd.data::icd9_sub_chapters[[subchap]]["start"],
    icd.data::icd9_sub_chapters[[subchap]]["end"],
    defined = FALSE
  )
}
