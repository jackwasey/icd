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
#'   the short description from the canonical CMS descriptions (included in
#'   \code{data-raw}) will be used, otherwise the long description is used.
#' @param warn single logical value, default is \code{TRUE}, meaning that codes
#'   which do not correspond to diagnoses, or to three-digit codes, will trigger
#'   a warning.
#' @template dotdotdot
#' @examples
#' # by default, just show parent code and ignore children (428.0 not shown
#' # because 428 is present):
#' icd_explain(ahrqComorbid$CHF[1:3])
#' # same without condensing the list. In this case, 428.0 is shown:
#' icd_explain(icd9_map_ahrq$CHF[1:3], brief = TRUE)
#' # The first three in the ICD-10 equivalent are a little different:
#' icd_explain(icd10_map_ahrq$CHF[1:3], brief = TRUE)
#' # nice to have magrittr, but not essential
#' library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
#' icd_explain(icd9_map_ahrq$CHF[1:3] %>% icd_condense)
#' @return data frame, or list of data frames, with fields for ICD-9 code, name
#'   and description
#' @export
icd_explain <- function(...)
  UseMethod("icd_explain")

#' @describeIn icd_explain Explain ICD codes from a character vector, guessing ICD version
#' @export
#' @keywords internal
icd_explain.default <- function(x, short_code = icd_guess_short(x), condense = TRUE, brief = FALSE, warn = TRUE, ...) {
  switch(
    icd_guess_version.character(as_char_no_warn(x), short_code = short_code),
    "icd9" = icd_explain.icd9(x, short_code = short_code, condense = condense, brief = brief, warn = warn, ...),
    "icd10" = icd_explain.icd10(x, short_code = short_code, brief = brief, ...),
    stop("Unknown ICD version.")
  )
}

#' @describeIn icd_explain Explain all ICD-9 codes in a list of vectors
#' @export
#' @keywords internal
icd_explain.list <- function(x, ...) {
  lapply(x, icd_explain, ...)
}

#' @describeIn icd_explain explain character vector of ICD-9 codes.
#' @export
#' @keywords internal
icd_explain.icd9 <- function(...) {
  icd_explain.icd9cm(...)
}

#' @describeIn icd_explain explain character vector of ICD-9-CM codes
#' @export
#' @keywords internal
icd_explain.icd9cm <- function(x, short_code = icd_guess_short(x),
                               condense = TRUE, brief = FALSE, warn = TRUE, ...) {
  assert(checkmate::checkCharacter(x), checkmate::checkFactor(x))
  assert_flag(short_code)
  assert_flag(condense)
  assert_flag(brief)
  assert_flag(warn)
  if (!short_code)
    x <- icd_decimal_to_short.icd9(x)

  # if there are only defined codes, we should condense with this in mind:
  if (condense) {
    if (warn && !all(icd_is_defined.icd9(x, short_code = TRUE))) {
      undefined <- x[!icd_is_defined.icd9(x, short_code = TRUE)]
      warning("Some ICD codes are not 'defined' when trying to condense when explaining codes.
              Will drop these and continue. E.g. ",
              paste(undefined[seq(from = 1, to = min(5, length(undefined)))],
                    collapse = " "), call. = FALSE)
    }
    x <- icd_condense.icd9(icd_get_defined.icd9(x, short_code = TRUE), defined = TRUE, short_code = TRUE)
  }
  mj <- unique(icd_get_major.icd9(x, short_code = TRUE))

  mjexplain <- names(icd::icd9_majors)[icd::icd9_majors %in% mj[mj %in% x]]
  # don't double count when major is also billable
  x <- x[x %nin% mj]
  desc_field <- ifelse(brief, "short_desc", "long_desc")
  c(mjexplain,
    icd::icd9cm_hierarchy[icd::icd9cm_hierarchy[["code"]] %in% x, desc_field]
  )
}

#' @describeIn icd_explain ICD-10 explanation, current a minimal implementation
#' @export
#' @keywords internal
icd_explain.icd10 <- function(x, short_code = icd_guess_short(x),
                              condense = TRUE, brief = FALSE, warn = TRUE, ...) {
  assert_vector(x)
  assert_flag(short_code)
  assert_flag(brief)

  if (!missing(condense))
    .NotYetUsed("condense", error = FALSE)
  if (!missing(warn))
    .NotYetUsed("warn", error = FALSE)

  if (!short_code)
    x <- icd_decimal_to_short.icd10(x)

  icd::icd10cm2016[icd::icd10cm2016[["code"]] %in% unique(as_char_no_warn(x)),
                   ifelse(brief, "short_desc", "long_desc")]
}

#' get ICD-9 Chapters from vector of ICD-9 codes
#'
#' This runs quite slowly. Used too rarely to be worth optimizing
#'   now. This is used to build a master list of ICD-9 codes with their
#'   respective chapters, sub-chapters, etc.. The querying of the web page to
#'   get these is already done, and the results saved in the lists
#'   \code{icd9Chapters} etc which define ranges.
#' @param x vector of ICD-9 codes
#' @template short_code
#' @template verbose
#' @keywords internal
icd9_get_chapters <- function(x, short_code = icd_guess_short(x),
                              verbose = FALSE) {
  # set up comorbidity maps for chapters/sub/major group, then loop through each
  # ICD-9 code, loop through each comorbidity and lookup code in the map for
  # that field, then add the factor level for the match. There should be 100%
  # matches.
  assert(checkmate::checkCharacter(x), checkmate::checkFactor(x))
  assert_flag(short_code)
  icd9 <- as_char_no_warn(x)
  majors <- icd_get_major.icd9(icd9, short_code)

  # could consider factor_nosort, but this isn't the main bottleneck
  cf <- factor(rep(NA_character_, length(icd9)),
               levels = c(names(icd::icd9_chapters), NA_character_))
  sf <- factor(rep(NA_character_, length(icd9)),
               levels = c(names(icd::icd9_sub_chapters), NA_character_))
  mf <- factor(rep(NA_character_, length(icd9)),
               levels = c(names(icd::icd9_majors), NA_character_))
  thrdgt <- factor(rep(NA_character_, length(icd9)), levels = c(icd::icd9_majors, NA_character_))
  out <- data.frame(three_digit = thrdgt, major = mf,
                    sub_chapter = sf, chapter = cf)

  chap_lookup <- lapply(icd::icd9_chapters, function(y)
    icd_expand_range_major.icd9(y[["start"]], y[["end"]], defined = FALSE))

  subchap_lookup <- lapply(icd::icd9_sub_chapters, function(y)
    icd_expand_range_major.icd9(y[["start"]], y[["end"]], defined = FALSE))

  for (i in 1:length(majors)) {
    if (verbose)
      message("icd9_get_chapters: working on major ", majors[i], ", row ", i)
    for (chap_num in 1:length(icd::icd9_chapters)) {
      if (majors[i] %fin% chap_lookup[[chap_num]]) {
        out[i, "chapter"] <- names(icd::icd9_chapters)[chap_num]
        break
      }
    }
    for (subchap_num in 1:length(icd::icd9_sub_chapters)) {
      if (majors[i] %fin% subchap_lookup[[subchap_num]]) {
        out[i, "sub_chapter"] <- names(icd::icd9_sub_chapters)[subchap_num]
        break
      }
    }
  }
  whch <- match(majors, icd::icd9_majors, nomatch = NA_character_)
  out$major[] <- names(icd::icd9_majors)[whch]
  out$three_digit[] <- unlist(icd::icd9_majors)[whch]
  out$three_digit <- out$three_digit %>% icd9cm

  # many possible three digit codes don't exist. We should return NA for the
  # whole row. Chapter is coded as a range, so picks up these non-existent codes
  out$chapter[is.na(out$major)] <- NA_character_

  out
}

icd9_expand_chapter_majors <- function(chap) {
  icd_expand_range_major.icd9(
    icd::icd9_chapters[[chap]]["start"],
    icd::icd9_chapters[[chap]]["end"],
    defined = FALSE)
}

icd9_expand_sub_chapter_majors <- function(subchap) {
  icd_expand_range_major.icd9(
    icd::icd9_sub_chapters[[subchap]]["start"],
    icd::icd9_sub_chapters[[subchap]]["end"],
    defined = FALSE)
}
