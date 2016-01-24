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

#' @title explain ICD9 codes
#' @description convert 'decimal' format (123.45 style) ICD9 codes into the name
#'   and description for human review there are official ICD9-CM data tables,
#'   not with conversion to decimal notation, but to the textual format.
#' @param x vector or other structure of ICD codes to explain in human language
#' @template short_code
#' @param condense single logical value which indicates whether to condense the
#'   given set of ICD-9 codes by replacing subsets of codes with 'parent' codes
#'   which exactly encompass certain subsets. E.g. If all cholera diagnoses are
#'   provided, only '001 - Cholera' needs to be displayed, not all subtypes.
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
#' library(magrittr) # nice to heave, but not essential
#' icd_explain(ahrqComorbid$CHF[1:3])
#' # same without condensing the list. In this case, 428.0 is shown:
#' icd_explain(ahrqComorbid$CHF[1:3] %>%  icd_condense)
#' icd_explain(ahrqComorbid$CHF[1:3], brief = TRUE)
#' @return data frame, or list of data frames, with fields for ICD9 code, name
#'   and description, derived from datamart lookup table
#' @seealso package comorbidities
#' @export
icd_explain <- function(...)
  UseMethod("icd_explain")

#' @describeIn icd_explain Explain ICD codes from a character vecotr, guessing ICD version
#' @export
icd_explain.character <- function(x, short_code = NULL, condense = TRUE, brief = FALSE, warn = TRUE, ...) {
  guess <- icd_guess_version.character(x, short_code = short_code)
  if (guess == "icd9") {
    if (is.null(short_code)) short_code <- icd_guess_short.icd9(x)
    icd_explain.icd9(x = x, short_code = short_code, condense = condense, brief = brief, warn = warn, ...)
  } else if (guess == "icd10") {
    if (is.null(short_code)) short_code <- icd_guess_short.icd10(x)
    stop("icd_explain.icd10 not implemented yet")
  } else {
    stop("Unable to guess ICD type")
  }
}

#' @describeIn icd_explain Explain all ICD-9 codes in a list of vectors
#' @export
icd_explain.list <- function(x,  short_code = icd_guess_short(x),
                             condense = TRUE, brief = FALSE, warn = TRUE, ...) {
  lapply(x, icd_explain, short_code = short_code,
         condense = condense, brief = brief, warn = warn)
}

#' @describeIn icd_explain explain Explain factor of ICD-9 codes
#' @export
icd_explain.factor <- function(icd9, short_code = icd_guess_short(icd9),
                               condense = TRUE, brief = FALSE, warn = TRUE, ...)
  icd_explain.character(asCharacterNoWarn(icd9), short_code = short_code,
                        condense = condense, brief = brief, warn = warn)

#' @describeIn icd_explain explain character vector of ICD-9 codes. TODO: actually, this is ICD-9-CM
#' @export
icd_explain.icd9 <- function(...)
  icd_explain.icd9cm(...)

#' @describeIn icd_explain explain character vector of ICD-9-CM codes
#' @export
icd_explain.icd9cm <- function(x, short_code = icd_guess_short.icd9(x),
                               condense = TRUE, brief = FALSE, warn = TRUE, ...) {
  assertCharacter(x)
  assertFlag(short_code)
  assertFlag(condense)
  assertFlag(brief)
  assertFlag(warn)
  if (!short_code) x <- icd_decimal_to_short.icd9(x)

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

  mjexplain <- names(icd9::icd9_majors)[icd9::icd9_majors %in% mj[mj %in% x]]
  # don't double count when major is also billable
  x <- x[x %nin% mj]
  descField <- ifelse(brief, "descShort", "descLong")
  c(mjexplain,
    icd9::icd9cm_hierarchy[ icd9::icd9cm_hierarchy[["icd9"]] %in% x, descField]
  )
}

#' @describeIn icd_explain explain numeric vector of ICD-9 codes, with warning.
#'   In general, this is not allowed because of the possible ambiguity of
#'   numeric decimal codes, but for convenience, this is allowed in this case to
#'   avoid typing many quotes.
#' @export
icd_explain.numeric <- function(icd9, short_code = icd_guess_short(icd9),
                                condense = TRUE, brief = FALSE, warn = FALSE, ...) {
  warnNumericCode()
  icd_explain.character(as.character(icd9), short_code = short_code, condense = condense, brief = brief, warn = warn)
}

#' @title get ICD-9 Chapters from vector of ICD-9 codes
#' @description This runs quite slowly. Used too rarely to be worth optimizing
#'   now. This is used to build a master list of ICD-9 codes with their
#'   respective chapters, sub-chapters, etc.. The querying of the web page to
#'   get these is already done, and the results saved in the lists
#'   \code{icd9Chapters} etc which define ranges.
#' @param x vector of ICD-9 codes
#' @template short_code
#' @template verbose
#' @keywords internal
icd9_get_chapters <- function(x, short_code = icd_guess_short.icd9(x),
                              verbose = FALSE) {
  # set up comorbidity maps for chapters/sub/major group, then loop through each
  # ICD-9 code, loop through each comorbidity and lookup code in the map for
  # that field, then add the factor level for the match. There should be 100%
  # matches.
  assertFlag(short_code)
  #assert(checkFactor(x), checkCharacter(x), checkClass(x, c("icd9", "icd9cm")))
  icd9 <- asCharacterNoWarn(x)
  majors <- icd_get_major.icd9(icd9, short_code)

  # could consider factor_nosort, but this isn't the main bottleneck
  cf <- factor(rep(NA_character_, length(icd9)),
               levels = c(names(icd9::icd9_chapters), NA_character_))
  sf <- factor(rep(NA_character_, length(icd9)),
               levels = c(names(icd9::icd9_sub_chapters), NA_character_))
  mf <- factor(rep(NA_character_, length(icd9)),
               levels = c(names(icd9::icd9_majors), NA_character_))
  thrdgt <- factor(rep(NA_character_, length(icd9)), levels = c(icd9::icd9_majors, NA_character_))
  out <- data.frame(threedigit = thrdgt, major = mf,
                    subchapter = sf, chapter = cf)

  chap_lookup <- lapply(icd9::icd9_chapters, function(y)
    icd_expand_range_major.icd9(y[["start"]], y[["end"]], defined = FALSE))

  subchap_lookup <- lapply(icd9::icd9_sub_chapters, function(y)
    icd_expand_range_major.icd9(y[["start"]], y[["end"]], defined = FALSE))

  for (i in 1:length(majors)) {
    if (verbose)
      message("icd9_get_chapters: working on major ", majors[i], ", row ", i)
    for (chap_num in 1:length(icd9::icd9_chapters)) {
      if (majors[i] %fin% chap_lookup[[chap_num]]) {
        out[i, "chapter"] <- names(icd9::icd9_chapters)[chap_num]
        break
      }
    }
    for (subchap_num in 1:length(icd9::icd9_sub_chapters)) {
      if (majors[i] %fin% subchap_lookup[[subchap_num]]) {
        out[i, "subchapter"] <- names(icd9::icd9_sub_chapters)[subchap_num]
        break
      }
    }
  }
  whch <- match(majors, icd9::icd9_majors, nomatch = NA_character_)
  out$major[] <- names(icd9::icd9_majors)[whch]
  out$threedigit[] <- unlist(icd9::icd9_majors)[whch]
  out$threedigit <- out$threedigit %>% icd9cm

  # many possible three digit codes don't exist. We should return NA for the
  # whole row. Chapter is coded as a range, so picks up these non-existent codes
  out$chapter[is.na(out$major)] <- NA_character_

  out
}

icd9_expand_chapter_majors <- function(chap) {
  icd_expand_range_major.icd9(
    icd9::icd9_chapters[[chap]]["start"],
    icd9::icd9_chapters[[chap]]["end"],
    defined = FALSE)
}

icd9_expand_subchapter_majors <- function(subchap) {
  icd_expand_range_major.icd9(
    icd9::icd9_sub_chapters[[subchap]]["start"],
    icd9::icd9_sub_chapters[[subchap]]["end"],
    defined = FALSE)
}
