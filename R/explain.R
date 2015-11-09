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
#' @description convert 'decimal' format (123.45 style) ICD9 codes into the name and
#'   description for human review there are official ICD9-CM data tables, not
#'   with conversion to decimal notation, but to the textual format.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @param doCondense single logical value which indicates whether to condense
#'   the given set of ICD-9 codes by replacing subsets of codes with 'parent'
#'   codes which exactly encompass certain subsets. E.g. If all cholera
#'   diagnoses are provided, only '001 - Cholera' needs to be displayed, not all
#'   subtypes.
#' @param brief single logical value, default is \code{FALSE}. If \code{TRUE},
#'   the short description from the canonical CMS descriptions (included in
#'   \code{extdata}) will be used, otherwise the long description is used.
#' @param warn single logical value, default is \code{TRUE}, meaning that codes
#'   which do not correspond to diagnoses, or to three-digit codes, will trigger
#'   a warning.
#' @examples
#' icd9ExplainShort(ahrqComorbid[[1]][1:3])
#' icd9Explain(ahrqComorbid[[1]][1:3], brief = TRUE)
#' @return data frame, or list of data frames, with fields for ICD9 code, name
#'   and description, derived from datamart lookup table
#' @seealso package comorbidities
#' @export
icd9Explain <- function(icd9, isShort = icd9GuessIsShort(icd9), doCondense = TRUE, brief = FALSE, warn = TRUE) {
  UseMethod("icd9Explain")
}

#' @rdname icd9Explain
#' @export
icd9ExplainShort <- function(icd9Short, doCondense = TRUE, brief = FALSE, warn = TRUE) {
  icd9Explain(icd9Short, isShort = TRUE, doCondense = doCondense, brief = brief, warn = warn)
}

#' @rdname icd9Explain
#' @export
icd9ExplainDecimal <- function(icd9Decimal, doCondense = TRUE, brief = FALSE, warn = TRUE) {
  icd9Explain(icd9Decimal, isShort = FALSE, doCondense = doCondense, brief = brief, warn = warn)
}

#' @describeIn icd9Explain explain all ICD-9 codes in a list of vectors
#' @export
icd9Explain.list <- function(icd9,  isShort = icd9GuessIsShort(icd9), doCondense = TRUE, brief = FALSE, warn = TRUE) {
  lapply(icd9, icd9Explain, isShort = isShort, doCondense = doCondense, brief = brief, warn = warn)
}

#' @describeIn icd9Explain explain factor of ICD-9 codes
#' @export
icd9Explain.factor <- function(icd9, isShort = icd9GuessIsShort(icd9), doCondense = TRUE, brief = FALSE, warn = TRUE)
  icd9Explain.character(asCharacterNoWarn(icd9), isShort = isShort, doCondense = doCondense, brief = brief, warn = warn)

#' @describeIn icd9Explain explain character vector of ICD-9 codes
#' @export
icd9Explain.character <- function(icd9, isShort = icd9GuessIsShort(icd9),
                                  doCondense = TRUE, brief = FALSE, warn = TRUE) {
  assertCharacter(icd9)
  assertFlag(isShort)
  assertFlag(doCondense)
  assertFlag(brief)
  assertFlag(warn)
  if (!isShort) icd9 <- icd9DecimalToShort(icd9)

  # if there are only real codes, we should condense with this in mind:
  if (doCondense) {
    onlyReal <- all(icd9IsRealShort(icd9))
    if (warn && !onlyReal) {
      unreal <- icd9[!icd9IsRealShort(icd9)]
      warning("Some ICD codes are not 'real' when trying to condense when explaining codes.
              Will drop these and continue. E.g. ",
              paste(unreal[seq(from = 1, to = min(5, length(unreal)))],
                    collapse = " "), call. = FALSE)
    }
    icd9 <- icd9CondenseShort(
      icd9GetRealShort(icd9),
      onlyReal = TRUE)
  }
  mj <- unique(icd9GetMajor(icd9, isShort = TRUE))

  mjexplain <- names(icd9::icd9ChaptersMajor)[icd9::icd9ChaptersMajor %in%
                                                mj[mj %in% icd9]]
  # don't double count when major is also billable
  icd9 <- icd9[icd9 %nin% mj]
  descField <- ifelse(brief, "descShort", "descLong")
  c(mjexplain,
    icd9::icd9Hierarchy[ icd9::icd9Hierarchy[["icd9"]] %in% icd9, descField]
  )
}

#' @describeIn icd9Explain explain numeric vector of ICD-9 codes, with warning.
#'   In general, this is not allowed because of the possible ambiguity of
#'   numeric decimal codes, but for convenience, this is allowed in this case to
#'   avoid typing many quotes.
#' @export
icd9Explain.numeric <- function(icd9, isShort = icd9GuessIsShort(icd9),
                                doCondense = TRUE, brief = FALSE, warn = FALSE) {
  warnNumericCode()
  icd9Explain.character(as.character(icd9), isShort = isShort, doCondense = doCondense, brief = brief, warn = warn)
}

#' @title guess whether short or long
#' @description partly implemented. Goal is to guess whether codes are short or
#'   decimal form, then to call icd9Explain with the condense argument.
#'   Currently condense works, but not with the icd9 lookup table currently in
#'   use. Not exporting this function until it works as intended. Of note,
#'   validation is a bit different here, since we don't know the type until
#'   after we guess. We could look for where both short and long are invalid,
#'   and otherwise assume valid, even if the bulk are short. However, it may be
#'   more useful to check validity after the guess.
#' @return single logical value, \code{TRUE} if input data are predominantly
#'   short type. If there is some uncertainty, then return NA.
#' @keywords internal
icd9GuessIsShort <- function(icd9) {
  # don't take responsibility for validation of codes:
  if (is.list(icd9)) icd9 <- unlist(icd9)
  icd9 <- asCharacterNoWarn(icd9)
  if (is.list(icd9)) {
    if (length(icd9) > 0)
      icd9 <- icd9[[1]]
    else
      return(TRUE)
  }
  testend <- length(icd9)
  if (testend > 100) testend <- 100
  vs <- icd9IsValidShort(icd9[1:testend])
  vd <- icd9IsValidDecimal(icd9[1:testend])
  if (sum(vd) > sum(vs)) return(FALSE)
  TRUE
}

#' @title get ICD-9 Chapters from vector of ICD-9 codes
#' @description This runs quite slowly. Used too rarely to be worth optimizing
#'   now. This is used to build a master list of ICD-9 codes with their
#'   respective chapters, sub-chapters, etc.. The querying of the web page to
#'   get these is already done, and the results saved in the lists
#'   \code{icd9Chapters} etc which define ranges.
#' @template icd9-any
#' @template isShort
#' @template verbose
#' @keywords internal
icd9GetChapters <- function(icd9, isShort = icd9GuessIsShort(icd9), verbose = FALSE) {
  # set up comorbidity maps for chapters/sub/major group, then loop through each
  # ICD-9 code, loop through each comorbidity and lookup code in the map for
  # that field, then add the factor level for the match. There should be 100%
  # matches.
  assertFlag(isShort)
  assertFactorOrCharacter(icd9)
  icd9 <- asCharacterNoWarn(icd9)
  majors <- icd9GetMajor(icd9, isShort)

  cf <- factor(rep(NA, length(icd9)),
               levels = c(names(icd9::icd9Chapters), NA_character_))
  sf <- factor(rep(NA, length(icd9)),
               levels = c(names(icd9::icd9ChaptersSub), NA_character_))
  mf <- factor(rep(NA, length(icd9)),
               levels = c(names(icd9::icd9ChaptersMajor), NA_character_))
  allmjrs <- lapply(icd9::icd9ChaptersMajor, `[[`, "major")
  thrdgt <- factor(rep(NA, length(icd9)), levels = c(allmjrs, NA_character_))
  out <- data.frame(threedigit = thrdgt, major = mf,
                    subchapter = sf, chapter = cf)
  for (i in 1:length(majors)) {
    if (verbose) message("working on major: ", majors[i])
    for (chap in names(icd9::icd9Chapters)) {
      if (any(majors[i] %in%
              (icd9::icd9Chapters[[chap]]["start"] %i9mj%
               icd9::icd9Chapters[[chap]]["end"])
      )) {
        out[i, "chapter"] <- chap
        break
      }
    }
    for (subchap in names(icd9::icd9ChaptersSub)) {
      if (any(majors[i] %in%
              (icd9::icd9ChaptersSub[[subchap]]["start"] %i9mj%
               icd9::icd9ChaptersSub[[subchap]]["end"])
      )) {
        out[i, "subchapter"] <- subchap
        break
      }
    }
  }
  whch <- match(majors, allmjrs, nomatch = NA_character_)
  out$major[] <- names(allmjrs)[whch]
  out$threedigit[] <- unlist(allmjrs)[whch]

  # many possible three digit codes don't exist. We should return NA for the
  # whole row. Chapter is coded as a range, so picks up these non-existent codes
  out$chapter[is.na(out$major)] <- NA_character_

  out
}
