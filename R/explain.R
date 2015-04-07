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
#' @template toParent
#' @examples
#' icd9ExplainShort(ahrqComorbid[[1]][1:3])
#' icd9Explain(ahrqComorbid[[1]][1:3], brief = TRUE)
#' @return data frame, or list of data frames, with fields for ICD9 code, name
#'   and description, derived from datamart lookup table
#' @seealso package comorbidities
#' @references \url{http://www.stata.com/help.cgi?icd9}
#' @export
icd9Explain <- function(icd9, isShort = icd9GuessIsShort(icd9),
                        doCondense = TRUE, brief = FALSE, warn = TRUE,
                        toParent = TRUE) {
  UseMethod("icd9Explain")
}

#' @rdname icd9Explain
#' @export
icd9ExplainShort <- function(icd9Short, doCondense = TRUE, brief = FALSE, warn = TRUE,
                             toParent = TRUE) {
  icd9Explain(icd9Short, isShort = TRUE,
              doCondense = doCondense, brief = brief, warn = warn, toParent = toParent)
}

#' @rdname icd9Explain
#' @export
icd9ExplainDecimal <- function(icd9Decimal, doCondense = TRUE, brief = FALSE, warn = TRUE,
                               toParent = TRUE) {
  icd9Explain(icd9Decimal, isShort = FALSE,
              doCondense = doCondense, brief = brief, warn = warn, toParent = toParent)
}

#' @describeIn icd9Explain explain all ICD-9 codes in a list of vectors
#' @export
icd9Explain.list <- function(icd9,  isShort = icd9GuessIsShort(icd9),
                             doCondense = TRUE, brief = FALSE, warn = TRUE,
                             toParent = TRUE) {
  lapply(icd9, icd9Explain, isShort = isShort,
         doCondense = doCondense, brief = brief, warn = warn,
         toParent = toParent)
}

#' @describeIn icd9Explain explain factor of ICD-9 codes
#' @export
icd9Explain.factor <- function(icd9, isShort = icd9GuessIsShort(icd9),
                               doCondense = TRUE, brief = FALSE, warn = TRUE,
                               toParent = TRUE)
  icd9Explain.character(asCharacterNoWarn(icd9), isShort = isShort,
                        doCondense = doCondense, brief = brief, warn = warn, toParent = toParent)

#' @describeIn icd9Explain explain character vector of ICD-9 codes
#' @export
icd9Explain.character <- function(icd9, isShort = icd9GuessIsShort(icd9),
                                  doCondense = TRUE, brief = FALSE, warn = TRUE,
                                  toParent = TRUE) {
  checkmate::assertCharacter(icd9)
  checkmate::assertFlag(isShort)
  checkmate::assertFlag(doCondense)
  checkmate::assertFlag(brief)
  checkmate::assertFlag(warn)
  if (!isShort) icd9 <- icd9DecimalToShort(icd9)

  # if there are only real codes, we should condense with this in mind:
  if (doCondense) {
    onlyReal <- all(icd9IsRealShort(icd9))
    if (warn && !onlyReal) {
      unreal <- icd9[!icd9IsRealShort(icd9)]
      warning("Some ICD codes are not 'real', e.g. ",
              paste(unreal[seq(from = 1, to = min(5, length(unreal)))],
                    collapse = " "),
              call. = FALSE)
    }

    icd9 <- icd9GetRealShort(icd9, onlyBillable = !toParent)
    icd9 <- icd9CondenseShort(icd9, onlyReal = TRUE)
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
                                doCondense = TRUE, brief = FALSE, warn = FALSE,
                                toParent = TRUE) {
  warnNumericCode()
  icd9Explain.character(as.character(icd9), isShort = isShort,
                        doCondense = doCondense, brief = brief, warn = warn, toParent = toParent)
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
  vs <- icd9IsValidShort(icd9)
  vd <- icd9IsValidDecimal(icd9)
  if (sum(vd) > sum(vs)) return(FALSE)
  TRUE
}

#' @title get ICD-9 Chapters from vector of ICD-9 codes
#' @description This runs quite slowly. Used too rarely to be worth optimizing
#'   now. This is used to build a master list of ICD-9 codes with their respective chapters, sub-chapters, etc.. The querying of the web page to get these is already done, and the results saved in the lists \code{icd9Chapters} etc which define ranges.
#' @param icd9-any
#' @param isShort
#' @param invalid
#' @keywords internal
icd9GetChapters <- function(icd9, isShort = icd9GuessIsShort(icd9)) {
  # set up comorbidity maps for chapters/sub/major group, then loop through each
  # ICD-9 code, loop through each comorbidity and lookup code in the map for
  # that field, then add the factor level for the match. There should be 100%
  # matches.
  checkmate::assertFlag(isShort)
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
  thrdgt <- factor(rep(NA, length(icd9)),levels = c(allmjrs, NA_character_))
  out <- data.frame(threedigit = thrdgt, major = mf,
                    subchapter = sf, chapter = cf)
  for (i in 1:length(majors)) {
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

#' @title Condense ICD-9 code by replacing complete families with parent codes
#' @description This can be thought of as the inverse operation to
#'   \code{icd9Children}.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @template onlyBillable
#' @family ICD-9 ranges
#' @export
icd9Condense <- function(icd9, isShort, onlyReal = NULL, toParent = TRUE) {
  checkmate::assertFlag(isShort)
  checkmate::assertFlag(toParent)
  if (isShort) return(icd9CondenseShort(icd9, onlyReal, toParent))
  icd9CondenseDecimal(icd9, onlyReal, toParent)
}

#' @rdname icd9Condense
#' @export
icd9Condense <- function(icd9, isShort = icd9GuessIsShort(icd9),
                         onlyReal = NULL) {
  if (isShort) return(icd9CondenseShort(icd9, onlyReal))
  icd9CondenseDecimal(icd9, onlyReal)
}

#' @rdname icd9Condense
#' @export
icd9CondenseDecimal <- function(icd9Decimal, onlyReal = NULL)
  icd9CondenseShort(icd9DecimalToShort(icd9Decimal), onlyReal)

#' @rdname icd9Condense
#' @template warnReal
#' @export
icd9CondenseShort <- function(icd9Short, onlyReal = NULL,
                              onlyBillable = all(icd9IsBillableShort(icd9Short)),
                              warnReal = FALSE) {
  # there is no such thing as 'condensing to billable codes' since they are all
  # leaves.
  assertFactorOrCharacter(icd9Short)
  checkmate::assertFlag(warnReal)
  icd9Short <- asCharacterNoWarn(icd9Short) # TODO: still necessary?

  i9w <- sort(unique(icd9Short)) # TODO sorting may not be helpful

  if (is.null(onlyReal)) {
    if (all(icd9IsRealShort(i9w))) {
      onlyReal <- TRUE
      if (warnReal) warning("onlyReal not given, but all codes are 'real' so assuming TRUE")
    } else {
      onlyReal <- FALSE
      if (warnReal) warning("onlyReal not given, but not all codes are 'real' so assuming FALSE")
    }
  } else
    checkmate::assertFlag(onlyReal)

  if (!onlyReal)
    onlyBillable <- FALSE

  if (warnReal && onlyReal && !all(icd9IsRealShort(icd9Short)))
    warning("only real values requested, but some undefined ('non-real') ICD-9 code(s) given.")

  # i9w <- "65381" %i9s% "65510"; onlyReal = TRUE; i9w[i9w == "654"] <- "657"

  # and try icd9CondenseToParentShort(c("10081", "10089", "1000", "1009"))

  # any major codes are automatically in output (not condensing higher than
  # three digit code) and all their children can be removed from the work list
  out <- majors <- i9w[areMajor <- icd9IsMajor(i9w)]
  i9w <- i9w[!areMajor]
  i9w <- i9w[i9w %nin% icd9Children(majors, onlyReal = onlyReal)]
  # now four digit codes trump any (possible) children, so take care of them:

  # for each parent major in tests data, are the number of distinct four digit
  # children the same as the number of possible (real or not) children? Don't
  # need to compare them all, just count. actually, start with bigger group,
  # then we can eliminate more quickly
  icd9GetMajor(i9w, isShort = TRUE) %>% unique -> majorParents
  for (mp in majorParents) {
    icd9GetMajor(i9w, isShort = TRUE) -> mjrs
    major_match <- mjrs == mp
    test_kids <- icd9ChildrenShort(mp, onlyReal = onlyReal, onlyBillable = onlyBillable)
    if ((length(test_kids) - !onlyBillable) == sum(major_match)) {
      out <- c(out, mp)
      i9w <- i9w[-which(major_match)]
    }
  }

  # now same for four digit codes, thinking carefully about V and E codes
  # the remaining codes are 4 or 5 chars. They have no common parents.
  substr(i9w, 0, 4) %>% unique -> fourth_parents
  for (fp in fourth_parents) {
    substr(i9w, 0, 4) -> fourth_level
    fourth_match <- fourth_level == fp
    test_kids <- icd9ChildrenShort(fp, onlyReal = onlyReal, onlyBillable = onlyBillable)
    # assumption is that no four digit code is billable AND a leaf node
    if (length(test_kids) == sum(fourth_match)) {
      # we matched (don't count the four-char itself)
      out <- c(out, fp)
      i9w <- i9w[-which(fourth_match)]
    }
  }
  #  if (onlyReal) return(icd9GetRealShort(c(i9o, i9w), majorOk = TRUE))
  out <- unique(icd9SortShort(c(out, i9w)))
  if (onlyReal) return(icd9GetRealShort(out, onlyBillable = onlyBillable))
  out
}
