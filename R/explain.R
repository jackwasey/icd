#' @title explain ICD9 codes
#' @description convert full format (123.45 style) ICD9 codes into the name and
#'   description for human review there are official ICD9-CM data tables, not
#'   with conversion to decimal notation, but to the textual format.
#' @note TODO: it will be useful to have S3 ICD-9 short and long classes for
#'   situations like this where we could easily dispatch on short or long type,
#'   or even use a print.icd9decimal or print.icd9Short S3 method to display
#'   ICD-9 codes.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @param doCondense single logical value which indicates whether to condense
#'   the given set of ICD-9 codes by replacing subsets of codes with 'parent'
#'   codes which exactly encompass certain subsets. E.g. If all cholera
#'   diagnoses are provided, only '001 - Cholera' needs to be displayed, not all
#'   subtypes. This is currently partially implemented. See issue #3 in github.
#' @examples
#' icd9ExplainShort(ahrqComorbid[[1]][1:3])
#' @return data frame, or list of data frames, with fields for ICD9 code, name
#'   and description, derived from datamart lookup table
#' @seealso package comorbidities
#' @references \url{http://www.stata.com/help.cgi?icd9}
#' @export
icd9Explain <- function(icd9, isShort, doCondense = TRUE) {
  UseMethod("icd9Explain")
}

#' @rdname icd9Explain
#' @export
icd9ExplainShort <- function(icd9Short, doCondense = TRUE) {
  icd9Explain(icd9Short, isShort = TRUE, doCondense = doCondense)
}

#' @rdname icd9Explain
#' @export
icd9ExplainDecimal <- function(icd9Decimal, doCondense = TRUE) {
  icd9Explain(icd9Decimal, isShort = FALSE, doCondense = doCondense)
}

#' @describeIn icd9Explain explain alll ICD-9 codes in a list of vectors
#' @export
icd9Explain.list <- function(icd9, isShort, doCondense = TRUE) {
  lapply(icd9, icd9Explain, isShort = isShort, doCondense = doCondense)
}

#' @describeIn icd9Explain explain character vector of ICD-9 codes
#' @export
icd9Explain.character <- function(icd9, isShort, doCondense = TRUE) {

  if (!isShort) {
    # make sure there are preceding zeroes, in order to match the icd9Hierarchy
    # data.
    icd9 <- icd9AddLeadingZeroesDecimal(icd9)
    icd9 <- icd9DecimalToShort(icd9)
  }
  if (doCondense) {
    return(icd9CondenseToExplain(icd9))
  }
  mj <- unique(icd9ShortToMajor(icd9))
  c(names(icd9ChaptersMajor)[icd9ChaptersMajor %in% mj[mj %in% icd9]],
    icd9Hierarchy[ icd9Hierarchy[["icd9"]] %in% icd9, "descLong"]
  )
}

#' @describeIn icd9Explain explain numeric vector of ICD-9 codes, with warning
#' @export
icd9Explain.numeric <- function(icd9, isShort, doCondense = TRUE) {
  warning("Numeric ICD-9 codes are unable to accurately represent actual ICD-9
           codes. Converting to character, but beware of inevitable errors.")
  icd9Explain.character(as.character(icd9), isShort = isShort)
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
icd9GuessIsShort <- function(icd9, invalidAction = icd9InvalidActions) {
  invalidAction <- match.arg(invalidAction)
  icd9 <- as.character(icd9)
  if (is.list(icd9)) {
    testCodes <- icd9[[1]]
  } else {
    testCodes <- icd9
  }
  vs <- icd9ValidShort(testCodes)
  vd <- icd9ValidDecimal(testCodes)
  vsm <- mean(vs)
  vdm <- mean(vd)
  if (vsm - vdm > 0.5) {
    # this function returns, but we don't care what it returns.
    icd9ValidNaWarnStopShort(icd9Short = testCodes,
                             invalidAction = invalidAction)
    return(TRUE)
  }
  if (vdm - vsm > 0.5) {
    icd9ValidNaWarnStopDecimal(icd9Decimal = testCodes,
                               invalidAction = invalidAction)
    return(FALSE)
  }
  # now we really can't do much validation, but if requested, we can see if any
  # are neither valid short nor valid decimal
  if (any(!vs & !vd)) {
    icd9WarnStopMessage(
      "icd9GuessIsShort: predominant type not determined, and some codes were
        neither valid short nor decimal formats:", paste(testCodes[!vs | !vd]),
      invalidAction = invalidAction
    )
  }
  NA
}

#' @title get ICD-9 Chapters from vector of ICD-9 codes
#' @description work-in-progress
#' @param icd9-any
#' @param isShort
#' @param invalid
#' @keywords internal
icd9GetChapters <- function(icd9, isShort,
                            invalidAction = icd9InvalidActions) {
  invalidAction <- match.arg(invalidAction)

  # set up comorbidity maps for chapters/sub/major group, then loop through each
  # ICD-9 code, loop through each comorbidity and lookup code in the map for
  # that field, then add the factor level for the match. There should be 100%
  # matches.

  majors     <- icd9GetMajor(icd9, isShort, invalidAction)

  cf <- factor(rep(NA, length(icd9)),
               levels = c(names(icd9Chapters), NA_character_))
  sf <- factor(rep(NA, length(icd9)),
               levels = c(names(icd9ChaptersSub), NA_character_))
  mf <- factor(rep(NA, length(icd9)),
               levels = c(names(icd9ChaptersMajor), NA_character_))
  out <- data.frame(chapter = cf, subchapter = sf, major = mf)
  for (i in 1:length(majors)) {
    for (chap in names(icd9Chapters)) {
      if (any(majors[i] %in%
                (icd9Chapters[[chap]]["start"] %i9mj%
                   icd9Chapters[[chap]]["end"])
      )) {
        out[i, "chapter"] <- chap
        break
      }
    }
    for (subchap in names(icd9ChaptersSub)) {
      if (any(majors[i] %in%
                (icd9ChaptersSub[[subchap]]["start"] %i9mj%
                   icd9ChaptersSub[[subchap]]["end"])
      )) {
        out[i, "subchapter"] <- subchap
        break
      }
    }
    for (mj in names(icd9ChaptersMajor)) {
      if (majors[i] == icd9ChaptersMajor[[mj]]) {
        out[i, "major"] <- mj
        break
      }
    }
  }
  out
}

# internal. Assumes working in project root, so saving to directory 'data'
icd9GetChaptersHierarchy <- function(save = FALSE, path = "data") {

  # don't rely on having already done this when setting up other data.
  icd9CmDesc <- parseIcd9Descriptions()

  icd9Hierarchy <- cbind(
    icd9CmDesc,
    icd9GetChapters(icd9 = icd9CmDesc[["icd9"]], isShort = TRUE,
                    invalidAction = "stop")
  )
  if (save) saveSourceTreeData("icd9Hierarchy", path = path)
}

#' @title condense list of short ICD-9 code into minimal set of parent descriptions
#' @description This can be thought of as the inverse operation to expanding a
#'   range. The list given must already contain the parents, because this
#'   function will never add a parent ICD-9 which, although may have all
#'   children present, may itself have an additional clinical meaning. In
#'   addition, in contrast to \code{icd9CondenseToMajor}, this function only walks
#'   back up to parents which have descriptions in \code{icd9Hierarchy}, so it is
#'   useful for generating a minimal textual description of a set of ICD-9
#'   codes.
#' @template icd9-short
#' @template invalid
#' @family ICD-9 ranges
#' @export
#' @keywords manip
icd9CondenseToExplain <- function(icd9Short,
                                  invalidAction = c("stop", "ignore",
                                                    "silent", "warn")) {

  invalidAction <- match.arg(invalidAction)
  icd9Short <- icd9ValidNaWarnStopShort(icd9Short,
                                        invalidAction = invalidAction)

  # we also rely on the icd9 codes existing in the reference table:
  if (invalidAction == "warn" && any(!icd9RealShort(icd9Short)))
    warning("dropping values which are not in the reference table",
            paste(icd9Short[!icd9RealShort(icd9Short)],
                  sep = ", ", collapse = ", "))

  # make homogeneous and sort so we will hit the parents first, kids later.
  icd9Short <- sort(icd9AddLeadingZeroesShort(icd9Short))
  # set up a factor which contains levels for any description within the
  # hierarchy:
  fout <- factor(levels = c(
    levels(icd9Hierarchy["chapter"]),
    levels(icd9Hierarchy["subchapter"]),
    levels(icd9Hierarchy["major"]),
    levels(icd9Hierarchy["descLong"])
  ))

  # find factor levels for the given codes in icd9hier
  # for chapter/sub/major do we have all rows for that level?

  for (i in icd9Short) {

    if (icd9IsMajor(i)) {
      fout <- c(fout, names(icd9ChaptersMajor)[icd9ChaptersMajor == i])
      break
    }

    fm <- asCharacterNoWarn(icd9Hierarchy[icd9Hierarchy$icd9 == i,
                                          "chapter"])
    peers <- icd9Hierarchy[icd9Hierarchy[["chapter"]] == fm, "icd9"]
    if (all(peers %in% icd9Short)) {
      fout <- c(fout, fm)
      break
    }

    fm <- asCharacterNoWarn(icd9Hierarchy[icd9Hierarchy$icd9 == i,
                                          "subchapter"])
    peers <- icd9Hierarchy[icd9Hierarchy[["subchapter"]] == fm, "icd9"]
    if (all(peers %in% icd9Short)) {
      fout <- c(fout, fm)
      break
    }

    fm <- asCharacterNoWarn(icd9Hierarchy[icd9Hierarchy$icd9 == i,
                                          "major"])
    peers <- icd9Hierarchy[icd9Hierarchy[["major"]] == fm, "icd9"]
    if (all(peers %in% icd9Short)) {
      fout <- c(fout, fm)
      break
    }

    fout <- c(fout,
              asCharacterNoWarn(
                icd9Hierarchy[icd9Hierarchy$icd9 == i, "descLong"]))
  }

  # not just major parts, but the codes which are already majors
  mjrs <- unique(fout[icd9IsMajor(fout)] )
  for (i in mjrs) {
    fm <- asCharacterNoWarn(icd9Hierarchy[icd9Hierarchy$icd9 == i,
                                          "chapter"])
    peers <- icd9Hierarchy[icd9Hierarchy[["chapter"]] == fm, "icd9"]
    if (all(peers %in% icd9Short)) {
      fout <- c(fout, fm)
      break
    }

    fm <- asCharacterNoWarn(icd9Hierarchy[icd9Hierarchy$icd9 == i,
                                          "subchapter"])
    peers <- icd9Hierarchy[icd9Hierarchy[["subchapter"]] == fm, "icd9"]
    if (all(peers %in% icd9Short)) {
      fout <- c(fout, fm)
      break
    }

    fm <- asCharacterNoWarn(icd9Hierarchy[icd9Hierarchy$icd9 == i,
                                          "major"])
    peers <- icd9Hierarchy[icd9Hierarchy[["major"]] == fm, "icd9"]
    if (all(peers %in% icd9Short))  {
      fout <- c(fout, fm)
      break
    }

    fout <- c(fout,
              asCharacterNoWarn(
                icd9Hierarchy[icd9Hierarchy$icd9 == i, "descLong"]))
  }
  asCharacterNoWarn(fout)
}

#' @title condense list of short ICD-9 code into minimal set of major-part-only
#'   codes
#' @description This can be thought of as the inverse operation to
#'   icd9Children("123"). The list given must already contain the parents,
#'   because this function will never add a parent ICD-9 which, although may
#'   have all children present, may itself have an additional clinical meaning.
#' @template icd9-short
#' @template onlyReal
#' @param dropNonReal single logical, if TRUE, and \code{onlyReal} is TRUE, then
#'   codes not found in the master list are dropped; otherwise they are included
#'   in the output.
#' @template invalid
#' @family ICD-9 ranges
#' @export
icd9CondenseToMajor <- function(icd9Short, onlyReal, dropNonReal = TRUE,
                                invalidAction = c("stop", "ignore",
                                                  "silent", "warn")) {
  icd9Short <- icd9ValidNaWarnStopShort(
    icd9Short,
    invalidAction = match.arg(invalidAction))

  # make homogeneous and sort so we will hit the parents first, kids later.
  out <- icd9Short <- sort(icd9AddLeadingZeroesShort(icd9Short))
  mjs <- unique(icd9ShortToMajor(icd9Short))
  # if all major children are in the list, replace those items with just the
  # major codes, leave the rest.
  includemjs <- c()
  for (mj in mjs) {
    matchKids <- icd9ChildrenShort(mj, onlyReal = onlyReal)
    if (all(matchKids %in% out)) {
      out <- out[!out %in% matchKids]
      includemjs <- c(includemjs, mj)
    }
  }
  # TODO: tests for this
  if (onlyReal && dropNonReal) out <- out[icd9RealShort(out)]
  c(unique(includemjs), out)
}
