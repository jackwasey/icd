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
#' @param brief single logical value, default is \code{FALSE}. If \code{TRUE},
#'   the short description from the canonical CMS descriptions (included in
#'   \code{extdata}) will be used, otherwise the long description is used.
#' @param warn single logical value, default is \code{TRUE}, meaning that codes
#'   which do not correspond to diagnoses, or to three-digit codes, will trigger
#'   a warning.
#' @examples
#' icd9ExplainShort(ahrqComorbid[[1]][1:3])
#' icd9ExplainShort(ahrqComorbid[[1]][1:3], brief = TRUE)
#' @return data frame, or list of data frames, with fields for ICD9 code, name
#'   and description, derived from datamart lookup table
#' @seealso package comorbidities
#' @references \url{http://www.stata.com/help.cgi?icd9}
#' @export
icd9Explain <- function(icd9, isShort = icd9GuessIsShort(icd9),
                        doCondense = TRUE, brief = FALSE, warn = TRUE) {
  UseMethod("icd9Explain")
}

#' @rdname icd9Explain
#' @export
icd9ExplainShort <- function(icd9Short, doCondense = TRUE, brief = FALSE, warn = TRUE) {
  icd9Explain(icd9Short, isShort = TRUE,
              doCondense = doCondense, brief = brief, warn = warn)
}

#' @rdname icd9Explain
#' @export
icd9ExplainDecimal <- function(icd9Decimal, doCondense = TRUE, brief = FALSE, warn = TRUE) {
  icd9Explain(icd9Decimal, isShort = FALSE,
              doCondense = doCondense, brief = brief, warn = warn)
}

#' @describeIn icd9Explain explain all ICD-9 codes in a list of vectors
#' @export
icd9Explain.list <- function(icd9,  isShort = icd9GuessIsShort(icd9),
                             doCondense = TRUE, brief = FALSE, warn = TRUE) {
  lapply(icd9, icd9Explain, isShort = isShort,
         doCondense = doCondense, brief = brief, warn = warn)
}

#' @describeIn icd9Explain explain character vector of ICD-9 codes
#' @export
icd9Explain.character <- function(icd9, isShort = icd9GuessIsShort(icd9),
                                  doCondense = TRUE, brief = FALSE, warn = TRUE) {

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
    icd9 <- icd9CondenseShort(icd9, onlyReal)
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

#' @describeIn icd9Explain explain numeric vector of ICD-9 codes, with warning
#' @export
icd9Explain.numeric <- function(icd9, isShort = icd9GuessIsShort(icd9),
                                doCondense = TRUE, brief = FALSE, warn = FALSE) {
  warnNumericCode()
  icd9Explain.character(as.character(icd9), isShort = isShort,
                        doCondense = doCondense, brief = brief, warn = warn)
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
  # don't take responsibility for validation here.
  if (is.list(icd9)) icd9 <- unlist(icd9)
  icd9 <- as.character(icd9)
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
#'   now.
#' @param icd9-any
#' @param isShort
#' @param invalid
#' @keywords internal
icd9GetChapters <- function(icd9, isShort = icd9GuessIsShort(icd9)) {

  # set up comorbidity maps for chapters/sub/major group, then loop through each
  # ICD-9 code, loop through each comorbidity and lookup code in the map for
  # that field, then add the factor level for the match. There should be 100%
  # matches.

  majors     <- icd9GetMajor(icd9, isShort)

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

# this is rather slow, queries a web page repeatedly
icd9GetChaptersHierarchy <- function(save = FALSE) {

  # don't rely on having already done this when setting up other data.
  icd9CmDesc <- parseIcd9Descriptions()

  icd9Hierarchy <- cbind(
    icd9CmDesc,
    icd9GetChapters(icd9 = icd9CmDesc[["icd9"]], isShort = TRUE)
  )
  if (save) saveInDataDir("icd9Hierarchy")
}

#' @title Condense ICD-9 code by replacing complete families with parent codes
#' @description This can be thought of as the inverse operation to
#'   \code{icd9Children}.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @template onlyReal
#' @param toMajor Most major codes are not \emph{real}, e.g. Salmonella 003 is a
#'   major category, but is not itself used as a diagnostic code. Therefore,
#'   strictly, asking for only \emph{real} codes excludes the major. We'll turn
#'   a blind eye to this if we specifically request majors with toMajor.
#' @family ICD-9 ranges
#' @export
icd9Condense <- function(icd9, isShort, onlyReal = NULL, toMajor = TRUE) {
  if (isShort) return(icd9CondenseShort(icd9, onlyReal, toMajor))
  icd9CondenseDecimal(icd9, onlyReal, toMajor)
}

#' @rdname icd9Condense
#' @details \code{icd9CondenseToMajor} family of functions are required, at
#'   least in this release because of issue #37 in github. This function just
#'   gets majors, unlike \code{icd9CondenseShort}.
#' @export
icd9CondenseToMajor <- function(icd9, isShort = icd9GuessIsShort(icd9),
                                onlyReal = NULL) {
  if (isShort) return(icd9CondenseToMajorShort(icd9, onlyReal))
  icd9CondenseToMajorDecimal(icd9, onlyReal)
}

#' @rdname icd9Condense
#' @export
icd9CondenseToMajorDecimal <- function(icd9Decimal, onlyReal = NULL)
  icd9CondenseToMajorShort(icd9DecimalToShort(icd9Decimal), onlyReal)

#' @rdname icd9Condense
#' @export
icd9CondenseToMajorShort <- function(icd9Short, onlyReal = NULL) {
  i9w <- sort(unique(icd9Short))

  if (is.null(onlyReal)) {
    if (all(icd9IsRealShort(i9w, majorOk = TRUE))) {
      onlyReal <- TRUE
      message("onlyReal not given, but all codes 'real' so assuming TRUE")
    } else {
      onlyReal <- FALSE
      message("onlyReal not given, but not all codes 'real' so assuming FALSE")
    }
  } else {
    checkmate::checkLogical(onlyReal, len = 1)
  }

  if (onlyReal && !all(icd9IsRealShort(icd9Short, majorOk = TRUE)))
    warning("only real values requested, but unreal ICD-9 code(s) given.")

  i9o <- c()
  for (i in unique(icd9GetMajor(i9w, isShort = TRUE))) {

    matchKids <- icd9ChildrenShort(i, onlyReal = onlyReal)
    lookin <- unique(c(icd9GetMajor(i, isShort = TRUE), i9w))
    if (all(matchKids %in% lookin)) {
      i9w <- i9w[i9w %nin% matchKids] # drop the matches
      i9o <- c(i9o, i)
    }
  }
  if (onlyReal) return(icd9GetRealShort(c(i9o, i9w), majorOk = TRUE))
  # return parents with residual unmatched leaf codes, in this case, majors, and
  # incomplete sets of non-majors.
  sort(unique(c(i9o, i9w)))
}

#' @rdname icd9Condense
#' @export
icd9CondenseShort <- function(icd9Short, onlyReal = NULL, toMajor = TRUE) {

  checkmate::checkLogical(toMajor, len = 1, any.missing = FALSE)
  i9w <- sort(unique(icd9Short))

  if (is.null(onlyReal)) {
    if (all(icd9IsRealShort(i9w, majorOk = toMajor))) {
      onlyReal <- TRUE
      message("onlyReal not given, but all codes 'real' so assuming TRUE")
    } else {
      onlyReal <- FALSE
      message("onlyReal not given, but not all codes 'real' so assuming FALSE")
    }
  } else {
    checkmate::checkLogical(onlyReal, len = 1)
  }

  if (onlyReal && !all(icd9IsRealShort(icd9Short, majorOk = toMajor)))
    warning("only real values requested, but unreal ICD-9 code(s) given.")

  if (toMajor)
    i9n <- unique(c(icd9GetMajor(i9w, isShort = TRUE), i9w))
  else
    i9n <- i9w

  i9o <- c()
  for (i in i9n) {
    matchKids <- icd9ChildrenShort(i, onlyReal = onlyReal)
    lookin <- i9w
    if (toMajor) lookin <- unique(c(icd9GetMajor(i, isShort = TRUE), lookin))
    if (all(matchKids %in% lookin)) {
      # drop the matches (but not including major if added)
      i9w <- i9w[i9w %nin% matchKids]
      i9o <- c(i9o, i)
    }
  }

  if (onlyReal) return(icd9GetRealShort(unique(c(i9o, i9w)), majorOk = toMajor))
  # return parents with residual unmatched leaf codes
  sort(unique(c(i9o, i9w)))
}

#' @rdname icd9Condense
#' @export
icd9CondenseDecimal <- function(icd9Decimal, onlyReal = NULL, toMajor = TRUE)
  icd9ShortToDecimal(
    icd9CondenseShort(
      icd9DecimalToShort(icd9Decimal), onlyReal, toMajor
    )
  )
