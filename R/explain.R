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
#' @param doCondense single logical value which indicates whether to condense the given set of ICD-9 codes by replacing subsets of codes with 'parent' codes which exactly encompass certain subsets. E.g. If all cholera diagnoses are provided, only '001 - Cholera' needs to be displayed, not all subtypes. This is currently partially implemented. See issue #3 in github.
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
    # make sure there are preceding zeroes, in order to match the icd9CmDesc data.
    icd9 <- icd9AddLeadingZeroesDecimal(icd9)
    icd9 <- icd9DecimalToShort(icd9)
  }
  orphans <- c()
  if (doCondense) {
    # find common parent ICD-9 codes, but only if they have a description
    icd9 <- icd9CondenseToExplainShort(icd9, invalidAction = "warn")
    # find those codes without explanations:
    unexplainedParents <- icd9[!(icd9 %in% icd9CmDesc[["icd9"]])]
    # get all their children, so we can work backwards to the highest-level explanations
    orphans <- icd9CondenseToExplainShort(icd9ChildrenShort(unexplainedParents), invalidAction = "warn")
  }
  out <- icd9CmDesc[ icd9CmDesc[["icd9"]] %in% c(icd9, orphans), ]
  row.names(out) <- NULL
  names(out) <- c("ICD-9", "Diagnosis", "Description")
  if (!isShort) out[["ICD-9"]] <- icd9ShortToDecimal(out[["ICD-9"]])
  out
}

#' @describeIn icd9Explain explain numeric vector of ICD-9 codes, with warning
#' @export
icd9Explain.numeric <- function(icd9, isShort, doCondense = TRUE) {
  warning("Numeric ICD-9 codes are unable to accurately represent actual ICD-9 codes.
          Converting to character, but beware of inevitable errors.")
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
    icd9ValidNaWarnStopShort(icd9Short = testCodes, invalidAction = invalidAction)
    return(TRUE)
  }
  if (vdm - vsm > 0.5) {
    icd9ValidNaWarnStopDecimal(icd9Decimal = testCodes, invalidAction = invalidAction)
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
icd9Chapter <- function(icd9, isShort, invalidAction = icd9InvalidActions) {
  invalidAction = match.arg(invalidAction)
  if (isShort) {
    majors <- icd9ShortToMajor(icd9, invalidAction)
  } else {
    majors <- icd9DecimalToMajor(icd9, invalidAction)
  }
  chapterExpandedRanges <- lapply(icd9CmChapters, function(x) x[["start"]] %i9mj% x[["end"]])
  #TODO complete this with 'spread' type function, as done when generating co-morbidities. Maybe use dplyr
  #this is derived from icd9Comorbid. ?generalize

}
