
#' @title Condense ICD-9 code by replacing complete families with parent codes
#' @description This can be thought of as the inverse operation to
#'   \code{icd9Children}.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @template onlyReal
#' @family ICD-9 ranges
#' @export
icd9Condense <- function(icd9, isShort = icd9GuessIsShort(icd9),
                         onlyReal = NULL, warn = TRUE) {
  checkmate::assertFlag(isShort)
  if (isShort) return(icd9CondenseShort(icd9,
                                        onlyReal = onlyReal, warn = warn))
  icd9CondenseDecimal(icd9, onlyReal)
}

#' @rdname icd9Condense
#' @export
icd9CondenseDecimal <- function(icd9Decimal, onlyReal = NULL, warn = TRUE)
  icd9ShortToDecimal(icd9CondenseShort(icd9DecimalToShort(icd9Decimal),
                                       onlyReal = onlyReal, warn = warn))



#' @rdname icd9Condense
#' @template warn
#' @export
icd9CondenseShort <- function(icd9Short, onlyReal = NULL, warn = TRUE) {
  assertFactorOrCharacter(icd9Short)
  checkmate::assertFlag(warn)
  icd9Short <- asCharacterNoWarn(icd9Short) # TODO: still necessary?

  #i9w <- sort(unique(icd9GetValidShort(icd9Short))) # TODO sorting may not be helpful
  i9w <- unique(icd9GetValidShort(icd9Short))

  if (is.null(onlyReal)) {
    if (all(icd9IsRealShort(i9w))) {
      onlyReal <- TRUE
      message("'onlyReal' not given, but all codes are 'real' so assuming TRUE")
    } else {
      onlyReal <- FALSE
      if (warn) warning("onlyReal not given, but not all codes are 'real' so assuming FALSE")
    }
  }
  checkmate::assertFlag(onlyReal)

  if (warn && onlyReal && !all(icd9IsRealShort(icd9Short))) {
    icd9Short <- icd9GetRealShort(icd9Short)
    warning("only real values requested, but some undefined ('non-real') ICD-9 code(s) given, so dropping them")
  }

  # find good four digit parents for five digit codes
  # group with the provided four digit parents
  # find good three-digit parents for four digit codes
  # think about V and E codes.

  # any major codes are automatically in output (not condensing higher than
  # three digit code) and all their children can be removed from the work list
  out <- majors <- i9w[areMajor <- icd9IsMajor(i9w)]
  i9w <- i9w[!areMajor]
  i9w <- i9w[i9w %nin% icd9Children(majors, onlyReal = onlyReal)]
fout <- c()
  unique(substr(i9w, 0, 4)) -> four_digit_parents
  # four_digit_parents <- icd9GetValidShort(four_digit_parents)
  for (fp in four_digit_parents) {
    fourth_level <- substr(i9w, 0, 4)
    fourth_matches <- fourth_level == fp
    test_kids <- icd9ChildrenShort(fp, onlyReal = onlyReal) # onlyBillable at 5th level is same as onlyReal
    if (length(test_kids) > 0 && all(test_kids %in% c(fp, i9w))) {
      #if ((length(test_kids) > 1) || (fp %in% i9w)) {
        fout <- c(fout, fp)
        i9w <- i9w[ - which(i9w %in% test_kids)]
      #}
    }
  }

  # 'out' now has original major codes, 'fout' has parent four digit codes. Now
  # see whether any groups of four digit codes comprise all the four digit codes
  # in any parent (or original) major. We test the original majors again to
  # catch where non-billable four digit codes were omitted, but otherwise all
  # the children are complete. This replaces the old 'onlyBillable' flag which
  # was annoying.

  # set new variable so we don't change the thing we are looping over...
  majorParents <- unique(icd9GetMajor(c(out, fout, i9w), isShort = TRUE))
  # majorParents <- icd9GetValidShort(majorParents) # this gets rid of NAs, too.
  for (mp in majorParents) {
    mjrs <- unique(icd9GetMajor(c(out, fout, i9w), isShort = TRUE))
    major_match <- mjrs == mp
    test_kids <- icd9ChildrenShort(mp, onlyReal = onlyReal, onlyBillable = FALSE)
    test_kids <- test_kids[nchar(test_kids) < (5 + icd9IsE(mp))] # we've done these already
    test_kids <- test_kids[ - which(test_kids == mp)]
    if (length(test_kids) > 0 && all(test_kids %in% c(out, fout, i9w))) {
      out <- c(out, mp)
      fout <- fout[ - which(fout %in% test_kids)]
    }
  }
  out <- unique(icd9SortShort(c(out, fout, i9w)))
  if (onlyReal) return(icd9GetRealShort(out)) # should there be any non-real?
  out
}

icd9CondenseShortOld <- function(icd9Short, onlyReal = NULL, warn = FALSE) {
  assertFactorOrCharacter(icd9Short)
  checkmate::assertFlag(warn)
  icd9Short <- asCharacterNoWarn(icd9Short) # TODO: still necessary?

  i9w <- sort(unique(icd9Short)) # TODO sorting may not be helpful

  if (is.null(onlyReal)) {
    if (all(icd9IsRealShort(i9w))) {
      onlyReal <- TRUE
      if (warn) warning("onlyReal not given, but all codes are 'real' so assuming TRUE")
    } else {
      onlyReal <- FALSE
      if (warn) warning("onlyReal not given, but not all codes are 'real' so assuming FALSE")
    }
  }
  checkmate::assertFlag(onlyReal)

  if (warn && onlyReal && !all(icd9IsRealShort(icd9Short)))
    warning("only real values requested, but some undefined ('non-real') ICD-9 code(s) given.")

  # i9w <- "65381" %i9s% "65510"; onlyReal = TRUE; i9w[i9w == "654"] <- "657"; icd9CondenseToParentShort(i9w)
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
  unique(icd9GetMajor(i9w, isShort = TRUE)) -> majorParents
  majorParents <- icd9GetValidShort(majorParents) # this gets rid of NAs, too.
  for (mp in majorParents) {
    icd9GetMajor(i9w, isShort = TRUE) -> mjrs
    major_match <- mjrs == mp
    test_kids <- icd9ChildrenShort(mp, onlyReal = onlyReal, onlyBillable = FALSE)
    #if ((length(test_kids) - !onlyBillable) == sum(major_match)) {
    if (all(test_kids %in% c(mp, i9w))) {
      if ((length(test_kids) > 1) || (mp %in% i9w)) {
        out <- c(out, mp)
        i9w <- i9w[-which(major_match)]
      }
    }
  }

  # now same for four digit codes, thinking carefully about V and E codes
  # the remaining codes are 4 or 5 chars. They have no common parents.
  unique(substr(i9w, 0, 4)) -> fourth_parents
  fourth_parents <- icd9GetValidShort(fourth_parents)
  for (fp in fourth_parents) {
    substr(i9w, 0, 4) -> fourth_level
    fourth_match <- fourth_level == fp
    test_kids <- icd9ChildrenShort(fp, onlyReal = onlyReal, onlyBillable = FALSE)
    # if billable, then if the parent is not billable, the length will be one less
    #if ((length(test_kids) - 1 + (onlyReal && !icd9IsBillableShort(fp))) == sum(fourth_match)) {
    if (all(test_kids %in% c(fp, i9w))) {
      if ((length(test_kids) > 1) || (fp %in% i9w)) {
        out <- c(out, fp)
        i9w <- i9w[-which(fourth_match)]
      }
    }
  }
  #  if (onlyReal) return(icd9GetRealShort(c(i9o, i9w), majorOk = TRUE))
  out <- unique(icd9SortShort(c(out, i9w)))
  if (onlyReal) return(icd9GetRealShort(out))
  out
}
