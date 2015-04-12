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
  assertFlag(isShort)
  assertFlag(toParent)
  if (isShort) return(icd9CondenseShort(icd9, onlyReal))
  icd9CondenseDecimal(icd9, onlyReal)
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
  icd9ShortToDecimal(icd9CondenseShort(icd9DecimalToShort(icd9Decimal), onlyReal))

#' @rdname icd9Condense
#' @template warn
#' @export
icd9CondenseShort <- function(icd9Short, onlyReal = NULL,
                              onlyBillable = NULL,
                              warn = FALSE) {
  # there is no such thing as 'condensing to billable codes' since they are all
  # leaves.
  assertFactorOrCharacter(icd9Short)
  assertFlag(warn)
  icd9Short <- asCharacterNoWarn(icd9Short) # TODO: still necessary?

  i9w <- sort(unique(icd9Short)) # TODO sorting may not be helpful

  # user may assert that the codes are billable: only check this if we are going
  # to warn.
  if (warn && !is.null(onlyBillable)) {
    checkOnlyBillable <- icd9IsBillableShort(icd9Short)
    nonBillable <- icd9Short[checkOnlyBillable]
    billable <- icd9Short[!checkOnlyBillable]
    if (onlyBillable && !all(checkOnlyBillable))
      warning("onlyBillable asserted TRUE, but there are non-billable codes.
              First few such codes are: ",
              paste(head(nonBillable)))
    if (!onlyBillable && all(checkOnlyBillable))
      warning('onlyBillable asserted FALSE, but all the codes are billable!
              Use with "onlyBillable = TRUE" instead.')
  }
  if (is.null(onlyBillable)) onlyBillable <- all(icd9IsBillableShort(icd9Short))

  if (is.null(onlyReal)) {
    if (all(icd9IsRealShort(i9w))) {
      onlyReal <- TRUE
      if (warn) warning("onlyReal not given, but all codes are 'real' so assuming TRUE")
    } else {
      onlyReal <- FALSE
      if (warn) warning("onlyReal not given, but not all codes are 'real' so assuming FALSE")
    }
  }
  assertFlag(onlyReal)
  if (!onlyReal) onlyBillable <- FALSE

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
    test_kids <- icd9ChildrenShort(mp, onlyReal = onlyReal, onlyBillable = onlyBillable)
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
    test_kids <- icd9ChildrenShort(fp, onlyReal = onlyReal, onlyBillable = onlyBillable)
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
