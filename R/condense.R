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
#' @template onlyReal
#' @family ICD-9 ranges
#' @export
icd9Condense <- function(icd9, isShort = icd9GuessIsShort(icd9),
                         onlyReal = NULL, warn = TRUE) {
  assertFlag(isShort)
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
#' @param keepFactorLevels single logical value, default \code{FALSE}. If
#'   \code{TRUE}, will reuse the factor levels from the input data for the
#'   output data. This only applies if a factor is given for the input codes.
#' @export
icd9CondenseShort <- function(icd9Short, onlyReal = NULL, warn = TRUE, keepFactorLevels = FALSE) {
  assertFactorOrCharacter(icd9Short)
  assertFlag(warn)
  icd9Levels <- levels(icd9Short) # NULL if not a factor

  # we can convert back to factor later. Lots of scope for errors by handling
  # factors and character vectors in this function, so keep simple with
  # character only.
  icd9Short <- asCharacterNoWarn(icd9Short)

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
  assertFlag(onlyReal)

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
  for (fp in four_digit_parents) {
    test_kids <- icd9ChildrenShort(fp, onlyReal = onlyReal) # onlyBillable at 5th level is same as onlyReal
    if (length(test_kids) > 0 && all(test_kids %in% c(fp, i9w))) {
      #if ((length(test_kids) > 1) || (fp %in% i9w)) {
      fout <- c(fout, fp)
      i9w <- i9w[-which(i9w %in% test_kids)]
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
  for (mp in majorParents) {
    test_kids <- icd9ChildrenShort(mp, onlyReal = onlyReal)
    test_kids <- test_kids[nchar(test_kids) < (5 + icd9IsE(mp))] # we've done these already
    test_kids <- test_kids[-which(test_kids == mp)]
    if (length(test_kids) > 0 && all(test_kids %in% c(out, fout, i9w))) {
      out <- c(out, mp)
      fout <- fout[-which(fout %in% test_kids)]
    }
  }
  out <- unique(icd9SortShort(c(out, fout, i9w)))

  if (!is.null(icd9Levels)) {
    if (keepFactorLevels)
      out <- factor(out, icd9Levels)
    else
      out <- factor(out)
  }

  if (onlyReal) return(icd9GetRealShort(out)) # should there be any non-real?
  out
}
