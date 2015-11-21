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


#' @title parse AHRQ data
#' @description Takes the raw data taken directly from the AHRQ web site and
#'   parses into RData. It is then saved in the development tree data directory,
#'   so this is an internal function, used in generating the package itself!
#' @template savesas
#' @template parse-template
#' @param returnAll logical which, if TRUE, will result in the invisible return
#'   of ahrqComorbidAll result, otherwise, ahrqComorbid is reutrned.
#' @template verbose
#' @keywords internal
parseAhrqSas <- function(sasPath = system.file("data-raw", "comformat2012-2013.txt", package = get_pkg_name()),
                         save = FALSE, path = "data", verbose = FALSE) {
  #returnAll = FALSE,
  assertString(sasPath)
  assertString(path)
  assertFlag(save)
  f <- file(sasPath, "r")
  ahrqAll <- sasFormatExtract(readLines(f)) # these seem to be ascii encoded
  close(f)

  ahrqComorbidWork <- ahrqAll[["$RCOMFMT"]]

  ahrqComorbidAll <- list()

  for (cmb in names(ahrqComorbidWork)) {
    if (verbose) message("parsing AHRQ SAS codes for '", cmb, "'")
    somePairs <- strsplit(x = ahrqComorbidWork[[cmb]], split = "-")

    # non-range values (and their children) just go on list
    unpaired_items <- sapply(somePairs, length) == 1
    out <- c()
    if (any(unpaired_items))
      out <- icd9ChildrenShort(unlist(somePairs[unpaired_items]), onlyReal = FALSE)

    thePairs <- somePairs[lapply(somePairs, length) == 2]
    out <- c(out, lapply(thePairs, function(x) icd9ExpandRangeForSas(x[1], x[2])))
    # update ahrqComorbid with full range of icd9 codes:
    ahrqComorbidAll[[cmb]] <- icd9SortShort(unique(unlist(out)))
  }

  # drop this superfluous finale which allocates any other ICD-9 code to the
  # "Other" group
  ahrqComorbidAll[[" "]] <- NULL

  ahrqComorbid <- ahrqComorbidAll

  ahrqComorbid$HTNCX <- c(
    ahrqComorbid$HTNCX, # some codes already in this category
    ahrqComorbid$HTNPREG,
    ahrqComorbid$OHTNPREG,
    ahrqComorbid$HTNWOCHF,
    ahrqComorbid$HTNWCHF,
    ahrqComorbid$HRENWORF,
    ahrqComorbid$HRENWRF,
    ahrqComorbid$HHRWOHRF,
    ahrqComorbid$HHRWCHF,
    ahrqComorbid$HHRWRF,
    ahrqComorbid$HHRWHRF)

  ahrqComorbid$CHF <- c(
    ahrqComorbid$CHF, # some codes already in this category
    ahrqComorbid$HTNWCHF,
    ahrqComorbid$HHRWCHF,
    ahrqComorbid$HHRWHRF)

  ahrqComorbid$RENLFAIL <- c(
    ahrqComorbid$RENLFAIL, # some codes already in this category
    ahrqComorbid$HRENWRF,
    ahrqComorbid$HHRWRF,
    ahrqComorbid$HHRWHRF)


  ahrqComorbid[c("HTNPREG", "OHTNPREG", "HTNWOCHF",
                 "HTNWCHF", "HRENWORF", "HRENWRF", "HHRWOHRF",
                 "HHRWCHF", "HHRWRF", "HHRWHRF")] <- NULL

  # officially, AHRQ HTN with complications means that HTN on its own should be
  # unset. however, this is not feasible here, since we just package up the data
  # into a list, and it can be used however the user wishes. It would not be
  # hard to write an AHRQ specific function to do this if needed, but it makes
  # more sense to me

  #   condense to parents, for each parent, if children are all in the list, add the parent
  for (cmb in names(ahrqComorbid)) {
    if (verbose) message("working on ranges for: ", cmb)
    parents <- icd9CondenseShort(ahrqComorbid[[cmb]], onlyReal = FALSE)
    for (p in parents) {
      kids <- icd9ChildrenShort(p, onlyReal = FALSE)
      kids <- kids[-which(kids == p)] # don't include parent in test
      if (all(kids %in% ahrqComorbid[[cmb]]))
        ahrqComorbid[[cmb]] <- c(ahrqComorbid[[cmb]], p)
    }
  }

  names(ahrqComorbid) <- icd9::ahrqComorbidNamesHtnAbbrev
  if (save) save_in_data_dir("ahrqComorbid") # nocov
  invisible(ahrqComorbid)
}

#' @title parse original SAS code defining Quan's update of Deyo comorbidities.
#' @description As with \code{parseAhrqSas}, this function reads SAS code, and
#'   in, a very limited way, extracts definitions. In this case the code uses
#'   LET statements, with strings or lists of strings. This saves and invisibly
#'   returns a list with names corresponding to the comorbidities and values as
#'   a vector of 'short' form (i.e. non-decimal) ICD9 codes. Unlike
#'   \code{parseAhrqSas}, there are no ranges defined, so this interpretation is
#'   simpler.
#'
#'   With thanks to Dr. Quan, I have permission to distribute his SAS code.
#'   Previously, the SAS code would be downloaded from the University of
#'   Manitoba at
#'   \url{http://mchp-appserv.cpe.umanitoba.ca/concept/ICD9_E_Charlson.sas.txt}.
#'   There are structural differences between this version and the version
#'   directly from Dr. Quan, however, the parsing results in identical data.
#' @template savesas
#' @template parse-template
#' @keywords internal
parseQuanDeyoSas <- function(sasPath = system.file("data-raw",
                                                   "ICD9_E_Charlson.sas",
                                                   package = get_pkg_name()),
                             condense = FALSE,
                             save = FALSE,
                             path = "data") {

  quanSas <- readLines(sasPath, warn = FALSE)
  qlets <- sasExtractLetStrings(quanSas)
  qlabels <- qlets[grepl("LBL[[:digit:]]+", names(qlets))]
  quanDeyoComorbid <- qlets[grepl("DC[[:digit:]]+", names(qlets))]
  names(quanDeyoComorbid) <- unlist(unname(qlabels))

  # use validation: takes time, but these are run-once per package creation (and
  # test) tasks.
  if (condense)
    quanDeyoComorbid <- lapply(quanDeyoComorbid, icd9Condense)
  else
    quanDeyoComorbid <- lapply(quanDeyoComorbid, icd9ChildrenShort, onlyReal = FALSE)

  names(quanDeyoComorbid) <- icd9::charlsonComorbidNamesAbbrev
  if (save) save_in_data_dir(quanDeyoComorbid)
  invisible(quanDeyoComorbid)
}
