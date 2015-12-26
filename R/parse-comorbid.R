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


#' @title parse AHRQ SAS code to get mapping
#' @description Takes the raw data taken directly from the AHRQ web site and
#'   parses into RData. It is then saved in the development tree data directory,
#'   so this is an internal function, used in generating the package itself!
#' @template savesas
#' @template parse-template
#' @param returnAll logical which, if TRUE, will result in the invisible return
#'   of icd9_map_ahrq_all result, otherwise, icd9_map_ahrq is reutrned.
#' @keywords internal
parse_ahrq_sas <- function(
  sasPath = system.file("data-raw", "comformat2012-2013.txt", package = get_pkg_name()),
  save_data = FALSE, path = NULL, verbose = NULL) {

  if (!missing(path) || !missing(verbose))
    warning("'verbose' and 'path' in parse_ahrq_sas are deprecated and no longer has any effect.
            'path' is now the data dir in the working tree.",
            call. = FALSE)

  assertString(sasPath)
  assertFlag(save_data)
  f <- file(sasPath, "r")
  ahrqAll <- sasFormatExtract(readLines(f)) # these seem to be ascii encoded
  close(f)

  icd9_map_ahrqWork <- ahrqAll[["$RCOMFMT"]]

  icd9_map_ahrq_all <- list()

  for (cmb in names(icd9_map_ahrqWork)) {
    message("parsing AHRQ SAS codes for '", cmb, "'")
    somePairs <- strsplit(x = icd9_map_ahrqWork[[cmb]], split = "-")

    # non-range values (and their children) just go on list
    unpaired_items <- sapply(somePairs, length) == 1
    out <- c()
    if (any(unpaired_items))
      out <- icd_children.icd9(unlist(somePairs[unpaired_items]), defined = FALSE, short_code = TRUE)

    thePairs <- somePairs[lapply(somePairs, length) == 2]
    out <- c(out, lapply(thePairs, function(x) icd9ExpandRangeForSas(x[1], x[2])))
    # update icd9_map_ahrq with full range of icd9 codes:
    icd9_map_ahrq_all[[cmb]] <- out %>% unlist %>% unique
  }

  # drop this superfluous finale which allocates any other ICD-9 code to the
  # "Other" group
  icd9_map_ahrq_all[[" "]] <- NULL

  icd9_map_ahrq <- icd9_map_ahrq_all

  icd9_map_ahrq$HTNCX <- c(
    icd9_map_ahrq$HTNCX, # some codes already in this category
    icd9_map_ahrq$HTNPREG,
    icd9_map_ahrq$OHTNPREG,
    icd9_map_ahrq$HTNWOCHF,
    icd9_map_ahrq$HTNWCHF,
    icd9_map_ahrq$HRENWORF,
    icd9_map_ahrq$HRENWRF,
    icd9_map_ahrq$HHRWOHRF,
    icd9_map_ahrq$HHRWCHF,
    icd9_map_ahrq$HHRWRF,
    icd9_map_ahrq$HHRWHRF)

  icd9_map_ahrq$CHF <- c(
    icd9_map_ahrq$CHF, # some codes already in this category
    icd9_map_ahrq$HTNWCHF,
    icd9_map_ahrq$HHRWCHF,
    icd9_map_ahrq$HHRWHRF)

  icd9_map_ahrq$RENLFAIL <- c(
    icd9_map_ahrq$RENLFAIL, # some codes already in this category
    icd9_map_ahrq$HRENWRF,
    icd9_map_ahrq$HHRWRF,
    icd9_map_ahrq$HHRWHRF)


  icd9_map_ahrq[c("HTNPREG", "OHTNPREG", "HTNWOCHF",
                  "HTNWCHF", "HRENWORF", "HRENWRF", "HHRWOHRF",
                  "HHRWCHF", "HHRWRF", "HHRWHRF")] <- NULL

  # officially, AHRQ HTN with complications means that HTN on its own should be
  # unset. however, this is not feasible here, since we just package up the data
  # into a list, and it can be used however the user wishes. It would not be
  # hard to write an AHRQ specific function to do this if needed, but it makes
  # more sense to me

  #   condense to parents, for each parent, if children are all in the list, add the parent
  for (cmb in names(icd9_map_ahrq)) {
    message("working on ranges for: ", cmb)
    parents <- icd_condense.icd9(icd9_map_ahrq[[cmb]], defined = FALSE, short_code = TRUE)
    for (p in parents) {
      kids <- icd_children.icd9(p, defined = FALSE, short_code = TRUE)
      kids <- kids[-which(kids == p)] # don't include parent in test
      if (all(kids %in% icd9_map_ahrq[[cmb]]))
        icd9_map_ahrq[[cmb]] <- c(icd9_map_ahrq[[cmb]], p) %>% unique %>% icd_sort.icd9(short_code = TRUE)
    }
  }

  names(icd9_map_ahrq) <- icd9::icd_names_ahrq_htn_abbrev
  if (save_data) {
    save_in_data_dir("icd9_map_ahrq") # nocov
    save_in_data_dir("icd9_map_ahrq_all") # nocov
  }
  invisible(icd9_map_ahrq)
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
parse_quan_deyo_sas <- function(sasPath = system.file("data-raw",
                                                      "ICD9_E_Charlson.sas",
                                                      package = get_pkg_name()),
                                condense = NULL,
                                save_data = FALSE,
                                path = NULL) {
  assertFile(sasPath)
  assertFlag(save_data)

  if (!missing(condense) || !missing(path))
    warning("'condense' and 'path' are deprecated in parse_quan_deyo_sas, and no longer has any effect.
            The map can be condensed using other functions in the package. 'path' is now the data dir in the working tree.",
            call. = FALSE)

  quanSas <- readLines(sasPath, warn = FALSE)
  qlets <- sasExtractLetStrings(quanSas)
  qlabels <- qlets[grepl("LBL[[:digit:]]+", names(qlets))]
  icd9_map_quan_deyo <- qlets[grepl("DC[[:digit:]]+", names(qlets))]
  names(icd9_map_quan_deyo) <- unlist(unname(qlabels))

  # use validation: takes time, but these are run-once per package creation (and
  # test) tasks.
  icd9_map_quan_deyo <- lapply(icd9_map_quan_deyo, icd_children.icd9,
                               short_code = TRUE, defined = FALSE)

  # do use icd9:: to refer to a lazy-loaded dataset which is obscurely within
  # the package, but not in its namespace, or something...
  names(icd9_map_quan_deyo) <- icd9::icd_names_charlson_abbrev
  if (save_data)
    save_in_data_dir(icd9_map_quan_deyo)
  invisible(icd9_map_quan_deyo)
}
