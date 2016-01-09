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

# nocov start

# icd9_sources is defined in this file and saved in sysdata.rda
utils::globalVariables(c("icd9_sources"))

#' @title generate all package data
#' @description Parses (and downloads if necessary) CDC annual revisions of
#'   ICD-9-CM to get the 'billable' codes. Also parses the AHRQ and Quan/Deyo
#'   comorbidity mappings from the source SAS data. Elixhauser and
#'   Quan/Elixhauser mappings are generated from transcribed codes.
#' @keywords internal
generate_everything <- function() {
  # this is not strictly a parsing step, but is quite slow. It relies on picking
  # up already saved files from previous steps. It can take hours to complete,
  # but only needs to be done rarely. This is only intended to be run from
  # development tree, not as installed package
  loadNamespace("devtools")
  generate_sysdata()

  devtools::load_data(pkg = ".") # reload the newly saved data
  loadNamespace("devtools")
  message("Parsing RTF file(s) to create icd9Desc descriptions of entire hierarchy")
  devtools::load_data(pkg = ".")

  # plain text billable codes
  message("Parsing plain text billable codes to create icd9cm_billable list of
                       data frames with descriptions of billable codes only.
                       No dependencies on other data.")
  parse_leaf_descriptions_all(save_data = TRUE)
  devtools::load_data(pkg = ".")

  message("Parsing comorbidity mappings from SAS and text sources.
                       (Make sure lookup files are updated first.)
                       Depends on icd9cm_hierarchy being updated.")
  parse_ahrq_sas(save_data = TRUE)
  parse_quan_deyo_sas(save_data = TRUE)
  icd9_generate_map_quan_elix(save_data = TRUE)
  icd9_generate_map_elix(save_data = TRUE)
  devtools::load_data(pkg = ".") # reload the newly saved data
  icd9cm_generate_chapters_hierarchy(save_data = TRUE, verbose = FALSE) # depends on icd9cm_billable

}
# nocov end

#' @title get billable codes from all available years
#' @description for versions 23 to 32, those which are on the CMS web site, get
#'   any codes with long or short descriptions. Earlier years only have
#'   abbreviated descriptions.
#' @param save_data single logical value, if \code{TRUE} the source text or CSV file
#'   will be saved in \code{data-raw}, otherwise (the default) the data is
#'   simply returned invisibly.
#' @template offline
#' @return data frame with icd9, descShort and descLong columns. NA is placed in
#'   descLong when not available.
#' @examples
#'   # To populate the data-raw directory with the ICD-9 source:
#'   # not included in installed package, run using the full source from github,
#'   # e.g. using devtools::load_all()
#'   \dontrun{
#'   parseLeafDescriptionsAll(save_data = TRUE, offline = TRUE)
#'   }
#' @source
#' http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
#' @keywords internal
parse_leaf_descriptions_all <- function(save_data = FALSE, offline = FALSE) {
  assertFlag(save_data)
  assertFlag(offline)

  # icd9_sources is in sysdata.RData
  versions <- icd9_sources$version
  message("Available versions of sources are: ", paste(versions, collapse = ", "))
  icd9cm_billable <- list()
  for (v in versions)
    icd9cm_billable[[v]] <- parse_leaf_descriptions_version(version = v,
                                                            save_data = save_data,
                                                            offline = offline)

  # and in my utils.R  getNonASCII(charactervector)
  if (save_data)
    save_in_data_dir("icd9cm_billable")
  invisible(icd9cm_billable)
}

#' @title read the ICD-9-CM description data as provided by the Center for
#'   Medicaid Services.
#' @description ICD9-CM data unfortunately has no comma separation, so have to
#'   pre-process. Note that this canonical data doesn't specify non-diagnostic
#'   higher-level codes, just the specific diagnostic 'child' codes.
#'
#'   The file can be pulled from the zip files on the CMS web site or from
#'   within the package. Pulled data can be saved to the package development
#'   tree.
#' @param icd9path path of the source data which is typically in \code{data-raw}
#' @param save logical whether to attempt to re-save source files in inst
#' @param path Absolute path in which to save parsed data
#' @return invisibly return the result
#' @keywords internal
parse_leaf_descriptions_version <- function(version = icd9cm_latest_edition(),
                                            save_data = FALSE,
                                            offline = FALSE) {
  assertString(version)
  assertFlag(save_data)
  assertFlag(offline)

  message("Fetching billable codes version: ", version)

  if (version == "27")
    return(invisible(parse_leaf_desc_icd9cm_v27(offline = offline)))
  stopifnot(version %in% icd9_sources$version)
  dat <- icd9_sources[icd9_sources$version == version, ]
  fn_short_orig <- dat$short_filename
  fn_long_orig <- dat$long_filename

  f_info_short <- unzip_to_data_raw(dat$url, file_name = fn_short_orig, offline = offline)
  f_info_long <- NULL
  if (!is.na(fn_long_orig))
      f_info_long <- unzip_to_data_raw(dat$url, file_name = fn_long_orig, offline = offline)

  message("short filename = ", f_info_short$file_name,
          "\n long filename = ", f_info_long$file_name)
  message("short path = ", f_info_short$file_path,
          "\n long path = ", f_info_long$file_name)

  # yes, specify encoding twice, once to declare the source format, and again
  # to tell R to flag (apparently only where necessary), the destination
  # strings: in our case this is about ten accented character in long
  # descriptions of disease names

  # shortlines should always exist
  shortlines <- readLines(f_info_short$file_path)

  # longlines may not, and may have more complicated encoding
  if (!is.na(fn_long_orig)) {
    file_long <- file(f_info_long$file_path, encoding = "latin1")
    longlines <- readLines(f_info_long$file_path, encoding = "latin1")
    close(file_long)
  } else
    longlines <- NA_character_

  shortlines <- str_split(shortlines, "[[:space:]]")
  longlines <- str_split(longlines, "[[:space:]]")

  # my trim function drops encodings, so let's use stringr::str_trim:
  short_codes <- lapply(shortlines, FUN = function(x) str_trim(x[1]))
  short_descs <- lapply(shortlines,
                        function(x) str_trim(paste(x[-1], collapse = " ")))

  if (!is.na(longlines[1]))
    long_descs <- lapply(longlines,
                         function(x) str_trim(paste(x[-1], collapse = " ")))
  else
    long_descs <- NA

  out <- data.frame(icd9 = unlist(short_codes),
                    descShort = unlist(short_descs),
                    descLong = unlist(long_descs),
                    stringsAsFactors = FALSE)

  # now sort so that E is after V:
  reorder <- icd9_order_short(out[["icd9"]])
  out <- out[reorder, ]

  # warn as we go:
  oldwarn <- options(warn = 1)
  on.exit(options(oldwarn))
  if (!is.na(fn_long_orig)) {
    encs <- Encoding(out[["descLong"]])
    message("Found labelled encodings: ", paste(unique(encs), collapse = ", "))
    message("non-ASCII rows of long descriptions are: ",
            paste(getNonASCII(out[["descLong"]]), collapse = ", "))
    message(Encoding(out[["descLong"]][isNonASCII(out[["descLong"]])]))

  }
  invisible(out)
}

parse_leaf_desc_icd9cm_v27 <- function(offline = FALSE) {
  message("working on version 27 quirk")
  assertFlag(offline)
  v27_dat <- icd9_sources[icd9_sources$version == "27", ]
  fn_orig <- v27_dat$other_filename
  url <- v27_dat$url

  message("original v27 file name = '", fn_orig,
          "'. URL = ", url)

  f27_info <- unzip_to_data_raw(url, fn_orig, offline = offline)

  f <- file(f27_info$file_path, encoding = "latin1")
  icd9cm_billable27 <- read.csv(f27_info$file_path, stringsAsFactors = FALSE,
                                colClasses = "character", encoding = "latin1")
  close(f)
  names(icd9cm_billable27) <- c("icd9", "descLong", "descShort")
  icd9cm_billable27 <- icd9cm_billable27[c(1, 3, 2)] # reorder columns
  reorder <- icd9_order_short(icd9cm_billable27[["icd9"]])
  invisible(icd9cm_billable27[reorder, ])
}

#' generate ICD-9-CM hierarchy
#'
#' For each row of billing code, give the chapter, sub-chapter, major code and
#' description, and short and long descriptions. Currently this is specifically
#' for the 2011 ICD-9-CM after which there have been minimal changes.
#' Thankfully, ICD-10-CM has machine readable data available.
#' @keywords internal
icd9cm_generate_chapters_hierarchy <- function(save_data = FALSE,
                                               verbose = FALSE) {
  assertFlag(save_data)
  assertFlag(verbose)

  message("get column of ICD-9 codes, up to the three digit headings. ~10s")
  icd9Desc <- parse_rtf_year(year = "2011",
                             save_data = FALSE,
                             verbose = verbose)

  message("slower step of building icd9 chapters hierarchy from 2011 RTF. ~20s")
  chaps <- icd9_get_chapters(x = icd9Desc$icd9,
                             short_code = TRUE,
                             verbose = verbose)

  # could also get some long descs from more recent billable lists, but not
  # older ones which only have short descs
  icd9cm_hierarchy <- cbind(
    data.frame("icd9" = icd9Desc$icd9,
               "descLong" = icd9Desc$desc,
               stringsAsFactors = FALSE),
    # the following can and should be factors:
    chaps
  )

  # fix congenital abnormalities not having subchapter defined:
  # ( this might be easier to do when parsing the chapters themselves...)
  icd9cm_hierarchy <- fixSubchapterNa(icd9cm_hierarchy, 740, 759)
  # and hematopoietic organs
  icd9cm_hierarchy <- fixSubchapterNa(icd9cm_hierarchy, 280, 289)

  # insert the short descriptions from the billable codes text file. Where there
  # is no short description, e.g. for most Major codes, or intermediate codes,
  # just copy the long description over.
  bill32 <- icd9::icd9cm_billable[["32"]]

  billable_codes <- icd_get_billable.icd9(icd9cm_hierarchy$icd9, short_code = TRUE) # or from bill32
  billable_rows <- which(icd9cm_hierarchy$icd9 %fin% billable_codes)
  title_rows <- which(icd9cm_hierarchy$icd9 %nin% billable_codes)
  icd9cm_hierarchy[billable_rows, "descShort"] <- bill32$descShort
  # for rows without a short description (i.e. titles, non-billable),
  # useexisting long desc
  icd9cm_hierarchy[title_rows, "descShort"] <- icd9cm_hierarchy[title_rows, "descLong"]
  # the billable codes list (where available) currently has better long
  # descriptions than the RTF parse. For previous years, there is no long desc
  # in billable, so careful when updating this.
  icd9cm_hierarchy[billable_rows, "descLong"] <- bill32$descLong

  # now put the short description in the right column position
  icd9cm_hierarchy <- icd9cm_hierarchy[c("icd9", "descShort", "descLong", "threedigit",
                                     "major", "subchapter", "chapter")]

  # quick sanity checks - full tests in test-parse.R
  stopifnot(all(icd_is_valid.icd9(icd9cm_hierarchy$icd9, short_code = TRUE)))
  stopifnot(!any(sapply(icd9cm_hierarchy, is.na)))

  if (save_data)
    save_in_data_dir("icd9cm_hierarchy") # nocov
  invisible(icd9cm_hierarchy)
}

#' Fix NA subchapters in RTF parsing
#'
#' Fixes a couple of corner cases in parsing the 2011 ICD-9-CM RTF
#' @keywords internal
fixSubchapterNa <- function(x, start, end) {
  # 740 CONGENITAL ANOMALIES is a chapter with no sub-chapters defined. For
  # consistency, assign the same name to sub-chapters
  congenital <- x$icd9 %in% (start %i9sa% end)
  # assert all the same:
  stopifnot(all(x[congenital[1], "chapter"] == x[congenital[-1], "chapter"]))
  # now some work to insert a new level into the sub-chapter factor in the right place
  previous_sub <- asCharacterNoWarn(x[(which(congenital) - 1)[1], "subchapter"])
  previous_sub_pos <- which(levels(x$subchapter) == previous_sub)
  congenital_title <- asCharacterNoWarn(x[which(congenital)[1], "chapter"])
  new_subs <- asCharacterNoWarn(x$subchapter)
  new_subs[congenital] <- congenital_title
  new_levels <- append(levels(x$subchapter), congenital_title, previous_sub_pos)
  x$subchapter <- factor(new_subs, new_levels)
  x
}
