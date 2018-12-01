# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

#nocov start

#' generate all package data
#'
#' Parses (and downloads if necessary) CDC annual revisions of ICD-9-CM to get
#' the 'billable' codes. Also parses the AHRQ, Quan/Deyo, and CMS HCC comorbidity mappings
#' from the source SAS data. Elixhauser and Quan/Elixhauser mappings are
#' generated from transcribed codes.
#' @keywords internal datagen
update_everything <- function() {
  # this is not strictly a parsing step, but is quite slow. It relies on picking
  # up already saved files from previous steps. It can take hours to complete,
  # but only needs to be done rarely. This is only intended to be run from
  # development tree, not as installed package
  generate_sysdata()
  load(file.path("R", "sysdata.rda"))
  generate_spelling()
  # plain text billable codes
  message("Parsing plain text billable codes to create icd9cm_billable list of
                       data frames with descriptions of billable codes only.
                       No dependencies on other data.")
  parse_leaf_descriptions_all(save_data = TRUE, offline = FALSE) # nolint
  load(system.file("data", "icd9cm_billable.RData", package = "icd"))
  message("Parsing comorbidity mappings from SAS and text sources.
                       (Make sure lookup files are updated first.)
                       Depends on icd9cm_hierarchy being updated.")
  # ICD 9
  icd9_parse_ahrq_sas(save_data = TRUE, offline = FALSE)
  icd9_parse_quan_deyo_sas(save_data = TRUE, offline = FALSE)
  icd9_parse_cc(save_data = TRUE)
  icd9_parse_ahrq_ccs(single = TRUE, save_data = TRUE, offline = FALSE)
  icd9_parse_ahrq_ccs(single = FALSE, save_data = TRUE, offline = FALSE)
  icd10_parse_ahrq_ccs(version = "2018.1", save_data = TRUE, offline = FALSE)
  icd9_generate_map_quan_elix(save_data = TRUE)
  icd9_generate_map_elix(save_data = TRUE)
  # ICD 10
  icd10_parse_ahrq_sas(save_data = TRUE, offline = FALSE)
  icd10_parse_cc(save_data = TRUE)
  icd10_parse_ahrq_pcs(save_data = TRUE)
  icd10_generate_map_quan_elix(save_data = TRUE)
  icd10_generate_map_quan_deyo(save_data = TRUE)
  icd10_generate_map_elix(save_data = TRUE)
  icd10cm_get_all_defined(save_data = TRUE, offline = FALSE)
  icd10cm_extract_sub_chapters(save_data = TRUE, offline = FALSE)
  # reload the newly saved data before generating chapters.
  # The next step depends on icd9cm_billable
  icd9cm_generate_chapters_hierarchy(save_data = TRUE, offline = FALSE, verbose = FALSE)
  generate_maps_pccc(save_data = TRUE)
}

# quick sanity checks - full tests of x in test-parse.R
icd9cm_hierarchy_sanity <- function(x) {
  stopifnot(all(is_valid.icd9(x[["code"]], short_code = TRUE)))
  if (!any(vapply(x, is.na, logical(1))))
    return()
  print(colSums(vapply(x, is.na, logical(1))))
  print(x[which(is.na(x$major)), ])
  print(x[which(is.na(x$three_digit)), ])
  print(x[which(is.na(x$sub_chapter))[1:10], ]) # just top ten
  print(x[which(is.na(x$chapter)), ])
  stop("should not have any NA values in the ICD-9-CM flatten hierarchy data frame")
}

#' Get billable codes from all available years
#'
#' For versions 23 to 32, those which are on the CMS web site, get any codes
#' with long or short descriptions. Earlier years only have abbreviated
#' descriptions.
#' @param save_data single logical value, if \code{TRUE} the source text or CSV
#'   file will be saved in the raw data directory, otherwise (the default) the
#'   data is simply returned invisibly.
#' @template offline
#' @return data frame with \code{icd9}, \code{short_desc} and \code{long_desc}
#'   columns. \code{NA} is placed in \code{long_desc} when not available.
#' @examples
#'   # To populate the raw data directory with the ICD-9 source:
#'   # not included in installed package, run using the full source from github,
#'   # e.g. using devtools::load_all()
#'   \dontrun{
#'   parse_leaf_descriptions_all(save_data = TRUE, offline = TRUE)
#'   }
#' @source
#' http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
#' @keywords internal datagen
parse_leaf_descriptions_all <- function(save_data = TRUE, offline = TRUE) {
  assert_flag(save_data)
  assert_flag(offline)
  versions <- icd::icd9_sources$version
  message("Available versions of sources are: ", paste(versions, collapse = ", "))
  icd9cm_billable <- list()
  for (v in versions) {
    message("working on version: ", v)
    icd9cm_billable[[v]] <- icd9_parse_leaf_desc_ver(version = v,
                                                     save_data = save_data,
                                                     offline = offline)
    icd9cm_billable[[v]][["short_desc"]] <- enc2utf8(icd9cm_billable[[v]][["short_desc"]])
    icd9cm_billable[[v]][["long_desc"]] <- enc2utf8(icd9cm_billable[[v]][["long_desc"]])
  }
  if (save_data)
    save_in_data_dir(icd9cm_billable)
  invisible(icd9cm_billable)
}

#' Read the ICD-9-CM description data as provided by the Center for Medicaid
#' Services (CMS).
#'
#' The full ICD-9 specification is in an RTF file, but CMS also distributes a
#' space-separated text file with just the definitions for 'defined', 'billable'
#' codes. Note that this canonical data doesn't specify non-diagnostic
#' higher-level codes, just the specific diagnostic 'child' codes.
#'
#' The file can be pulled from the zip files on the CMS web site or from within
#' the package.
#' @param version character vector of length one containing the ICD-9 version,
#'   e.g. \code{"32"} which is the default.
#' @template save_data
#' @param path Absolute path in which to save parsed data
#' @template offline
#' @return invisibly return the result
#' @keywords internal datagen
icd9_parse_leaf_desc_ver <- function(version = icd9cm_latest_edition(),
                                     save_data = TRUE,
                                     offline = TRUE) {
  assert_string(version)
  assert_flag(save_data)
  assert_flag(offline)
  message("Fetching billable codes version: ", version)
  if (version == "27")
    return(invisible(parse_leaf_desc_icd9cm_v27(offline = offline)))
  stopifnot(version %in% icd::icd9_sources$version)
  dat <- icd::icd9_sources[icd::icd9_sources$version == version, ]
  fn_short_orig <- dat$short_filename
  fn_long_orig <- dat$long_filename

  f_info_short <- unzip_to_data_raw(dat$url, file_name = fn_short_orig, offline = offline)
  f_info_long <- NULL
  if (!is.na(fn_long_orig))
    f_info_long <- unzip_to_data_raw(dat$url, file_name = fn_long_orig, offline = offline)
  message("short filename = ", f_info_short$file_name, "\n long filename = ", f_info_long$file_name)
  message("short path = ", f_info_short$file_path, "\n long path = ", f_info_long$file_name)
  # yes, specify encoding twice, once to declare the source format, and again
  # to tell R to flag (apparently only where necessary), the destination
  # strings: in our case this is about ten accented character in long
  # descriptions of disease names

  # shortlines should always exist
  shortlines <- readLines(f_info_short$file_path)
  message("got short lines")

  # longlines may not, and may have more complicated encoding
  if (!is.na(fn_long_orig)) {
    file_long <- file(f_info_long$file_path, encoding = "latin1")
    longlines <- readLines(f_info_long$file_path, encoding = "latin1")
    close(file_long)
    message("got long lines")
  } else longlines <- NA_character_

  shortlines <- strsplit(shortlines, "[[:space:]]+")
  longlines <- strsplit(longlines, "[[:space:]]+")
  message("split done")
  # no need to trim: we just split on "space', so there can't be any extra spaces
  short_codes <- vapply(shortlines, FUN = function(x) x[1], FUN.VALUE = character(1))
  short_descs <- vapply(shortlines, FUN = function(x) paste(x[-1], collapse = " "), FUN.VALUE = character(1))
  if (!is.na(longlines[1]))
    long_descs <- trim(vapply(longlines, function(x) paste(x[-1], collapse = " "), FUN.VALUE = character(1)))
  else long_descs <- NA_character_
  message("codes and descs separated")
  out <- data.frame(code = short_codes,
                    short_desc = short_descs,
                    long_desc = long_descs,
                    stringsAsFactors = FALSE)
  message("now sort so that E is after V")
  reorder <- icd9_order_short(out[["code"]])
  stopifnot(!anyNA(out[["code"]]))
  stopifnot(!anyNA(reorder))
  stopifnot(!any(grepl(out[["code"]], pattern = "[[:space:]]")))
  stopifnot(!anyDuplicated(reorder))
  stopifnot(all(seq_len(nrow(out))) %in% reorder)
  # catches a mistaken zero-indexed reorder result
  stopifnot(length(setdiff(seq_len(nrow(out)), reorder)) == 0)
  stopifnot(length(setdiff(reorder, seq_len(nrow(out)))) == 0)
  message("order found")
  out <- out[reorder, ]
  message("reordered")
  oldwarn <- options(warn = 1)
  on.exit(options(oldwarn))
  if (!is.na(fn_long_orig)) {
    encs <- Encoding(out[["long_desc"]])
    message("Found labelled encodings in long_desc: ",
            paste(unique(encs), collapse = ", "))
    message("non-ASCII rows of long descriptions are: ",
            paste(get_non_ASCII(out[["long_desc"]]), collapse = ", "))
    message("Encodings found in long_desc: ",
            unique(
              Encoding(out[["long_desc"]][is_non_ASCII(out[["long_desc"]])])))
  }
  invisible(out)
}

#' Parse billable codes for ICD-9-CM version 27
#'
#' These have a quirk which needs a different approach
#' @template offline
#' @keywords internal datagen
parse_leaf_desc_icd9cm_v27 <- function(offline = TRUE) {
  message("working on version 27 quirk")
  assert_flag(offline)
  v27_dat <- icd::icd9_sources[icd::icd9_sources$version == "27", ]
  fn_orig <- v27_dat$other_filename
  url <- v27_dat$url
  message("original v27 file name = '", fn_orig, "'. URL = ", url)
  f27_info <- unzip_to_data_raw(url, fn_orig, offline = offline)
  f <- file(f27_info$file_path, encoding = "latin1")
  icd9cm_billable27 <-
    utils::read.csv(f27_info$file_path, stringsAsFactors = FALSE,
                    colClasses = "character", encoding = "latin1")
  close(f)
  names(icd9cm_billable27) <- c("code", "long_desc", "short_desc")
  icd9cm_billable27 <- icd9cm_billable27[c(1, 3, 2)] # reorder columns
  reorder <- icd9_order_short(icd9cm_billable27[["code"]])
  invisible(icd9cm_billable27[reorder, ])
}

#' Generate ICD-9-CM hierarchy
#'
#' For each row of billing code, give the chapter, sub-chapter, major code and
#' description, and short and long descriptions. Currently this is specifically
#' for the 2011 ICD-9-CM after which there have been minimal changes.
#' Thankfully, ICD-10-CM has machine readable data available.
#' @template save_data
#' @template verbose
#' @template offline
#' @keywords internal datagen
icd9cm_generate_chapters_hierarchy <- function(save_data = FALSE,
                                               verbose = FALSE, offline = TRUE,
                                               perl = TRUE, use_bytes = TRUE) {
# TODO: Someday add 'billable' column, and make consistent ICD-9 and ICD-10
# lookup tables
  assert_flag(save_data)
  assert_flag(verbose)
  assert_flag(offline)
  icd9_rtf <- rtf_parse_year(year = "2011", perl = perl, useBytes = use_bytes,
                             save_data = FALSE, verbose = verbose,
                             offline = offline)
  chaps <- icd9_get_chapters(x = icd9_rtf$code, short_code = TRUE,
                             verbose = verbose)
  # could also get some long descs from more recent billable lists, but not
  # older ones which only have short descs
  out <- cbind(
    data.frame("code" = icd9_rtf$code,
               "long_desc" = icd9_rtf$desc,
               stringsAsFactors = FALSE),
    # the following can and should be factors:
    chaps)
  # fix congenital abnormalities not having sub-chapter defined: (this might be
  # easier to do when parsing the chapters themselves...)
  out <- fixSubchapterNa(out, "740", "759")
  # and hematopoietic organs
  out <- fixSubchapterNa(out, "280", "289")
  # insert the short descriptions from the billable codes text file. Where there
  # is no short description, e.g. for most Major codes, or intermediate codes,
  # just copy the long description over.
  bill32 <- icd9cm_billable[["32"]]
  billable_codes <- get_billable.icd9(out[["code"]], short_code = TRUE)
  billable_rows <- which(out[["code"]] %in% billable_codes)
  title_rows <- which(out[["code"]] %nin% billable_codes)
  stopifnot(setdiff(c(billable_rows, title_rows),
                    seq_along(out$code)) == integer(0))
  out[billable_rows, "short_desc"] <- bill32$short_desc
  # for rows without a short description (i.e. titles, non-billable),
  # useexisting long desc
  out[title_rows, "short_desc"] <- out[title_rows, "long_desc"]
  # the billable codes list (where available) currently has better long
  # descriptions than the RTF parse. For previous years, there is no long desc
  # in billable, so careful when updating this.
  out[billable_rows, "long_desc"] <- bill32$long_desc
  # now put the short description in the right column position
  out <- out[c("code", "short_desc", "long_desc", "three_digit",
               "major", "sub_chapter", "chapter")]
  out[["short_desc"]] <- enc2utf8(out[["short_desc"]])
  out[["long_desc"]] <- enc2utf8(out[["long_desc"]])
  icd9cm_hierarchy_sanity(out)
  billable <- is_billable.icd9cm(out$code)
  icd9cm_hierarchy <- cbind(out[1], billable, out[-1])
  if (save_data)
    save_in_data_dir(icd9cm_hierarchy)
  invisible(icd9cm_hierarchy)
}

#' Fix NA sub-chapters in RTF parsing
#'
#' Fixes a couple of corner cases in parsing the 2011 ICD-9-CM RTF
#' @keywords internal datagen
fixSubchapterNa <- function(x, start, end) {
  # 740 CONGENITAL ANOMALIES is a chapter with no sub-chapters defined. For
  # consistency, assign the same name to sub-chapters
  congenital <- x[["code"]] %in% (start %i9sa% end)
  # assert all the same:
  stopifnot(all(x[congenital[1], "chapter"] == x[congenital[-1], "chapter"]))
  # insert a new level into the sub-chapter factor in the right place
  previous_sub <- as_char_no_warn(x[(which(congenital) - 1)[1], "sub_chapter"])
  previous_sub_pos <- which(levels(x$sub_chapter) == previous_sub)
  congenital_title <- as_char_no_warn(x[which(congenital)[1], "chapter"])
  new_subs <- as_char_no_warn(x$sub_chapter)
  new_subs[congenital] <- congenital_title
  new_levels <- append(levels(x$sub_chapter), congenital_title, previous_sub_pos)
  x$sub_chapter <- factor(new_subs, new_levels)
  x
}
#nocov end
