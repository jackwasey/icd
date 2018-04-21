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

#' Download ahrq-ccs-icd9 definition
#' @template ccs-single
#' @template offline
#' @keywords internal
icd9_fetch_ahrq_ccs <- function(single = TRUE, offline) {
  assert_flag(single)
  assert_flag(offline)
  if (single)
    unzip_to_data_raw(
      url = "https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip",
      file_name = "$dxref 2015.csv",
      offline = offline)
  else
    unzip_to_data_raw(
      url = "https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip",
      file_name = "ccs_multi_dx_tool_2015.csv",
      offline = offline)
}

#' Download ahrq-css-icd10 definition
#' @template offline
#' @keywords internal
icd10_fetch_ahrq_ccs <- function(version = "2018.1", offline) {
  assert_character(version, pattern = "^20[0-9]{2}\\.[1-9]$")
  assert_flag(offline)
  version <- gsub(".", "_", version, fixed = TRUE)
  # all information in one file, no need for single vs multi
  unzip_to_data_raw(
    url = paste0("https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_dx_icd10cm_", version, ".zip"),
    file_name = paste0("ccs_dx_icd10cm_", version, ".csv"),
    offline = offline)
}

#' parse AHRQ CCS for mapping
#'
#' Data is downloaded from AHRQ website. ICD9 codes were frozen so no updates
#' are needed. CCS codes are available in a Multi level format and a Single
#' level format. Single level is most common for risk adjustment (Ex. CMS
#' Readmission metric)
#' @template ccs-single
#' @param save_data logical whether to save the result in the source tree.
#'   Defaults to \code{FALSE}.
#' @template offline
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#'   icd:::icd9_parse_ahrq_ccs(single = TRUE, save_data = FALSE, offline = FALSE)
#'   icd:::icd9_parse_ahrq_ccs(single = TRUE, save_data = FALSE, offline = TRUE)
#'   icd:::icd9_parse_ahrq_ccs(single = FALSE, save_data = FALSE, offline = FALSE)
#'   icd:::icd9_parse_ahrq_ccs(single = FALSE, save_data = FALSE, offline = TRUE)
#' }
#' @keywords internal manip
icd9_parse_ahrq_ccs <- function(single = TRUE, save_data = FALSE, offline = TRUE) {
  assert_flag(single)
  assert_flag(save_data)
  assert_flag(offline)

  ahrq_ccs <- icd9_fetch_ahrq_ccs(single = single, offline = offline)

  clean_icd9 <- function(x){
    x %>%
      trimws() %>%
      as.icd9() %>%
      as.short_diag()
  }

  resort_lvls <- function(x) {
    # Function to reorder numbers of CCS
    lvls_names <- names(x)
    lvls_has_empty <- any(lvls_names == " ")
    lvls <- lvls_names[lvls_names != " "]
    lvls <- strsplit(lvls, ".", fixed = TRUE)
    number_splits <- length(lvls[[1]])
    lvls <- lvls %>%
      unlist %>%
      as.numeric %>%
      matrix(ncol = number_splits, byrow = TRUE, dimnames  = list(rownames = lvls_names[lvls_names != " "]))

    # complicated call needed or order using all columns of matrix
    lvls <- lvls[do.call(order, as.data.frame(lvls)), ]
    if (is.null(dim(lvls)))
      lvls <- names(lvls) # if only looking at lvl1, then this becomes a vector, not a matrix
    else
      lvls <- rownames(lvls)

    if (lvls_has_empty) lvls <- c(lvls, " ")
    x[lvls]
  }

  if (!single) {
    ahrq_df <- read.csv(ahrq_ccs$file_path, quote = "'\"", colClasses = "character")
    lvl1 <- tapply(ahrq_df[["ICD.9.CM.CODE"]], ahrq_df[["CCS.LVL.1"]], clean_icd9) %>%
      resort_lvls %>%
      comorbidity_map

    lvl2 <- tapply(ahrq_df[["ICD.9.CM.CODE"]], ahrq_df[["CCS.LVL.2"]], clean_icd9) %>%
      resort_lvls %>%
      comorbidity_map

    lvl3 <- tapply(ahrq_df[["ICD.9.CM.CODE"]], ahrq_df[["CCS.LVL.3"]], clean_icd9) %>%
      resort_lvls %>%
      comorbidity_map

    lvl4 <- tapply(ahrq_df[["ICD.9.CM.CODE"]], ahrq_df[["CCS.LVL.4"]], clean_icd9) %>%
      resort_lvls %>%
      comorbidity_map

    icd9_map_multi_ccs <- list(lvl1 = lvl1, lvl2 = lvl2, lvl3 = lvl3, lvl4 = lvl4)

    if (save_data)
      save_in_data_dir("icd9_map_multi_ccs")
    out <- icd9_map_multi_ccs
  } else {
    ahrq_df <- read.csv(ahrq_ccs$file_path, quote = "'\"", colClasses = "character", skip = 1)
    icd9_map_single_ccs <-
      tapply(ahrq_df[["ICD.9.CM.CODE"]], trimws(ahrq_df$CCS.CATEGORY), clean_icd9) %>%
      resort_lvls %>%
      comorbidity_map

    if (save_data)
      save_in_data_dir("icd9_map_single_ccs")
    out <- icd9_map_single_ccs

  }
  invisible(out)
}

#' parse AHRQ CCS for mapping - ICD10
#'
#' Data is downloaded from AHRQ website. ICD10 codes are continually being
#' updated so a paramter for \code{version} is provided. This parameter should
#' mimic those found in
#' \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs10.jsp#archive}.
#' These are in the format of \code{YYYY.1}, \code{YYYY.2} etc.
#' @param version string in format like "2018.1" where 1 is the version number
#'   as shown on the website
#' @param save_data logical whether to save the result in the source tree.
#'   Defaults to \code{FALSE}.
#' @template offline
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#'   icd:::icd10_parse_ahrq_ccs(version = "2018.1",
#'       save_data = FALSE, offline = FALSE)
#'   icd:::icd10_parse_ahrq_ccs(version = "2018.1",
#'       save_data = FALSE, offline = TRUE)
#' }
#' @keywords internal manip
icd10_parse_ahrq_ccs <- function(version = "2018.1",
                                 save_data = FALSE,
                                 offline = TRUE) {
  assert_character(version, pattern = "^20[0-9]{2}\\.[1-9]$")
  assert_flag(save_data)
  assert_flag(offline)
  ahrq_ccs <-
    icd10_fetch_ahrq_ccs(version = version, offline = offline)
  # simpler structure than icd9, all categories in one file
  ahrq_df <- read.csv(ahrq_ccs$file_path,
                      quote = "'\"",
                      colClasses = "character")

  clean_icd10 <- function(x) {
    x %>%
      trimws() %>%
      as.icd10()
  }

  resort_lvls <- function(x) {
    # Function to reorder numbers of CCS
    lvls_names <- names(x)
    lvls_has_empty <- any(lvls_names == " ")
    lvls <- lvls_names[lvls_names != " "]
    lvls <- strsplit(lvls, ".", fixed = TRUE)
    number_splits <- length(lvls[[1]])
    lvls <- lvls %>%
      unlist %>%
      as.numeric %>%
      matrix(
        ncol = number_splits,
        byrow = TRUE,
        dimnames  = list(rownames = lvls_names[lvls_names != " "])
      )
    # complicated call needed or order using all columns of matrix
    lvls <- lvls[do.call(order, as.data.frame(lvls)), ]
    if (is.null(dim(lvls)))
      lvls <-
      names(lvls) # if only looking at lvl1, then this becomes a vector, not a matrix
    else
      lvls <- rownames(lvls)
    if (lvls_has_empty)
      lvls <- c(lvls, " ")
    x[lvls]
  }

  ccs_lvl_map <- function(col_name) {
    tapply(ahrq_df[["ICD.10.CM.CODE"]], ahrq_df[[col_name]], clean_icd10) %>%
      resort_lvls %>%
      comorbidity_map
  }

  #list to define which columns match to which definition
  icd10_map_def <- list(single = "CCS.CATEGORY",
                        lvl1 = "MULTI.CCS.LVL.1",
                        lvl2 = "MULTI.CCS.LVL.2")
  # Because data is in one file, only have one mapping file to save
  icd10_map_ccs <- lapply(icd10_map_def, ccs_lvl_map)
  if (save_data)
    save_in_data_dir("icd10_map_ccs")
  invisible(icd10_map_ccs)
}
