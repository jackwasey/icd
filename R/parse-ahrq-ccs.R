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

#' Download ahrq-css-icd-09 definition
#'
#' @keywords internal
icd9_fetch_ahrq_ccs <- function(type, offline) {
  if(type == "Single"){
    out <- unzip_to_data_raw(
      url = "https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2015.zip",
      file_name = "$dxref 2015.csv",
      offline = offline)
  }else if(type == "Multi"){
    out <- unzip_to_data_raw(
      url = "https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip",
      file_name = "ccs_multi_dx_tool_2015.csv",
      offline = offline)
  }
  out
}

#' parse AHRQ CCS for mapping
#'
#' Data is downloaded from AHRQ website. ICD9 codes were frozen so no updates are needed. CCS codes are available in a
#' Multi level format and a Single level format. Single level is most common for risk adjustment (Ex. CMS Readmission
#' metric)
#' @param type either "Single" or "Multi" depending on the type of CCS you want to download and parse
#' @template parse-template
#' @keywords internal manip
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' icd9_parse_ahrq_ccs(type = "Single", save_data = TRUE, offline = FALSE)
#' icd9_parse_ahrq_ccs(type = "Multi", save_data = TRUE, offline = FALSE)
#' }
icd9_parse_ahrq_ccs <- function(type = "Single", save_data = FALSE, offline = TRUE) {
  assert_flag(save_data)
  assert_choice(type, c("Single", "Multi"))

  ahrq_ccs <- icd9_fetch_ahrq_ccs(type = type, offline = offline)

  clean_icd9 <- function(x){
    x %>%
      trimws() %>%
      as.icd9() %>%
      as.icd_short_diag()
  }

  resort_lvls <- function(x){
    # Function to reorder numbers of CCS
    lvls_names <- names(x)
    lvls_has_empty = any(lvls_names == " ")
    lvls <- lvls_names[lvls_names != " "]
    lvls <- strsplit(lvls, ".", fixed = TRUE)
    number_splits <- length(lvls[[1]])
    lvls <- lvls %>%
      unlist %>%
      as.numeric %>%
      matrix(ncol = number_splits, byrow = TRUE,dimnames  = list(rownames = lvls_names[lvls_names != " "]))

    # complicated call needed or order using all columns of matrix
    lvls <- lvls[do.call(order, as.data.frame(lvls)),]
    if(is.null(dim(lvls))) {
      lvls <- names(lvls) # if only looking at lvl1, then this becomes a vector, not a matrix
    }
    else{
      lvls <- rownames(lvls)
    }
    if(lvls_has_empty) lvls <- c(lvls, " ")
    x[lvls]
  }

  if(type == "Multi"){
    ahrq_df <- read.csv(ahrq_ccs$file_path, quote = "'\"", colClasses = "character")
    lvl1 <- tapply(ahrq_df$ICD.9.CM.CODE, ahrq_df$CCS.LVL.1, clean_icd9)
    lvl1 %<>% resort_lvls
    lvl1 %<>% icd_comorbidity_map

    lvl2 <- tapply(ahrq_df$ICD.9.CM.CODE, ahrq_df$CCS.LVL.2, clean_icd9)
    lvl2 %<>% resort_lvls
    lvl2 %<>% icd_comorbidity_map

    lvl3 <- tapply(ahrq_df$ICD.9.CM.CODE, ahrq_df$CCS.LVL.3, clean_icd9)
    lvl3 %<>% resort_lvls
    lvl3 %<>% icd_comorbidity_map

    lvl4 <- tapply(ahrq_df$ICD.9.CM.CODE, ahrq_df$CCS.LVL.4, clean_icd9)
    lvl4 %<>% resort_lvls
    lvl4 %<>% icd_comorbidity_map

    icd9_map_multi_ccs <- list(lvl1 = lvl1,
                lvl2 = lvl2,
                lvl3 = lvl3,
                lvl4 = lvl4)

    if (save_data)
      save_in_data_dir("icd9_map_multi_ccs")
    out <- icd9_map_multi_ccs

  }else if(type == "Single"){
    ahrq_df <- read.csv(ahrq_ccs$file_path, quote = "'\"", colClasses = "character", skip = 1)
    single_level <- tapply(ahrq_df$ICD.9.CM.CODE, trimws(ahrq_df$CCS.CATEGORY), clean_icd9)
    single_level %<>% resort_lvls
    single_level %<>% icd_comorbidity_map
    icd9_map_single_ccs <- single_level

    if (save_data)
      save_in_data_dir("icd9_map_single_ccs")
    out <- icd9_map_single_ccs

  }

  invisible(out)

}

