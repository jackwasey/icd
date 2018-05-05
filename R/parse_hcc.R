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

# nocov start

#' Generate ICD to HCC Crosswalks from CMS
#'
#' The ICD/HCC mappings were obtained from CMS
#' \href{https://www.cms.gov/Medicare/Health-Plans/MedicareAdvtgSpecRateStats/Risk-Adjustors.html}{
#' Medicare Risk Adjustors}. Due to the complex file structure of the original
#' data (many nested zip files), they have been organized in the folder
#' \code{data/icd_hcc_rawdata/} available in the source repository but not the
#' installed package. This function creates a data file containing ICD-9/10 to
#' CC crosswalks.
#' @template parse-template
#' @keywords internal manip
icd9_parse_cc <- function(save_data = FALSE) {
  assert_flag(save_data)
  # Import raw CMS data for ICD-9
  icd9_map_cc <- apply(
    data.frame(paste("inst/data-raw/icd_hcc_rawdata/icd9/",
                     list.files("inst/data-raw/icd_hcc_rawdata/icd9/"), sep = "")),
    1, FUN = read.fwf,
    widths = c(7, 4), header = FALSE, stringsAsFactors = FALSE)

  # Create a vector of year names based on the file names in the icd folders
  years <- list()
  years$icd9 <- as.numeric(
    substr(list.files("inst/data-raw/icd_hcc_rawdata/icd9/"), 0, 4)
  )

  # Assign year to each dataframe within the list of dataframes
  icd9_map_cc <- mapply(
    cbind, icd9_map_cc, "year" = years$icd9, SIMPLIFY = FALSE)
  rm(years)

  # Combine lsit of DFs into a single DF
  icd9_map_cc <- do.call(rbind, icd9_map_cc)

  # Add variable names
  colnames(icd9_map_cc) <- c("icd_code", "cc", "year")

  # Convert CC to numeric format, remove whitespace from ICD codes
  icd9_map_cc$cc <- as.numeric(icd9_map_cc$cc)
  icd9_map_cc$icd_code <- trimws(icd9_map_cc$icd_code)

  # Per CMS instructions, some ICD9s have to be manually assigned additional CCs
  extracodes <- list()
  # icd9 40403, 40413, and 40493 are assigned to CC 80 in 2007-2012
  extracodes$e1 <- c("40403", "40413", "40493")
  extracodes$e1 <- expand.grid(
    extracodes$e1, 80, 2007:2012, stringsAsFactors = FALSE)
  # icd9 40401, 40403, 40411, 40413, 40491, 40493 are assigned to CC85 in 2013
  extracodes$e2 <- c("40401", "40403", "40411", "40413", "40491", "40493")
  extracodes$e2 <- expand.grid(
    extracodes$e2, 85, 2013, stringsAsFactors = FALSE)
  # icd9 40403, 40413, 40493 are assigned to CC85 in 2014-2015
  extracodes$e3 <- c("40403", "40413", "40493")
  extracodes$e3 <- expand.grid(
    extracodes$e3, 85, 2014:2015, stringsAsFactors = FALSE)
  # icd9 3572 and 36202 are assigned to CC18 in 2013
  extracodes$e4 <- c("3572", "36202")
  extracodes$e4 <- expand.grid(
    extracodes$e4, 18, 2013, stringsAsFactors = FALSE)
  # icd9 36202 is assigned to CC18 in 2014-2015
  extracodes$e5 <- "36202"
  extracodes$e5 <- expand.grid(
    extracodes$e5, 18, 2014:2015, stringsAsFactors = FALSE)

  # Combine into one DF
  extracodes <- do.call(rbind, extracodes)

  # Add variable names to match icd9_map_cc
  colnames(extracodes) <- c("icd_code", "cc", "year")

  # combine with full icd9_map_cc listing
  icd9_map_cc <- rbind(icd9_map_cc, extracodes)
  rm(extracodes)

  if (save_data)
    save_in_data_dir(icd9_map_cc)
  invisible(icd9_map_cc)
}

#' Import the ICD10 to CC crosswalks
#' @template parse-template
#' @keywords internal manip
icd10_parse_cc <- function(save_data = FALSE) {
  assert_flag(save_data)

  # Import raw CMS data for ICD9
  icd10_map_cc <- apply(
    data.frame(paste("inst/data-raw/icd_hcc_rawdata/icd10/",
                     list.files("inst/data-raw/icd_hcc_rawdata/icd10/"), sep = "")),
    1, FUN = read.fwf,
    widths = c(7, 4), header = FALSE, stringsAsFactors = FALSE)

  # TODO: do use factors, as there is much duplication in "year" and "cc" which
  # shaves 20% off the memory requirement of the data.frame. This is likely to
  # improve speed downstream since more of the lookup table can be held in cache
  # closer to the CPU. Same for ICD-9 above.

  # Create a vector of year names based on the file names in the icd folders
  years <- list()
  years$icd10 <- as.numeric(
    substr(list.files("inst/data-raw/icd_hcc_rawdata/icd10/"), 0, 4))

  # Assign year to each dataframe within the list of dataframes
  icd10_map_cc <- mapply(
    cbind, icd10_map_cc, "year" = years$icd10, SIMPLIFY = FALSE)
  rm(years)

  # Combine lsit of DFs into a single DF
  icd10_map_cc <- do.call(rbind, icd10_map_cc)

  # Add variable names
  colnames(icd10_map_cc) <- c("icd_code", "cc", "year")

  # Convert CC to numeric format, remove whitespace from ICD codes
  icd10_map_cc$cc <- as.numeric(icd10_map_cc$cc)
  icd10_map_cc$icd_code <- trimws(icd10_map_cc$icd_code)

  # Per CMS instructions, some ICDs may to be manually assigned additional CCs
  # Currently, no rules exist for ICD10, but if they need to be added,
  # can adapt the code from icd9_map_cc()

  if (save_data)
    save_in_data_dir(icd10_map_cc)
  invisible(icd10_map_cc)
}

#' Import CMS HCC Rules
#'
#' The CMS Hierarchical Condition Categories are created by applying a series of
#' rules to the Condition Categories (CC) These rules were obtained from CMS
#' \href{https://www.cms.gov/Medicare/Health-Plans/MedicareAdvtgSpecRateStats/Risk-Adjustors.html}{
#' Medicare Risk Adjustors}. Due to the complex file structure of the original
#' data (many nested zip files), they have been organized manually in the folder
#' \code{data/icd_hcc_rawdata/hierarchy}.
#'
#' This function creates an \code{.RData} file containing rules for converting
#' CCs to HCC
#' @template parse-template
#' @keywords internal manip
icd_parse_cc_hierarchy <- function(save_data = FALSE) {
  assert_flag(save_data)

  # Define Hierarchy
  # import raw hierarchy files from CMS
  hierarchy_path <- system.file("data-raw", "icd_hcc_rawdata", "hierarchy",
                                package = "icd")
  hierarchy_files <- list.files(hierarchy_path)
  hierarchy_file_paths <- list.files(hierarchy_path, full.names = TRUE)
  icd_map_cc_hcc <- lapply(hierarchy_file_paths, FUN = readLines)

  # Create a vector of year names based on the file names in the icd folders
  years <- substr(hierarchy_files, 0, 4)

  # Add year variable to each dataframe
  icd_map_cc_hcc <- mapply(cbind, icd_map_cc_hcc, "year" = years, SIMPLIFY = FALSE)

  # Convert each item in the list of 'icd_map_cc_hcc' objects into a dataframe
  # and combine into a single DF
  icd_map_cc_hcc <- lapply(icd_map_cc_hcc, as.data.frame, stringsAsFactors = FALSE)
  icd_map_cc_hcc <- do.call(rbind, icd_map_cc_hcc)

  # convert years to numeric
  #
  # TODO: consider keeping as string: suppose there are
  # two releases in one year, e.g. 2017 and 2017-b
  icd_map_cc_hcc$year <- as.numeric(icd_map_cc_hcc$year)

  # only keep the lines that are logical hierarchy statements
  # removes comments, empty lines, additional code and rename variables
  icd_map_cc_hcc <- icd_map_cc_hcc[grepl("if hcc|%SET0", icd_map_cc_hcc[[1]]), ]
  colnames(icd_map_cc_hcc)[1] <- "condition"

  # Extract the HCC that is used in the if condition statement
  icd_map_cc_hcc[["ifcc"]] <- as.numeric(
    str_extract(icd_map_cc_hcc$condition, "(?<=hcc)([0-9]*)|(?<=CC\\=)([0-9]*)")
  )

  # Extract the HCCs that should be set to zero if the above condition is met
  todrop <- str_extract(
    icd_map_cc_hcc$condition,
    "(?<=i\\=)([:print:]*)(?=;hcc)|(?<=STR\\()([:print:]*)(?= \\)\\);)"
  )

  # convert it to a dataframe and bind it with the original icd_map_cc_ data
  # identify the maximum number of CC hierarchy rules
  longest <- max(vapply(strsplit(todrop, " ,"), length, integer(1))) + 1
  # split list of CCs, which are separated by comma, and convert into a dataframe
  todrop <- strsplit(todrop, ",")
  todrop <- do.call(rbind, lapply(todrop, function(x) {
    length(x) <- longest
    x
  }))

  # convert to numeric (warnings for "" to NA)
  todrop <- suppressWarnings(
    as.data.frame(apply(todrop, 2, as.numeric), stringsAsFactors = FALSE)
  )

  # combine CC requirements with CCs to zero
  icd_map_cc_hcc <- cbind(icd_map_cc_hcc[, c("year", "ifcc")], todrop)

  if (save_data)
    save_in_data_dir(icd_map_cc_hcc)
  invisible(icd_map_cc_hcc)
}

# nocov end
