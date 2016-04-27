# Copyright (C) 2014 - 2016  Jack O. Wasey
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


#' Generate ICD to HCC Crosswalks from CMS
#'
#' The ICD/HCC mappings were obtained from CMS \url{https://www.cms.gov/Medicare/Health-Plans/MedicareAdvtgSpecRateStats/Risk-Adjustors.html}.
#' Due to the complex file structure of the original data (many nested zip files), they have
#' been organized in the folder data/icd_hcc_rawdata/.
#' This function creates an .RData file containing icd9/10 to CC crosswalks.

#' Import the ICD9 and ICD10 CC crosswalks, labels, and hierarchy rules
#' @template parse-template
#' @keywords internal

icd_parse_cc <- function(save_data = FALSE) {
  assert_flag(save_data)

  icd9cc <- apply(data.frame(paste("data/icd_hcc_rawdata/icd9/",list.files("data/icd_hcc_rawdata/icd9/"),sep="")), 1, FUN=read.fwf, width=c(7,4), header=F, stringsAsFactors=F)
  icd10cc <- apply(data.frame(paste("data/icd_hcc_rawdata/icd10/",list.files("data/icd_hcc_rawdata/icd10/"),sep="")), 1, FUN=read.fwf, width=c(7,4), header=F, stringsAsFactors=F)

  # Create a vector of year names based on the file names in the icd folders
  years <- list()
  years$icd9 <- as.numeric(substr(list.files("data/icd_hcc_rawdata/icd9/"), 0,4))
  years$icd10 <- as.numeric(substr(list.files("data/icd_hcc_rawdata/icd10/"), 0,4))

  # assign year to each dataframe within the list of dataframes
  icd9cc <- mapply(cbind, icd9cc, "year" = years$icd9, SIMPLIFY=F)
  icd10cc <- mapply(cbind, icd10cc, "year" = years$icd10, SIMPLIFY=F)

  # Row bind icd9 and icd10 from different years into despective dataframes
  icd9cc <- do.call(rbind, icd9cc)
  icd10cc <- do.call(rbind, icd10cc)

  # Assign ICD version (9 or 10) and combine into single dataframe
  icd9cc$icdversion <- 9
  icd10cc$icdversion <- 10
  icdcc <- rbind(icd9cc, icd10cc)
  rm(icd9cc, icd10cc, years)

  # add variable names
  colnames(icdcc) <- c("icd_code", "cc", "year", "icdversion")

  # Remove whitespace from codes
  icdcc$icd_code <- trimws(icdcc$icd_code)
  icdcc$cc <- trimws(icdcc$cc)

  # Edit ICD9/10s
  # Per CMS instructions, certain ICD9s have to be manually assigned additional CCs

  # icd9 40403, 40413, and 40493 are assigned to CC 80 in 2007-2012
  extracodes <- list()
  extracodes$e1 <- c("40403", "40413", "40493")
  extracodes$e1 <- expand.grid(extracodes$e1, "80", 2007:2012, 9, stringsAsFactors = F)

  # icd9 40401, 40403, 40411, 40413, 40491, 40493 are assigned to CC85 in 2013
  extracodes$e2 <- c("40401","40403","40411","40413","40491","40493")
  extracodes$e2 <- expand.grid(extracodes$e2, "85", 2013, 9, stringsAsFactors = F)

  # icd9 40403, 40413, 40493 are assigned to CC85 in 2014-2015
  extracodes$e3 <- c("40403","40413","40493")
  extracodes$e3 <- expand.grid(extracodes$e3, "85", 2014:2015, 9, stringsAsFactors = F)

  # icd9 3572 and 36202 are assigned to CC18 in 2013
  extracodes$e4 <- c("3572", "36202")
  extracodes$e4 <- expand.grid(extracodes$e4, "18", 2013, 9, stringsAsFactors = F)

  # icd9 36202 is assigned to CC18 in 2014-2015
  extracodes$e5 <- "36202"
  extracodes$e5 <- expand.grid(extracodes$e5, "18", 2014:2015, 9, stringsAsFactors = F)

  # combine into one DF
  extracodes <- do.call(rbind, extracodes)

  # add variable names
  colnames(extracodes) <- c("icd_code", "cc", "year", "icdversion")

  # combine with full icdcc listing
  icd_map_cc <- rbind(icdcc, extracodes)
  rm(icdcc, extracodes)

  if (save_data)
    save_in_data_dir(icd_map_cc)
  invisible(icd_map_cc)
}

#' Import CMS CC Labels
#'
#' The CMS Condition Category labels were obtained from CMS \url{https://www.cms.gov/Medicare/Health-Plans/MedicareAdvtgSpecRateStats/Risk-Adjustors.html}.
#' Due to the complex file structure of the original data (many nested zip files), they have
#' been organized in the folder data/icd_hcc_rawdata/labels.

#' This function creates an .RData file containing labels for CC and HCCs
#' @template parse-template
#' @keywords internal

icd_parse_cc_labels <- function(save_data = FALSE) {
  assert_flag(save_data)
  # Import Labels
  # Import HCC labels from all years
  icd_map_cc_labels <- apply(data.frame(paste("data/icd_hcc_rawdata/labels/",list.files("data/icd_hcc_rawdata/labels/"),sep="")), 1, FUN=readLines)

  # Convert a single dataframe
  icd_map_cc_labels <- lapply(icd_map_cc_labels, as.data.frame, stringsAsFactors=F)
  icd_map_cc_labels <- do.call(rbind, icd_map_cc_labels)

  # Extract HCC numbers
  hccnum <- str_match(icd_map_cc_labels[,1], "HCC([:digit:]*)")[,2]

  # Extract HCC names
  hccname <- substr(icd_map_cc_labels[,1], regexpr("=", icd_map_cc_labels[,1])+2, nchar(icd_map_cc_labels[,1])-1)

  # Combine numbers and names into dataframe of labels
  icd_map_cc_labels <- data.frame(hccnum, hccname, stringsAsFactors = F, row.names = NULL)
  rm(hccnum, hccname)

  # Drop lines with NA/out of range HCC numbers
  icd_map_cc_labels$hccnum <- as.numeric(icd_map_cc_labels$hccnum)
  icd_map_cc_labels <- icd_map_cc_labels[!is.na(icd_map_cc_labels$hccnum),]

  # Drop duplicated HCC numbers
  icd_map_cc_labels <- icd_map_cc_labels[!duplicated(icd_map_cc_labels$hccnum),]

  # Remove whitespace from hccnames
  icd_map_cc_labels$hccname <- trimws(icd_map_cc_labels$hccname)

  # Order in ascending order of HCC number
  icd_map_cc_labels <- icd_map_cc_labels[order(icd_map_cc_labels$hccnum),]

   if (save_data)
    save_in_data_dir(icd_map_cc_labels)
  invisible(icd_map_cc_labels)
}

#' Import CMS HCC Rules
#'
#' The CMS Hierarchical Condition Categories are created by applying a series of rules to the Condition Categories (CC)
#' These rules were obtained from CMS \url{https://www.cms.gov/Medicare/Health-Plans/MedicareAdvtgSpecRateStats/Risk-Adjustors.html}.
#' Due to the complex file structure of the original data (many nested zip files), they have
#' been organized in the folder data/icd_hcc_rawdata/hierarchy.

#' This function creates an .RData file containing rules for converting CCs to HCC
#' @template parse-template
#' @keywords internal

icd_parse_cc_hierarchy <- function(save_data = FALSE) {
  assert_flag(save_data)

  # Define Hierarchy
  # import raw hierarchy files from CMS
  icd_map_cc_hcc <- apply(data.frame(paste("data/icd_hcc_rawdata/hierarchy/",list.files("data/icd_hcc_rawdata/hierarchy/"),sep="")), 1, FUN=readLines)

  # Create a vector of year names based on the file names in the icd folders
  years <- substr(list.files("data/icd_hcc_rawdata/hierarchy/"), 0,4)

  # Add year variable to each dataframe
  icd_map_cc_hcc <- mapply(cbind, icd_map_cc_hcc, "year" = years, SIMPLIFY=F)
  rm(years)

  # convert each item in the list of icd_map_cc_hcc objects into a data.frame and combine into a single DF
  icd_map_cc_hcc <- lapply(icd_map_cc_hcc, as.data.frame, stringsAsFactors=F)
  icd_map_cc_hcc <- do.call(rbind, icd_map_cc_hcc)

  # convert years to numeric
  icd_map_cc_hcc$year <- as.numeric(icd_map_cc_hcc$year)

  # only keep the lines that are logical hierarchy statements (removes comments, empty lines, additional code) and rename variable
  icd_map_cc_ <- icd_map_cc_hcc[grepl("if hcc|%SET0", icd_map_cc_hcc$V1),]
  colnames(icd_map_cc_hcc)[1] <- "condition"

  # Extract the HCC that is used in the if condition statement
  icd_map_cc_hcc$ifcc <- as.numeric(str_extract(icd_map_cc_hcc$condition, "(?<=hcc)([0-9]*)|(?<=CC\\=)([0-9]*)"))

  # Extract the HCCs that should be set to zero if the above condition is met
  todrop <- str_extract(icd_map_cc_hcc$condition,
                        "(?<=i\\=)([:print:]*)(?=;hcc)|(?<=STR\\()([:print:]*)(?= \\)\\);)")

  # convert it to a dataframe and bind it with the original icd_map_cc_ data
  todrop <- as.data.frame(str_split_fixed(todrop, ",", n=10), stringsAsFactors = F)
  # convert to numeric
  todrop <- as.data.frame(lapply(todrop, as.numeric))

  # combine CC requirements with CCs to zero
  icd_map_cc_hcc <- cbind(icd_map_cc_hcc[,c("year", "ifcc")], todrop)
  rm(todrop)

  # remove columns that are completely NA
  # intially, set up hierchy to allow for up to 10 possible conditions, now will remove extra columns
  # In current data, maximum is 6 conditions to zero, however leaving room in case these are expanded in the future
  icd_map_cc_hcc <- icd_map_cc_hcc[,colSums(is.na(icd_map_cc_hcc))<nrow(icd_map_cc_hcc)]

  if (save_data)
    save_in_data_dir(icd_map_cc_hcc)
  invisible(icd_map_cc_hcc)
}
