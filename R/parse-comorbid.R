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

ahrq_htn <- c("HTNCX", "HTNPREG", "OHTNPREG", "HTNWOCHF", "HTNWCHF", "HRENWORF",
              "HRENWRF", "HHRWOHRF", "HHRWCHF", "HHRWRF", "HHRWHRF")
ahrq_chf <- c("CHF", "HTNWCHF", "HHRWCHF", "HHRWHRF")
ahrq_renal <- c("RENLFAIL", "HRENWRF", "HHRWRF", "HHRWHRF")
ahrq_unused <- c("HTNPREG", "OHTNPREG", "HTNWOCHF", "HTNWCHF", "HRENWORF", "HRENWRF",
                 "HHRWOHRF", "HHRWCHF", "HHRWRF", "HHRWHRF")

ahrq_order <- c("CHF", "VALVE", "PULMCIRC", "PERIVASC", "HTN", "HTNCX", "PARA",
                "NEURO", "CHRNLUNG", "DM", "DMCX", "HYPOTHY", "RENLFAIL", "LIVER",
                "ULCER", "AIDS", "LYMPH", "METS", "TUMOR", "ARTH", "COAG", "OBESE",
                "WGHTLOSS", "LYTES", "BLDLOSS", "ANEMDEF", "ALCOHOL", "DRUG",
                "PSYCH", "DEPRESS")

ahrq_order_all <- c("CHF", "VALVE", "PULMCIRC", "PERIVASC", "HTN", "HTNCX", "HTNPREG",
                    "HTNWOCHF", "HTNWCHF", "HRENWORF", "HRENWRF", "HHRWOHRF", "HHRWCHF",
                    "HHRWRF", "HHRWHRF", "OHTNPREG", "PARA", "NEURO", "CHRNLUNG",
                    "DM", "DMCX", "HYPOTHY", "RENLFAIL", "LIVER", "ULCER", "AIDS",
                    "LYMPH", "METS", "TUMOR", "ARTH", "COAG", "OBESE", "WGHTLOSS",
                    "LYTES", "BLDLOSS", "ANEMDEF", "ALCOHOL", "DRUG", "PSYCH", "DEPRESS")

#' get the SAS code from AHRQ
#'
#' Get the SAS code from AHRQ and save in data-raw if not already there.
#' @keywords internal
icd9_fetch_ahrq_sas <- function(offline) {
  download_to_data_raw(
    url = "http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comformat2012-2013.txt",
    offline = offline)
}

icd10_fetch_ahrq_sas <- function(offline) {
  download_to_data_raw(
    url = "http://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comformat_icd10cm_2016.txt",
    offline = offline)
}

#' parse AHRQ SAS code to get mapping
#'
#' Raw data taken directly from the AHRQ web site and parsed. It is then saved
#' in the development tree data directory, so this is an internal function, used
#' in generating the package itself.
#' @template parse-template
#' @keywords internal
icd9_parse_ahrq_sas <- function(save_data = FALSE) {
  assert_flag(save_data)

  # readLines make assumptions or guess about encoding, consider using
  # Hadleyverse for this in future
  ahrq_info <- icd9_fetch_ahrq_sas(offline = TRUE)

  ahrq_sas_lines <- readLines(ahrq_info$file_path)
  icd9_map_ahrq_working <- sas_format_extract_rcomfmt(ahrq_sas_lines)
  icd9_map_ahrq <- list()

  for (cmb in names(icd9_map_ahrq_working)) {
    message("parsing AHRQ SAS codes for '", cmb, "'")
    some_pairs <- strsplit(x = icd9_map_ahrq_working[[cmb]], split = "-")

    # non-range values (and their children) just go on list
    unpaired_items <- sapply(some_pairs, length) == 1
    out <- c()
    if (any(unpaired_items))
      out <- icd_children.icd9(unlist(some_pairs[unpaired_items]), defined = FALSE, short_code = TRUE)

    the_pairs <- some_pairs[lapply(some_pairs, length) == 2]
    out <- c(out, lapply(the_pairs, function(x) sas_expand_range(x[1], x[2])))
    # update icd9_map_ahrq with full range of icd9 codes:
    out %>% unlist %>% unique %>% as.icd9 %>% as.icd_short_diag -> icd9_map_ahrq[[cmb]]
  }

  # drop this superfluous finale which allocates any other ICD-9 code to the
  # "Other" group
  icd9_map_ahrq[[" "]] <- NULL
  icd9_map_ahrq[ahrq_htn] %>% unlist %>% unname %>% as.icd9 %>% as.icd_short_diag -> icd9_map_ahrq[["HTNCX"]]
  icd9_map_ahrq[ahrq_chf] %>% unlist %>% unname %>% as.icd9 %>% as.icd_short_diag -> icd9_map_ahrq[["CHF"]]
  icd9_map_ahrq[ahrq_renal] %>% unlist %>% unname %>% as.icd9 %>% as.icd_short_diag -> icd9_map_ahrq[["RENLFAIL"]]

  icd9_map_ahrq[ahrq_unused] <- NULL

  # officially, AHRQ HTN with complications means that HTN on its own should be
  # unset. however, this is not feasible here, since we just package up the data
  # into a list, and it can be used however the user wishes. It would not be
  # hard to write an AHRQ specific function to do this if needed, but it makes
  # more sense to me

  #   condense to parents, for each parent, if children are all in the list, add
  #   the parent
  for (cmb in names(icd9_map_ahrq)) {
    message("working on ranges for: ", cmb)
    parents <- icd_condense.icd9(icd9_map_ahrq[[cmb]], defined = FALSE,
                                 short_code = TRUE)
    for (p in parents) {
      kids <- icd_children.icd9(p, defined = FALSE, short_code = TRUE)
      kids <- kids[-which(kids == p)] # don't include parent in test
      if (all(kids %in% icd9_map_ahrq[[cmb]]))
        icd9_map_ahrq[[cmb]] <- c(icd9_map_ahrq[[cmb]], p) %>%
        unique %>%
        icd_sort.icd9(short_code = TRUE) %>%
        as.icd9 %>% as.icd_short_diag
    }
  }

  names(icd9_map_ahrq) <- icd::icd_names_ahrq_htn_abbrev
  icd9_map_ahrq %<>% icd_comorbidity_map

  if (save_data)
    save_in_data_dir("icd9_map_ahrq")

  invisible(icd9_map_ahrq)
}

# This is in some ways simpler than that ICD-9 equivalent because I make no
# attempt to find all the child codes.
icd10_parse_ahrq_sas <- function(save_data = FALSE) {
  assert_flag(save_data)

  ahrq_info <- icd10_fetch_ahrq_sas(offline = TRUE)

  ahrq_sas_lines <- readLines(ahrq_info$file_path)
  icd10_map_ahrq <- sas_format_extract_rcomfmt(ahrq_sas_lines)

  icd10_map_ahrq[ahrq_htn] %>% unlist %>% unname -> icd10_map_ahrq[["HTNCX"]]
  icd10_map_ahrq[ahrq_chf] %>% unlist %>% unname -> icd10_map_ahrq[["CHF"]]
  icd10_map_ahrq[ahrq_renal] %>% unlist %>% unname -> icd10_map_ahrq[["RENLFAIL"]]

  icd10_map_ahrq[ahrq_unused] <- NULL

  # put in the same order as the ICD-9 listings (and the publications)
  icd10_map_ahrq <- icd10_map_ahrq[match(ahrq_order, names(icd10_map_ahrq))]

  names(icd10_map_ahrq) <- icd::icd_names_ahrq_htn_abbrev
  icd10_map_ahrq <- lapply(icd10_map_ahrq, as.icd_short_diag)
  icd10_map_ahrq <- lapply(icd10_map_ahrq, as.icd10)
  icd10_map_ahrq %<>% icd_comorbidity_map

  if (save_data)
    save_in_data_dir("icd10_map_ahrq")

  invisible(icd10_map_ahrq)
}

#' @keywords internal
icd9_fetch_quan_deyo_sas <- function(...) {
  download_to_data_raw(
    url =
      "http://mchp-appserv.cpe.umanitoba.ca/concept/ICD9_E_Charlson.sas.txt",
    file_name = "ICD9_E_Charlson.sas", ...)
}

#' parse original SAS code defining Quan's update of Deyo comorbidities.
#'
#' As with \code{parseAhrqSas}, this function reads SAS code, and
#'   in, a very limited way, extracts definitions. In this case the code uses
#'   LET statements, with strings or lists of strings. This saves and invisibly
#'   returns a list with names corresponding to the comorbidities and values as
#'   a vector of 'short' form (i.e. non-decimal) ICD9 codes. Unlike
#'   \code{icd9_parse_ahrq_sas}, there are no ranges defined, so this
#'   interpretation is simpler.
#'
#'   With thanks to Dr. Quan, I have permission to distribute his SAS code.
#'   Previously, the SAS code would be downloaded from the University of
#'   Manitoba at
#'   \url{http://mchp-appserv.cpe.umanitoba.ca/concept/ICD9_E_Charlson.sas.txt}.
#'   There are structural differences between this version and the version
#'   directly from Dr. Quan, however, the parsing results in identical data.
#' @template parse-template
#' @template offline
#' @keywords internal
icd9_parse_quan_deyo_sas <- function(save_data = FALSE) {
  assert_flag(save_data)

  # download the file and/or just get the path or file name, fails if missing
  # by default
  f_info <- icd9_fetch_quan_deyo_sas(offline = TRUE)

  quan_sas_lines <- readLines(f_info$file_path, warn = FALSE)
  let_statements <- sas_extract_let_strings(quan_sas_lines)
  icd9_map_quan_deyo <- let_statements[grepl("DC[[:digit:]]+", names(let_statements))]

  # use validation: takes time, but these are run-once per package creation (and
  # test) tasks.
  icd9_map_quan_deyo <- lapply(icd9_map_quan_deyo, icd_children.icd9,
                               short_code = TRUE, defined = FALSE)

  # do use icd:: to refer to a lazy-loaded dataset which is obscurely within
  # the package, but not in its namespace, or something...
  names(icd9_map_quan_deyo) <- icd::icd_names_charlson_abbrev
  icd9_map_quan_deyo %<>% as.icd_short_diag %>% icd9 %>% icd_comorbidity_map

  if (save_data)
    save_in_data_dir(icd9_map_quan_deyo)
  invisible(icd9_map_quan_deyo)
}

#' Generate ICD to HCC Crosswalks from CMS
#'
#' The ICD/HCC mappings were obtained from CMS \url{https://www.cms.gov/Medicare/Health-Plans/MedicareAdvtgSpecRateStats/Risk-Adjustors.html}.
#' Due to the complex file structure of the original data (many nested zip files), they have
#' been organized in the folder data/icd_hcc_rawdata/.
#' This function creates an .RData file containing icd9/10 to CC crosswalks.

#' Import the ICD9 to CC crosswalks
#' @template parse-template
#' @keywords internal

icd9_parse_cc <- function(save_data = FALSE) {
  assert_flag(save_data)
  # Import raw CMS data for ICD9
  icd9_map_cc <- apply(data.frame(
    paste("inst/data-raw/icd_hcc_rawdata/icd9/", list.files("inst/data-raw/icd_hcc_rawdata/icd9/"), sep = "")), 1,
    FUN = read.fwf, width = c(7, 4), header = F, stringsAsFactors = F)

  # Create a vector of year names based on the file names in the icd folders
  years <- list()
  years$icd9 <- as.numeric(substr(list.files("inst/data-raw/icd_hcc_rawdata/icd9/"), 0, 4))

  # Assign year to each dataframe within the list of dataframes
  icd9_map_cc <- mapply(cbind, icd9_map_cc, "year" = years$icd9, SIMPLIFY = F)
  rm(years)

  # Combine lsit of DFs into a single DF
  icd9_map_cc <- do.call(rbind, icd9_map_cc)

  # Add variable names
  colnames(icd9_map_cc) <- c("icd_code", "cc", "year")

  # Convert CC to numeric format, remove whitespace from ICD codes
  icd9_map_cc$cc <- as.numeric(icd9_map_cc$cc)
  icd9_map_cc$icd_code <- trimws(icd9_map_cc$icd_code)

  # Per CMS instructions, certain ICD9s have to be manually assigned additional CCs
  extracodes <- list()
  # icd9 40403, 40413, and 40493 are assigned to CC 80 in 2007-2012
  extracodes$e1 <- c("40403", "40413", "40493")
  extracodes$e1 <- expand.grid(extracodes$e1, 80, 2007:2012, stringsAsFactors = F)
  # icd9 40401, 40403, 40411, 40413, 40491, 40493 are assigned to CC85 in 2013
  extracodes$e2 <- c("40401", "40403", "40411", "40413", "40491", "40493")
  extracodes$e2 <- expand.grid(extracodes$e2, 85, 2013, stringsAsFactors = F)
  # icd9 40403, 40413, 40493 are assigned to CC85 in 2014-2015
  extracodes$e3 <- c("40403", "40413", "40493")
  extracodes$e3 <- expand.grid(extracodes$e3, 85, 2014:2015, stringsAsFactors = F)
  # icd9 3572 and 36202 are assigned to CC18 in 2013
  extracodes$e4 <- c("3572", "36202")
  extracodes$e4 <- expand.grid(extracodes$e4, 18, 2013, stringsAsFactors = F)
  # icd9 36202 is assigned to CC18 in 2014-2015
  extracodes$e5 <- "36202"
  extracodes$e5 <- expand.grid(extracodes$e5, 18, 2014:2015, stringsAsFactors = F)

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
#' @keywords internal

icd10_parse_cc <- function(save_data = FALSE) {
  assert_flag(save_data)
  # Import raw CMS data for ICD9
  icd10_map_cc <- apply(data.frame(
    paste("inst/data-raw/icd_hcc_rawdata/icd10/", list.files("inst/data-raw/icd_hcc_rawdata/icd10/"), sep = "")), 1,
    FUN = read.fwf, width = c(7, 4), header = F, stringsAsFactors = F)

  # Create a vector of year names based on the file names in the icd folders
  years <- list()
  years$icd10 <- as.numeric(substr(list.files("inst/data-raw/icd_hcc_rawdata/icd10/"), 0, 4))

  # Assign year to each dataframe within the list of dataframes
  icd10_map_cc <- mapply(cbind, icd10_map_cc, "year" = years$icd10, SIMPLIFY = F)
  rm(years)

  # Combine lsit of DFs into a single DF
  icd10_map_cc <- do.call(rbind, icd10_map_cc)

  # Add variable names
  colnames(icd10_map_cc) <- c("icd_code", "cc", "year")

  # Convert CC to numeric format, remove whitespace from ICD codes
  icd10_map_cc$cc <- as.numeric(icd10_map_cc$cc)
  icd10_map_cc$icd_code <- trimws(icd10_map_cc$icd_code)

  # Per CMS instructions, certain ICDs may to be manually assigned additional CCs
  # Currently, no manual rules exist for ICD10, but if they need to be added, can adapt the code from icd9_map_cc()

  if (save_data)
    save_in_data_dir(icd10_map_cc)
  invisible(icd10_map_cc)
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
  icd_map_cc_hcc <- apply(data.frame(paste("data/icd_hcc_rawdata/hierarchy/",list.files("data/icd_hcc_rawdata/hierarchy/"), sep = "")), 1,
    FUN=readLines)

  # Create a vector of year names based on the file names in the icd folders
  years <- substr(list.files("data/icd_hcc_rawdata/hierarchy/"), 0, 4)

  # Add year variable to each dataframe
  icd_map_cc_hcc <- mapply(cbind, icd_map_cc_hcc, "year" = years, SIMPLIFY = F)
  rm(years)

  # convert each item in the list of icd_map_cc_hcc objects into a data.frame and combine into a single DF
  icd_map_cc_hcc <- lapply(icd_map_cc_hcc, as.data.frame, stringsAsFactors = F)
  icd_map_cc_hcc <- do.call(rbind, icd_map_cc_hcc)

  # convert years to numeric
  icd_map_cc_hcc$year <- as.numeric(icd_map_cc_hcc$year)

  # only keep the lines that are logical hierarchy statements (removes comments, empty lines, additional code) and rename variable
  icd_map_cc_hcc <- icd_map_cc_hcc[grepl("if hcc|%SET0", icd_map_cc_hcc$V1),]
  colnames(icd_map_cc_hcc)[1] <- "condition"

  # Extract the HCC that is used in the if condition statement
  icd_map_cc_hcc$ifcc <- as.numeric(str_extract(icd_map_cc_hcc$condition, "(?<=hcc)([0-9]*)|(?<=CC\\=)([0-9]*)"))

  # Extract the HCCs that should be set to zero if the above condition is met
  todrop <- str_extract(icd_map_cc_hcc$condition,
                        "(?<=i\\=)([:print:]*)(?=;hcc)|(?<=STR\\()([:print:]*)(?= \\)\\);)")

  # convert it to a dataframe and bind it with the original icd_map_cc_ data
  todrop <- as.data.frame(str_split_fixed(todrop, ",", n = 10), stringsAsFactors = F)
  # convert to numeric
  todrop <- as.data.frame(lapply(todrop, as.numeric))

  # combine CC requirements with CCs to zero
  icd_map_cc_hcc <- cbind(icd_map_cc_hcc[,c("year", "ifcc")], todrop)
  rm(todrop)

  # Remove columns that are completely NA
  # Intially, we set up hierchy to allow for up to 10 possible conditions
  # In current data, maximum is 6 conditions to zero, however leaving room in case these are expanded in the future
  # Now remove extra columns
  icd_map_cc_hcc <- icd_map_cc_hcc[,colSums(is.na(icd_map_cc_hcc)) < nrow(icd_map_cc_hcc)]

  if (save_data)
    save_in_data_dir(icd_map_cc_hcc)
  invisible(icd_map_cc_hcc)
}