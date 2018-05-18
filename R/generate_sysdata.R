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

#' Generate \code{sysdata.rda}
#'
#' Generate correctly ordered look-up tables of numeric-only, V and E codes.
#' This is quick, but much too slow when it appears many times in a loop.
#' @template save_data
#' @return invisibly returns the data as a list
#' @keywords internal
generate_sysdata <- function(save_data = TRUE) {
  path <- file.path("R", "sysdata.rda")
  icd9_short_n <- icd9_generate_all_n()
  icd9_short_v <- icd9_generate_all_v()
  icd9_short_e <- icd9_generate_all_e()
  # we can either use the is_defined functions on these lists, or just grep the
  # canonical list directly to get the numeric, V and E codes.
  codes <- icd9cm_hierarchy[["code"]]
  icd9_short_n_defined <- vec_to_lookup_pair(grep("^[^VE]+", codes, perl = TRUE, value = TRUE))
  icd9_short_v_defined <- vec_to_lookup_pair(grep("^V", codes, perl = TRUE, value = TRUE))
  icd9_short_e_defined <- vec_to_lookup_pair(grep("^E", codes, perl = TRUE, value = TRUE))
  # also consider doing this in the ranging functions, even though slower, so
  # version can be chosen each time.
  icd9_short_n_leaf <- vec_to_lookup_pair(icd9cm_get_billable(
    icd9_short_n_defined$vec, short_code = TRUE, icd9cm_edition = "32")) # nolint
  icd9_short_v_leaf <- vec_to_lookup_pair(icd9cm_get_billable(
    icd9_short_v_defined$vec, short_code = TRUE, icd9cm_edition = "32")) # nolint
  icd9_short_e_leaf <- vec_to_lookup_pair(icd9cm_get_billable(
    icd9_short_e_defined$vec, short_code = TRUE, icd9cm_edition = "32")) # nolint
  icd9_sources <- icd9_generate_sources()
  # minimal data sources validation
  long_fns <- icd9_sources[["long_filename"]]
  short_fns <- icd9_sources[["long_filename"]]
  # make.names is stricter than necessary, but no function to sanitize a file
  # name in R, although R CMD check of course can do it...
  message("non-portable long file names: ",
          paste(long_fns[long_fns != make.names(long_fns)]))
  message("non-portable short file names: ",
          paste(short_fns[short_fns != make.names(short_fns)]))
  .nc <- nchar(icd10cm2016[["code"]]) # nolint
  # minimal test here just to use variable names to avoid warnings!
  stopifnot(all(!is.na(.nc)))
  stopifnot(length(icd9_short_n$vec) == length(icd9_short_n$env))
  stopifnot(length(icd9_short_v$vec) == length(icd9_short_v$env))
  stopifnot(length(icd9_short_e$vec) == length(icd9_short_e$env))
  stopifnot(length(icd9_short_n_leaf$vec) == length(icd9_short_n_leaf$env))
  stopifnot(length(icd9_short_v_leaf$vec) == length(icd9_short_v_leaf$env))
  stopifnot(length(icd9_short_e_leaf$vec) == length(icd9_short_e_leaf$env))
  icd9_short_e
  icd9_short_n_leaf
  icd9_short_v_leaf
  icd9_short_e_leaf
  sysdata_names <- c("icd9_short_n", "icd9_short_v", "icd9_short_e",
                     "icd9_short_n_defined", "icd9_short_v_defined", "icd9_short_e_defined",
                     "icd9_short_n_leaf", "icd9_short_v_leaf", "icd9_short_e_leaf",
                     "icd9_sources", ".nc")
  # we assume we are in the root of the package directory. Save to sysdata.rda
  # because these are probably not of interest to a user and would clutter an
  # already busy namespace.
  if (save_data)
    save(list = sysdata_names, file = path)
  invisible(mget(sysdata_names))
}

icd9_generate_all_major_n <- function() {
  sprintf("%03d", 1:999)
}

icd9_generate_all_major_v <- function() {
  sprintf("V%02d", 1:99)
}

icd9_generate_all_major_e <- function() {
  sprintf("E%03d", 0:999)
}

icd9_generate_all_n <- function(...) {
  icd9_generate_all_(major_fun = icd9_generate_all_major_n, ...)
}

icd9_generate_all_v <- function(...) {
  icd9_generate_all_(major_fun = icd9_generate_all_major_v, ...)
}

icd9_generate_all_e <- function(...) {
  icd9_generate_all_(major_fun = icd9_generate_all_major_e, ...)
}

#' generate lookup data for each class of ICD-9 code
#'
#' This is a little painful but the data is small enough, with huge speed gains
#' in common operations
#' @return list with two items, the first being an environment, the second being
#'   a vector. The environment has short ICD-9 codes as the names, and the
#'   sequence number as the contents. The vector contains the codes in order.
#' @keywords internal
icd9_generate_all_ <- function(major_fun, short_code = TRUE,
                               env = new.env(hash = TRUE, baseenv())) {
  vec <- character()
  for (i in major_fun()) {
    kids <- children.icd9(i, short_code = short_code, defined = FALSE)
    vec <- c(vec, kids)
  }
  vec_to_env_count(vec, env = env)
  invisible(list(env = env, vec = env_to_vec_flip(env)))
}

#' generate data for finding source data for ICD-9-CM
#' @source \url{http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
#' @keywords internal
icd9_generate_sources <- function(save_data = FALSE) {
  cms_base <- "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/"
  cdc_base <- "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/"
  icd9_sources <- data.frame(
    version = as.character(c(32, 31, 30, 29, 28, 27, 26, 25, 24, 23)),
    f_year = c(as.character(seq(2014, 2005))),
    start_date = c("2014-10-01", "2013-10-01", "2012-10-01", "2011-10-01", "2010-10-01",
                   "2009-10-01", "2008-10-01", "2007-10-01", "2006-10-01", "2005-10-01"),
    long_filename = c(
      "CMS32_DESC_LONG_DX.txt",
      "CMS31_DESC_LONG_DX.txt",
      "CMS30_DESC_LONG_DX 080612.txt",
      "CMS29_DESC_LONG_DX.101111.txt",
      "CMS28_DESC_LONG_DX.txt",
      NA, # see other filename
      NA, # no long descriptions available for these years
      NA,
      NA,
      NA),
    short_filename = c(
      "CMS32_DESC_SHORT_DX.txt",
      "CMS31_DESC_SHORT_DX.txt",
      "CMS30_DESC_SHORT_DX.txt",
      "CMS29_DESC_SHORT_DX.txt",
      "CMS28_DESC_SHORT_DX.txt",
      NA,
      "V26 I-9 Diagnosis.txt", # nolint
      "I9diagnosesV25.txt",
      "I9diagnosis.txt",
      "I9DX_DESC.txt"),
    other_filename = c(NA, NA, NA, NA, NA,
                       "V27LONG_SHORT_DX_110909.csv",
                       # "V27LONG_SHORT_DX_110909u021012.csv" is 'updated' but
                       # hasn't got correctly formatted <3digit codes.
                       NA, NA, NA, NA),
    long_encoding = c("latin1", "latin1", "latin1",
                      "latin1", "latin1", "latin1",
                      NA, NA, NA, NA),
    short_encoding = rep_len("ASCII", 10),
    url = c(
      paste0(cms_base, "ICD-9-CM-v32-master-descriptions.zip"),
      paste0(cms_base, "cmsv31-master-descriptions.zip"),
      paste0(cms_base, "cmsv30_master_descriptions.zip"),
      paste0(cms_base, "cmsv29_master_descriptions.zip"),
      paste0(cms_base, "cmsv28_master_descriptions.zip"),
      paste0(cms_base, "FY2010Diagnosis-ProcedureCodesFullTitles.zip"),
      # but this one is in a different format! only contains short descs:
      # paste0(cms_base, "v27_icd9.zip",
      paste0(cms_base, "v26_icd9.zip"),
      paste0(cms_base, "v25_icd9.zip"),
      paste0(cms_base, "v24_icd9.zip"),
      paste0(cms_base, "v23_icd9.zip")),
    # FY11,12,13,14 are the same?
    rtf_url = c(
      rep_len(paste0(cdc_base, "ICD9-CM/2011/Dtab12.zip"), 4),
      paste0(cdc_base, "ICD9-CM/2010/DTAB11.zip"),
      paste0(cdc_base, "ICD9-CM/2009/Dtab10.zip"),
      paste0(cdc_base, "ICD9-CM/2008/Dtab09.zip"),
      paste0(cdc_base, "ICD9-CM/2007/Dtab08.zip"),
      paste0(cdc_base, "ICD9-CM/2006/Dtab07.zip"),
      paste0(cdc_base, "ICD9-CM/2005/Dtab06.zip")),
    # there are more RTF files, but not with corresponding CMS, back to 1990s
    rtf_filename = c(
      rep_len("Dtab12.rtf", 4),
      "DTAB11.RTF", "Dtab10.RTF", "Dtab09.RTF", "Dtab08.RTF",
      "Dtab07.RTF", "Dtab06.rtf"),
    stringsAsFactors = FALSE
  )
  if (save_data) save_in_data_dir(icd9_sources)
  invisible(icd9_sources)
}

#' Generate list of data source information for ICD-10-CM diagnosis and
#' procedure codes
#'
#' @seealso \url{https://www.cms.gov/Medicare/Coding/ICD10/}
#' @keywords internal
generate_icd10_sources <- function(save_data = TRUE) {
  base_url <- "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/"
  icd10_sources <- list(
    # https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-PCS-Tables-And-Index.zip
    # https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-PCS-Order-File.zip
    "2018" = list(
      base_url = base_url,
      dx_zip = "2018-ICD-10-Code-Descriptions.zip",
      dx_xml_zip = "2018-ICD-10-Code-Tables-Index.zip",
      dx_flat = "icd10cm_codes_2018.txt",
      pcs_zip = "2018-ICD-10-PCS-Order-File.zip",
      pcs_xml_zip = "2018-ICD-10-PCS-Tables-And-Index.zip",
      pcs_flat = "icd10pcs_order_2018.txt"),
    "2017" = list(
      base_url = base_url,
      dx_zip = "2017-ICD10-Code-Descriptions.zip",
      pcs_zip = "2017-PCS-Long-Abbrev-Titles.zip",
      pcs_xml_zip = "2017-PCS-Code-Tables.zip",
      dx_xml_zip = "2017-ICD10-Code-Tables-Index.zip",
      dx_flat = "icd10cm_codes_2017.txt",
      # XML PCS codes: https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2017-PCS-Code-Tables.zip
      # https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2017-PCS-Long-Abbrev-Titles.zip
      pcs_flat = "icd10pcs_order_2017.txt"),
    # https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2016-Code-Descriptions-in-Tabular-Order.zip
    "2016" = list(
      base_url = base_url,
      dx_zip = "2016-Code-Descriptions-in-Tabular-Order.zip",
      dx_xml_zip = "2016-ICD10-Code-Tables-Index.zip",
      dx_flat = "icd10cm_codes_2016.txt",
      pcs_zip = "2016-PCS-Long-Abbrev-Titles.zip",
      pcs_xml_zip = "2016-PCS-Code-Tables.zip",
      pcs_flat = "icd10pcs_order_2016.txt"),
    # https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2015-code-descriptions.zip
    # https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2015-tables-index.zip
    # https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2015-Code_Tables-and-Index.zip
    # https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2015-PCS-long-and-abbreviated-titles.zip
    "2015" = list(
      base_url = base_url,
      dx_zip = "2015-code-descriptions.zip",
      dx_xml_zip = "2015-tables-index.zip",
      dx_flat = "icd10cm_order_2015.txt",
      pcs_zip = "2015-PCS-long-and-abbreviated-titles.zip",
      pcs_xml_zip = "2015-Code_Tables-and-Index.zip",
      pcs_flat = "icd10pcs_order_2015.txt"),
    # https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2014-ICD10-Code-Descriptions.zip
    # https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2014-ICD10-Code-Tables-and-Index.zip
    # https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2014-Code-Tables-and-Index.zip
    # https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2014-PCS-long-and-abbreviated-titles.zip
    "2014" = list(
      base_url = base_url,
      dx_zip = "2014-ICD10-Code-Descriptions.zip",
      dx_xml_zip = "2014-ICD10-Code-Tables-and-Index.zip",
      dx_flat = "icd10cm_order_2014.txt",
      pcs_zip = "2014-PCS-long-and-abbreviated-titles.zip",
      pcs_xml_zip = "2014-Code-Tables-and-Index.zip",
      pcs_flat = "icd10pcs_order_2014.txt")
  )
  if (save_data) save_in_data_dir(icd10_sources)
  invisible(icd10_sources)
}

#' Generate ICD-9 and ICD-10 Chapter names and number ranges, transcribed from
#' the official definitons from WHO and extended by US CMS.
#' @template save_data
#' @keywords internal datasets
generate_icd_chapters <- function(save_data = TRUE) {
  icd9_chapters <- list(
    "Infectious And Parasitic Diseases" = c(start = "001", end = "139"),
    "Neoplasms" = c(start = "140", end = "239"),
    "Endocrine, Nutritional And Metabolic Diseases, And Immunity Disorders" = c(start = "240", end = "279"),
    "Diseases Of The Blood And Blood-Forming Organs" = c(start = "280", end = "289"),
    "Mental Disorders" = c(start = "290", end = "319"),
    "Diseases Of The Nervous System And Sense Organs" = c(start = "320", end = "389"),
    "Diseases Of The Circulatory System" = c(start = "390", end = "459"),
    "Diseases Of The Respiratory System" = c(start = "460", end = "519"),
    "Diseases Of The Digestive System" = c(start = "520", end = "579"),
    "Diseases Of The Genitourinary System" = c(start = "580", end = "629"),
    "Complications Of Pregnancy, Childbirth, And The Puerperium" = c(start = "630", end = "679"),
    "Diseases Of The Skin And Subcutaneous Tissue" = c(start = "680", end = "709"),
    "Diseases Of The Musculoskeletal System And Connective Tissue" = c(start = "710", end = "739"),
    "Congenital Anomalies" = c(start = "740", end = "759"),
    "Certain Conditions Originating In The Perinatal Period" = c(start = "760", end = "779"),
    "Symptoms, Signs, And Ill-Defined Conditions" = c(start = "780", end = "799"),
    "Injury And Poisoning" = c(start = "800", end = "999"),
    "Supplementary Classification Of Factors Influencing Health Status And Contact With Health Services" = c(start = "V01", end = "V99"),
    "Supplementary Classification Of External Causes Of Injury And Poisoning" = c(start = "E000", end = "E999")
  )
  icd10_chapters <- list(
    "Certain infectious and parasitic diseases" = c(start = "A00", end = "B99"),
    "Neoplasms" = c(start = "C00", end = "D49"),
    "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism" = c(start = "D50", end = "D89"),
    "Endocrine, nutritional and metabolic diseases" = c(start = "E00", end = "E89"),
    "Mental, Behavioral and Neurodevelopmental disorders" = c(start = "F01", end = "F99"),
    "Diseases of the nervous system" = c(start = "G00", end = "G99"),
    "Diseases of the eye and adnexa" = c(start = "H00", end = "H59"),
    "Diseases of the ear and mastoid process" = c(start = "H60", end = "H95"),
    "Diseases of the circulatory system" = c(start = "I00", end = "I99"),
    "Diseases of the respiratory system" = c(start = "J00", end = "J99"),
    "Diseases of the digestive system" = c(start = "K00", end = "K95"),
    "Diseases of the skin and subcutaneous tissue" = c(start = "L00", end = "L99"),
    "Diseases of the musculoskeletal system and connective tissue" = c(start = "M00", end = "M99"),
    "Diseases of the genitourinary system" = c(start = "N00", end = "N99"),
    "Pregnancy, childbirth and the puerperium" = c(start = "O00", end = "O9A"),
    "Certain conditions originating in the perinatal period" = c(start = "P00", end = "P96"),
    "Congenital malformations, deformations and chromosomal abnormalities" = c(start = "Q00", end = "Q99"),
    "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = c(start = "R00", end = "R99"),
    "Injury, poisoning and certain other consequences of external causes" = c(start = "S00", end = "T88"),
    "External causes of morbidity" = c(start = "V00", end = "Y99"),
    "Factors influencing health status and contact with health services" = c(start = "Z00", end = "Z99")
  )
  if (save_data) {
    save_in_data_dir(icd9_chapters)
    save_in_data_dir(icd10_chapters)
  }
  invisible(list(icd9_chapters, icd10_chapters))
}
#nocov end
