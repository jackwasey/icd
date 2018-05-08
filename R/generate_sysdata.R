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
  codes <- icd::icd9cm_hierarchy[["code"]]
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
  .nc <- nchar(icd::icd10cm2016[["code"]]) # nolint
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

  if (save_data)
    save_in_data_dir(icd9_sources)

  invisible(icd9_sources)
}

#nocov end
