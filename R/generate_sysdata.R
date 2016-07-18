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

#' Generate \code{sysdata.rda}
#'
#' Generate correctly ordered look-up tables of numeric-only, V and E codes.
#' This is quick, but much too slow when it appears many times in a loop.
#' @template save_data
#' @return invisibly returns the data as a list
#' @keywords internal
generate_sysdata <- function(save_data = TRUE) {

  path <- file.path("R", "sysdata.rda")

  c() -> icd9NShort -> icd9VShort -> icd9EShort
  for (i in as.character(1:999))
    icd9NShort <- c(icd9NShort, sort(icd_children.icd9(i, short_code = TRUE, defined = FALSE)))
  for (i in as.character(0:99))
    icd9VShort <- c(icd9VShort, sort(icd_children.icd9(paste0("V", i), short_code = TRUE, defined = FALSE)))
  for (i in as.character(0:999))
    icd9EShort <- c(icd9EShort, sort(icd_children.icd9(paste0("E", i), short_code = TRUE, defined = FALSE)))

  # we can either use the icd_is_defined functions on these lists, or just grep the
  # canonical list directly to get the numeric, V and E codes.
  icd9NShortReal <- grep("^[^VE]+", icd::icd9cm_hierarchy[["code"]], value = TRUE) # nolint
  icd9VShortReal <- grep("V", icd::icd9cm_hierarchy[["code"]], value = TRUE) # nolint
  icd9EShortReal <- grep("E", icd::icd9cm_hierarchy[["code"]], value = TRUE) # nolint

  # also consider doing this in the ranging functions, even though slower, so
  # version can be chosen each time.
  icd9NShortBillable <- icd9cm_get_billable(icd9NShortReal, short_code = TRUE, icd9cm_edition = "32") # nolint
  icd9VShortBillable <- icd9cm_get_billable(icd9VShortReal, short_code = TRUE, icd9cm_edition = "32") # nolint
  icd9EShortBillable <- icd9cm_get_billable(icd9EShortReal, short_code = TRUE, icd9cm_edition = "32") # nolint

  # some very quick sanity checks: (duplicate in a test in test-ranges.R)
  stopifnot(length(icd9NShortBillable) < length(icd9NShortReal))
  stopifnot(length(icd9VShortBillable) < length(icd9VShortReal))
  stopifnot(length(icd9EShortBillable) < length(icd9EShortReal))
  stopifnot(length(icd9NShortReal) < length(icd9NShort))
  stopifnot(length(icd9VShortReal) < length(icd9VShort))
  stopifnot(length(icd9EShortReal) < length(icd9EShort))
  stopifnot(all(icd9NShortReal %in% icd9NShort))
  stopifnot(all(icd9VShortReal %in% icd9VShort))
  stopifnot(all(icd9EShortReal %in% icd9EShort))

  # http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
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
    long_encoding = c("latin1", "latin1", "latin1", "latin1", "latin1", "latin1", NA, NA, NA, NA),
    short_encoding = rep_len("ASCII", 10),
    url = c(
      "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/ICD-9-CM-v32-master-descriptions.zip",
      "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/cmsv31-master-descriptions.zip",
      "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/cmsv30_master_descriptions.zip",
      "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/cmsv29_master_descriptions.zip",
      "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/cmsv28_master_descriptions.zip",
      "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/FY2010Diagnosis-ProcedureCodesFullTitles.zip", # nolint
      # but this one is in a different format! only contains short descs:
      # "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v27_icd9.zip",
      "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v26_icd9.zip",
      "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v25_icd9.zip",
      "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v24_icd9.zip",
      "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v23_icd9.zip"),
    rtf_url = c( # FY11,12,13,14 are the same?
      rep_len("http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab12.zip", 4),
      "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/DTAB11.zip",
      "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab10.zip",
      "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab09.zip",
      "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab08.zip",
      "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab07.zip",
      "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab06.zip"),
    rtf_filename = c(
      rep_len("Dtab12.rtf", 4), "DTAB11.RTF", "Dtab10.RTF", "Dtab09.RTF", "Dtab08.RTF",
      "Dtab07.RTF", "Dtab06.rtf"), # there are more, but not with corresponding CMS, back to 1990s
    stringsAsFactors = FALSE
  )

  # minimal data sources validation
  long_fns <- icd9_sources[["long_filename"]]
  short_fns <- icd9_sources[["long_filename"]]
  # make.names is stricter than necessary, but no function to sanitize a file
  # name in R, although R CMD check of course can do it...
  message("non-portable long file names: ",
          paste(long_fns[long_fns != make.names(long_fns)]))
  message("non-portable short file names: ",
          paste(short_fns[short_fns != make.names(short_fns)]))

  .nc <- nchar(icd::icd10cm2016[["code"]])

  # we assume we are in the root of the package directory. Save to sysdata.rda
  # because these are probably not of interest to a user and would clutter an
  # already busy namespace.

  if (save_data)
    save(icd9NShort, icd9VShort, icd9EShort,
         icd9NShortBillable, icd9VShortBillable, icd9EShortBillable,
         icd9NShortReal, icd9VShortReal, icd9EShortReal,
         icd9_sources, .nc, file = path, compress = "xz")

  invisible(named_list(icd9NShort, icd9VShort, icd9EShort,
         icd9NShortBillable, icd9VShortBillable, icd9EShortBillable,
         icd9NShortReal, icd9VShortReal, icd9EShortReal,
         icd9_sources, .nc))
}
