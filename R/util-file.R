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

icd10cm_get_xml_file <- function(...) {
  unzip_to_data_raw(
    url = "http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_Full_XML.ZIP",
    file_name = "Tabular.xml", ...)
}

icd10cm_get_flat_file <- function(...) {
  unzip_to_data_raw(
    url = "http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip",
    file_name = "icd10cm_order_2016.txt", ...)
}


#' Fetch ICD-10-CM data from the CMS web site
#'
#' YEAR-ICD10-Code-Descriptions has flat files, YEAR-ICD10-Code-Tables-Index has
#' XML
#' @keywords internal
fetch_icd10cm_all <- function(verbose = FALSE, ...) {
  for (year in as.character(2014:2018)) {
    for (dx in c(TRUE, FALSE)) {
      if (verbose) message("Working on year: ", year, " and dx is ", dx)
      fetch_icd10cm_year(year, dx = dx, verbose = verbose, ...)
    }
  }
}

#' @rdname fetch_icd10cm_all
#' @keywords internal
fetch_icd10cm_year <- function(year = "2018", dx = TRUE,
                               verbose = FALSE, offline = FALSE, ...) {
  checkmate::assert_string(year, na.ok = FALSE, min.chars = 4, null.ok = FALSE)
  checkmate::assert_flag(dx, na.ok = FALSE, null.ok = FALSE)
  stopifnot(year %in% names(icd::icd10_sources))
  if (verbose) message(ifelse(dx, "dx", "pcs"))
  s <- icd::icd10_sources[[year]]
  url <- paste0(s$base_url, s$dx_zip)
  file_name <- s$dx_flat
  if (!dx) {
    if ("pcs_zip" %nin% names(s) || is.na(s$pcs_zip)) {
      if (verbose) message("No PCS flat file zip name.")
      return()
    }
    url <- paste0(s$base_url, s$pcs_zip)
    file_name <- s$pcs_flat
  }
  stopifnot(!is.null(file_name))
  if (is.na(file_name)) {
    if (verbose) message("No PCS file name.")
    return()
  }
  if (verbose) message("url = ", url, " and file_name = ", file_name)
  unzip_to_data_raw(url = url,
                    file_name = file_name,
                    verbose = verbose,
                    offline = offline, ...)
}

#' Fetch ICD-10-CM data from the CMS web site
#'
#' YEAR-ICD10-Code-Descriptions has flat files, YEAR-ICD10-Code-Tables-Index has
#' XML
#' @keywords internal
fetch_icd10cm_year <- function(year = "2018", type = c("dx", "pcs"), ...) {
  s <- icd::icd10_sources[[year]]
  unzip_to_data_raw(
    url = paste0(s$base_url, s$desc_zip),
    file_name = s$dx_flat, ...)
}

#' Get the raw data directory
#'
#' Following Hadley Wickham recommendations
#' in R Packages, should be in inst/extdata. devtools overrides
#' \code{system.file}.
#' @keywords internal
get_raw_data_dir <- function()
  system.file("extdata", package = "icd")

#nocov end
