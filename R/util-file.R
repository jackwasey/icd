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
  .Deprecated("fetch_icd10cm_year")
  unzip_to_data_raw(
    url = "http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_Full_XML.ZIP",
    file_name = "Tabular.xml", ...)
}

icd10cm_get_flat_file <- function(...) {
  .Deprecated("fetch_icd10cm_year")
  unzip_to_data_raw(
    url = "http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip",
    file_name = "icd10cm_order_2016.txt", ...)
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
