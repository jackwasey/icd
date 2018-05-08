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

#' generate uranium pathology data
#'
#' This is downloaded from \href{https://wsu.edu}{WSU} where it appears to be
#' provided in the public domain. It requires the file to be downloaded from
#' source, or already stored in the raw data directory, and it appears that the
#' function \code{odbcConnectAccess2007} is only available in the Windows build
#' of \pkg{RODBC}.
#' @template parse-template
#' @source \url{https://ustur.wsu.edu/about-us/}
#' @keywords internal datagen
generate_uranium_pathology <- function(save_data = TRUE, offline = TRUE) {
  requireNamespace("RODBC")
  stopifnot(length(utils::find("odbcConnectAccess2007")) > 0)
  assert_flag(save_data)
  assert_flag(offline)
  file_path <- file.path(icd:::get_raw_data_dir(), "Pathology_Office2007.accdb", package = "icd")
  # odbcConnectAccess2007 is only in the Windows version of RODBC
  channel <- RODBC::odbcConnectAccess2007(file_path)
  uranium_pathology <- RODBC::sqlFetch(channel, "qry_ICD-10")
  uranium_pathology <- uranium_pathology[, c("Case_No", "ICD-10_code")]
  names(uranium_pathology) <- c("case", "icd10")
  uranium_pathology <- uranium_pathology[order(uranium_pathology["case"]), ]
  uranium_pathology$icd10 <- as.decimal_diag(icd10(uranium_pathology$icd10))
  row.names(uranium_pathology) <- 1:nrow(uranium_pathology)
  uranium_pathology <- as.icd_long_data(uranium_pathology)
  if (save_data)
    save_in_data_dir(uranium_pathology)
  invisible(uranium_pathology)
}

generate_vermont_dx(save_data = TRUE, offline = FALSE)
