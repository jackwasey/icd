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

#' generate \code{vermont_dx} data
#'
#' Process data from \href{healthvermont.gov}{Health Vermont}
#' @template parse-template
#' @keywords internal datagen
generate_vermont_dx <- function(save_data = TRUE) {
  assert_flag(save_data)
  vermont_fp <- file.path(icd:::get_raw_data_dir(), make.names("VTINP13.TXT"))
  vermont_dx <- utils::read.csv(vermont_fp,
                                stringsAsFactors = FALSE,
                                strip.white = TRUE,
                                nrows = 1001)[, c(74, 4, 6, 7, 11, 13:32)]
  vermont_dx <- vermont_dx[1:1000, ]
  age_group <- vermont_dx$intage
  attr(age_group, "class") <- "factor"
  attr(age_group, "levels") <- c("Under 1", "1-17", "18-24",
                                 "25-29", "30-34", "35-39",
                                 "40-44", "45-49", "50-54",
                                 "55-59", "60-64", "65-69",
                                 "70-74", "75 and over",
                                 "Unknown")
  sex <- vermont_dx$sex
  attr(sex, "class") <- "factor"
  attr(sex, "levels") <- c("male", "female", "unknown")
  vermont_dx$intage <- age_group
  vermont_dx$sex <- sex
  # death = 8 (other codes are for various discharge statuses)
  vermont_dx$dstat <- vermont_dx$dstat == 8
  names(vermont_dx)[c(1:5)] <- c("visit_id", "age_group", "sex", "death", "DRG")
  vermont_dx <- as.icd_wide_data(vermont_dx)
  dx_cols <- paste0("DX", 1:20)
  for (dc in dx_cols)
    vermont_dx[[dc]]  <- as.icd9cm(as.short_diag(vermont_dx[[dc]]))
  if (save_data)
    save_in_data_dir(vermont_dx)
  invisible(vermont_dx)
}

generate_uranium_pathology(save_data = TRUE, offline = FALSE)
