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
  codes <- icd.data::icd9cm_hierarchy[["code"]]
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
  .nc <- nchar(icd.data::icd10cm2016[["code"]]) # nolint
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
    save(list = sysdata_names, file = path, compress = "xz")
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

#nocov end
