# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

# nocov start

#' unzip a single file
#' @keywords internal
unzip_single <- function(url, file_name, save_path) {
  zipfile <- tempfile()
  utils::download.file(url = url, destfile = zipfile, quiet = TRUE)
  zipdir <- tempfile()
  dir.create(zipdir)
  utils::unzip(zipfile, exdir = zipdir)  # files="" so extract all
  files <- list.files(zipdir)
  if (is.null(file_name)) {
    if (length(files) == 1) {
      file_name <- files
    } else {
      stop("multiple files in zip, but no file name specified: ",
           paste(files, collapse = ", "))
    }
  } else
    stopifnot(file_name %in% files)

  file.copy(file.path(zipdir, file_name), save_path, overwrite = TRUE)
}

#' Get a zipped file from a URL, or confirm it is in data-raw already
#'
#' @param url url of a zip file
#' @param file_name file name of a single file in that zip
#' @param force logical, if TRUE, then download even if already in
#'   \code{data-raw}
#' @param offline single logical, if \code{TRUE} then only return path and
#'   filename if the file already exists in data-raw. This is helpful for
#'   testing.
#' @return path of unzipped file in \code{data-raw}
#' @keywords internal
unzip_to_data_raw <- function(url, file_name, force = FALSE, offline = FALSE) {
  assertString(url)
  assertString(file_name)
  assertFlag(force)
  assertFlag(offline)
  data_raw_path <- system.file("data-raw", package = get_pkg_name())
  file_path <- file.path(data_raw_path, file_name)
  if (force || !file.exists(file_path))
    if (offline)
      return(list(file_path = NULL, file_name = NULL))
    stopifnot(
      unzip_single(url = url, file_name = file_name, save_path = file_path)
    )
  list(file_path = file_path, file_name = file_name)
}

download_to_data_raw <- function(url, file_name = NULL, force = FALSE) {
  data_raw_path <- system.file("data-raw", package = get_pkg_name())
  if (is.null(file_name))
    file_name <- str_extract(url, "[^/]*$")
  save_path <- file.path(data_raw_path, file_name)

  if (force || !file.exists(save_path))
    stopifnot(!download.file(url = url, destfile = save_path, quiet = TRUE))
  list(file_path = save_path, file_name = file_name)

}

# so that I can change to another package name when needed.
# \code{getPackageName} gives globalenv if running interactively.
get_pkg_name <- function() "icd9"

#' Save given variable in package data directory
#'
#' File is named varname.RData with an optional suffix before .RData
#'
#' @param var_name character or symbol, e.g. "myvar" or \code{myvar}, either of which
#'   would find \code{myvar} in the parent environment, and save it as
#'   \code{myvar.RData} in \code{package_root/data}.
#' @param suffix character scalar
#' @keywords internal
save_in_data_dir <- function(var_name, suffix = "") {
  assertString(suffix)
  var_name <- as.character(substitute(var_name))
  stopifnot(exists(var_name, envir = parent.frame()))
  save(list = var_name,
       envir = parent.frame(),
       file = file.path("data", strip(paste0(var_name, suffix, ".RData"))),
       compress = "xz")
  message("Now reload package to enable updated/new data: ", var_name)
}

# nocov end
