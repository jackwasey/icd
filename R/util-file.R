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

#' unzip a single file from URL
#'
#' take a single file from zip located at a given URL, unzip into temporary
#' directory, and copy to the given \code{save_path}
#' @keywords internal
unzip_single <- function(url, file_name, save_path) {
  zipfile <- tempfile()
  # using libcurl because it seems the internal method works inconsistently
  if (capabilities("libcurl"))
    method = "libcurl"
  else
    method = "auto"
  dl_code <- utils::download.file(url = url, destfile = zipfile,
                                  quiet = TRUE, method = method)
  stopifnot(dl_code == 0)
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
  unlink(file.path(zipdir, file_name))
}

#' Unzip file to \code{data-raw}
#'
#' Get a zip file from a URL, extract contents, and save file in
#' \code{data-raw}. If the file already exists there, it is only retrieved if
#' \code{force} is set to \code{TRUE}. If \code{offline} is \code{FALSE}, then
#' \code{NULL} is returned if the file isn't already downloaded.
#'
#' The file name is changed to a conservative cross platform name using
#' \code{make.names}
#'
#' @param url url of a zip file
#' @param file_name file name of a single file in that zip
#' @param force logical, if TRUE, then download even if already in
#'   \code{data-raw}
#' @template offline
#' @return path of unzipped file in \code{data-raw}
#' @keywords internal
unzip_to_data_raw <- function(url, file_name, force = FALSE, offline = FALSE) {
  assertString(url)
  if (is.na(file_name))
    return(NULL)
  assertString(file_name)
  assertFlag(force)
  assertFlag(offline)
  stopifnot(!(force & offline))

  data_raw_path <- system.file("data-raw", package = get_pkg_name())
  file_path <- file.path(data_raw_path, make.names(file_name))
  if (force || !file.exists(file_path)) {
    if (offline)
      return(NULL)
    stopifnot(
      unzip_single(url = url, file_name = file_name, save_path = file_path)
    )
  }
  list(file_path = file_path, file_name = make.names(file_name))
}

#' @rdname unzip_to_data_raw
#'
#' @keywords internal
download_to_data_raw <- function(url,
                                 file_name = str_extract(url, "[^/]*$"),
                                 force = FALSE, offline = FALSE) {
  assertString(url)
  assertString(file_name)
  assertFlag(force)
  stopifnot(!(force & offline))

  data_raw_path <- system.file("data-raw", package = get_pkg_name())
  save_path <- file.path(data_raw_path, file_name)

  if (force || !file.exists(save_path))
    if (offline)
      return(NULL)

  # using libcurl because it seems the internal method works inconsistently
  if (capabilities("libcurl"))
    method = "libcurl"
  else
    method = "auto"
  dl_code <- download.file(url = url, destfile = save_path,
                           quiet = TRUE, method = method)
  stopifnot(dl_code == 0)

  list(file_path = save_path, file_name = file_name)

}

#' get name of this package
#'
#' so that I can change to another package name when needed.
#' \code{getPackageName} gives globalenv if running interactively.
#' @keywords internal
get_pkg_name <- function() "icd9"

#' Save given variable in package data directory
#'
#' File is named varname.RData with an optional suffix before .RData
#'
#' @param var_name character or symbol, e.g. "myvar" or \code{myvar}, either of which
#'   would find \code{myvar} in the parent environment, and save it as
#'   \code{myvar.RData} in \code{package_root/data}.
#' @param suffix character scalar
#' @return invisibly returns the data
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
  invisible(get(var_name, envir = parent.frame()))
}

# nocov end
