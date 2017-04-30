# Copyright (C) 2014 - 2017  Jack O. Wasey
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
#' @param url URL of a zip file
#' @param file_name file name of a single file in that zip
#' @param force logical, if TRUE, then download even if already in
#'   \code{data-raw}
#' @template verbose
#' @template offline
#' @param data_raw_path path where the data-raw directory is.
#' @return path of unzipped file in \code{data-raw}
#' @keywords internal
unzip_to_data_raw <- function(url, file_name, force = FALSE, verbose = FALSE,
                              offline = TRUE, data_raw_path = "data-raw") {
  checkmate::assert_string(url, na.ok = FALSE)
  checkmate::assert_string(file_name, na.ok = FALSE)
  checkmate::assert_flag(offline)

  if (!dir.exists(data_raw_path))
    data_raw_path <- tempdir()

  file_path <- file.path(data_raw_path, make.names(file_name))
  if (verbose) {
    message("file path = ", file_path)
    message("file name = ", file_name)
  }
  if (force || !file.exists(file_path)) {
    if (offline)
      return()
    stopifnot(
      unzip_single(url = url, file_name = file_name, save_path = file_path)
    )
  }
  list(file_path = file_path, file_name = make.names(file_name))
}

#' @rdname unzip_to_data_raw
#' @keywords internal
download_to_data_raw <- function(
  url,
  file_name = regmatches(url, regexpr("[^/]*$", url)),
  offline = TRUE, data_raw_path = "data-raw") {
  checkmate::assert_string(url)
  checkmate::assert_string(file_name)
  checkmate::assert_flag(offline)

  if (!dir.exists(data_raw_path))
    data_raw_path <- tempdir()


  save_path <- file.path(data_raw_path, file_name)
  f_info <- list(file_path = save_path, file_name = file_name)

  if (file.exists(save_path))
    return(f_info)

  if (offline)
    return()

  # consider libcurl, but seems to work without now
  if (utils::download.file(url = url, destfile = save_path, quiet = TRUE) != 0)
    stop(paste(url, " not downloaded successfully."))

  f_info

}
