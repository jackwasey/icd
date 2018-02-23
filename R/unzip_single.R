# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of jwutil.
#
# jwutil is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# jwutil is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with jwutil If not, see <http:#www.gnu.org/licenses/>.

#' unzip a single file from URL
#'
#' take a single file from zip located at a given URL, unzip into temporary
#' directory, and copy to the given \code{save_path}
#' @param url URL of a zip file
#' @param file_name file name of the resource within the zip file
#' @param save_path file path to save the first file from the zip
#' @export
unzip_single <- function(url, file_name, save_path) {
  checkmate::assert_character(url, len = 1)
  checkmate::assert_character(file_name, len = 1)
  checkmate::assert_character(save_path, len = 1)
  zipfile <- tempfile()
  # using libcurl because it seems the internal method works inconsistently
  curl_cap <- capabilities("libcurl")
  if (length(curl_cap) > 0 && curl_cap)
    method <- "libcurl"
  else
    method <- "auto"
  dl_code <- utils::download.file(url = url, destfile = zipfile,
                                  quiet = TRUE, method = method, mode = "wb")
  stopifnot(dl_code == 0)
  zipdir <- tempfile() # i do want tempfile, so I get an empty new directory
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

  ret <- file.copy(file.path(zipdir, file_name), save_path, overwrite = TRUE)
  unlink(zipdir, recursive = TRUE)
  ret
}
