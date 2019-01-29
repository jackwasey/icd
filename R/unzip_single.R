# nocov start

#' unzip a single file from URL
#'
#' take a single file from zip located at a given URL, unzip into temporary
#' directory, and copy to the given \code{save_path}
#' @param url URL of a zip file
#' @param file_name file name of the resource within the zip file
#' @param save_path file path to save the first file from the zip
#' @keywords internal
#' @export
unzip_single <- function(url, file_name, save_path) {
  assert_character(url, len = 1)
  assert_character(file_name, len = 1)
  assert_character(save_path, len = 1)
  zipfile <- tempfile()
  dl_code <- utils::download.file(url = url, destfile = zipfile,
                                  quiet = TRUE, method = "libcurl", mode = "wb")
  stopifnot(dl_code == 0)
  zipdir <- tempfile() # i do want tempfile, so I get an empty new directory
  dir.create(zipdir)
  utils::unzip(zipfile, exdir = zipdir)  # files="" so extract all
  files <- list.files(zipdir)
  if (missing(file_name)) {
    if (length(files) == 1) {
      file_name <- files
    } else {
      stop("multiple files in zip, but no file name specified: ",
           paste(files, collapse = ", "))
    }
  } else {
    if (!file_name %in% files) {
      message(files, file_name)
      stop(paste(file_name, " not found in ", paste(files, collapse = ", ")))
    }
  }
  ret <- file.copy(file.path(zipdir, file_name), save_path, overwrite = TRUE)
  unlink(zipdir, recursive = TRUE)
  ret
}
# nocov end
