#' Unzip file to raw data directory
#'
#' Get a zip file from a URL, extract contents, and save file in the raw data
#' directory. If the file already exists, the path and filename are returned
#' without downloading. If \code{offline} is \code{FALSE}, then \code{NULL} is
#' returned if the file isn't already downloaded.
#'
#' The file name is changed to a conservative cross platform name using
#' \code{make.names}
#'
#' @param url URL of a zip file
#' @param file_name file name of a single file in that zip
#' @template offline
#' @param data_raw_path path where the raw directory is
#' @param save_name file name to save as, default is \code{file_name}
#' @param ... additional arguments passed to \code{utils::download.file}
#' @return path of unzipped file in the raw data directory
#' @keywords internal
#' @noRd
.unzip_to_data_raw <- function(..., # force explicit naming
                               url,
                               file_name,
                               data_raw_path = get_icd_data_dir(),
                               save_name = file_name,
                               dl_msg = NULL) {
  stopifnot(is.character(url), length(url) == 1)
  stopifnot(is.character(file_name), length(file_name) == 1)
  .msg(url)
  if (is.null(data_raw_path)) {
    .absent_action_switch("Data directory not defined.", must_work = TRUE)
    return()
  }
  if (!dir.exists(data_raw_path)) {
    .absent_action_switch(
      paste("Data directory", sQuote(data_raw_path), "doesn't exist"),
      must_work = FALSE
    )
    return()
  }
  file_path <- file.path(data_raw_path, save_name)
  if (any(grepl("tmp", file_path))) stop("TEMP")
  .msg(
    sprintf(
      "file path = %s\nfile name = %s\nsave name = %s",
      file_path, file_name, save_name
    )
  )
  if (!file.exists(file_path)) {
    .msg(
      "Unable to find downloaded file at: ",
      file_path, ". Attempting download..."
    )
    if (!.confirm_download(msg = dl_msg)) {
      return()
    }
    ok <- .unzip_single(
      url = url,
      file_name = file_name,
      save_path = file_path,
      ...
    )
    stopifnot(ok)
  }
  list(file_path = file_path, save_name = save_name)
}

.download_to_data_raw <- function(...) {
  .download(..., data_raw_path = .get_raw_data_path())
}

.download_to_data_cache <- function(...) {
  .download(..., data_raw_path = get_icd_data_dir())
}

.download <-
  function(url,
           data_raw_path,
           file_name = NULL,
           dl_msg = NULL,
           ...) {
    if (is.null(file_name)) {
      file_name <- regmatches(url, regexpr("[^/]*$", url))
    }
    stopifnot(is.character(url), length(url) == 1)
    stopifnot(is.character(file_name), length(file_name) == 1)
    if (is.null(data_raw_path) || !dir.exists(data_raw_path)) {
      .absent_action_switch(
        "icd cache directory not defined or created."
      )
    }
    save_path <- file.path(
      data_raw_path,
      file_name
    )
    f_info <- list(
      file_path = save_path,
      file_name = file_name
    )
    if (!is.null(data_raw_path) &&
      file.exists(save_path)) {
      return(f_info)
    }
    if (.offline()) {
      return()
    }
    if (!.confirm_download()) {
      return()
    }
    if (!is.null(dl_msg)) message(dl_msg)
    curl_res <- try(
      utils::download.file(
        url = url,
        destfile = save_path,
        quiet = TRUE,
        method = "curl",
        extras = "--insecure --silent",
        ...
      )
    )
    if (curl_res != 0) {
      # Windows, maybe some users, do no have curl, maybe not even libcurl. Cannot
      # set libcurl to avoid certificate verification without using RCurl, and I
      # want to avoid another dependency.
      curl_res <- utils::download.file(
        url = url,
        destfile = save_path,
        quiet = TRUE,
        ...
      )
    }
    if (curl_res != 0) stop(paste(url, " not downloaded successfully."))
    f_info
  }
