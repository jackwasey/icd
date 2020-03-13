#' Get the raw data directory
#'
#' Following Hadley Wickham recommendations in R Packages, this should be in
#' \code{inst/extdata}. \pkg{devtools} overrides \code{system.file}.
#' @keywords internal
#' @noRd
.get_raw_data_path <- function(file_name) {
  message("Using package raw data path, not resource directory.")
  dr <- system.file("data-raw", package = "icd")
  if (missing(file_name)) {
    dr
  } else {
    file.path(dr, file_name)
  }
}

.get_versioned_raw_file_name <- function(base_name, ver) {
  paste0("ver", ver, "_", base_name)
}

#' Save given variable in package data directory
#'
#' File is named \code{varname.rda}
#' @param var_name character name of variable or its symbol either of which
#'   would find \code{myvar} in the parent environment, and save it as
#'   \code{myvar.RData} in \code{package_root/data}.
#' @param compress gzip default because it is fastest to unzip
#' @param envir environment in which to look for the variable to save
#' @return invisibly returns the data
#' @keywords internal
#' @noRd
.save_in_data_dir <- function(var_name,
                              x = NULL,
                              compress = "gzip",
                              envir = parent.frame()) {
  if (!is.character(var_name)) {
    var_name <- as.character(substitute(var_name))
  }
  message("Asked to save ", var_name, " to PACKAGE data dir")
  stopifnot(is.character(var_name))
  if (is.null(x)) {
    stopifnot(exists(var_name, envir = envir))
    x <- get(var_name, envir = envir)
  }
  package_dir <- getwd()
  data_path <- "data"
  assign(x = var_name, value = x)
  .assign(var_name = var_name, value = x)
  out_file <- file.path(
    package_dir,
    data_path,
    paste0(var_name, ".rda")
  )
  # check diff before overwrite:
  oldx <- NULL
  if (file.exists(out_file)) {
    load(file = out_file, envir = (tenv <- new.env(emptyenv())))
    oldx <- tenv[[var_name]]
  }
  if (!identical(x, oldx)) {
    warning("Data for ", sQuote(var_name), " is being updated.")
    old_file <- tempfile(paste0(var_name, ".old"), fileext = "rds")
    new_file <- tempfile(paste0(var_name, ".new"), fileext = "rds")
    message(
      "Saving old to: ", old_file,
      " and new to: ", new_file
    )
    saveRDS(oldx, old_file, version = 2)
    saveRDS(x, new_file, version = 2)
    if (!askYesNo("Proceed?", default = FALSE)) {
      message(
        "Examine differences. Consider:",
        "testthat::compare(x, oldx), ",
        "daff::diff_data, ",
        "compareDF (slow), compare::compare (may need lapply), ",
        "arsenal::comparedf may have details but summary superficial, ",
        "base::setdiff not so helpful"
      )
      if (!askYesNo("Continue saving?", default = FALSE)) {
        stop("Not saved in package data.")
      }
    }
    save(
      list = var_name,
      envir = envir,
      file = out_file,
      compress = compress,
      version = 2
    )
    message("Now reload package to enable updated/new data: ", var_name)
  } else {
    message("No change in data, so not re-saving.")
  }
  invisible(x)
}

.save_in_cache <- function(var_name,
                           x = NULL,
                           envir = parent.frame()) {
  if (!is.character(var_name)) {
    var_name <- as.character(substitute(var_name))
  }
  stopifnot(is.character(var_name))
  if (is.null(x)) {
    stopifnot(exists(var_name, envir = envir))
    x <- get(var_name, envir = envir)
  }
  .assign(var_name, x)
  # gzip is not the best, but fastest decompression
  saveRDS(x,
    .rds_path(var_name),
    compress = "gzip"
  )
  invisible(.get(var_name))
}

#' Unzip a single file from URL
#'
#' take a single file from zip located at a given URL, unzip into temporary
#' directory, and copy to the given \code{save_path}
#' @param url URL of a zip file
#' @param file_name file name of the resource within the zip file
#' @param save_path file path to save the first file from the zip
#' @param insecure Logical value, will disable certificate check which fails on
#'   some platforms for some ICD data from CDC and CMS, probably because of TLS
#'   version or certificate key length issues. Default is \code{TRUE}.
#' @param ... additional arguments passed to \code{utils::download.file}
#' @keywords internal
#' @noRd
.unzip_single <- function(url,
                          file_name,
                          save_path,
                          insecure = TRUE,
                          dl_msg = NULL,
                          ...) {
  stopifnot(is.character(url))
  stopifnot(is.character(file_name))
  stopifnot(is.character(save_path))
  if (file.exists(save_path)) {
    return(TRUE)
  }
  if (!.confirm_download()) {
    return(FALSE)
  }
  zipfile <- tempfile(fileext = ".zip")
  on.exit(unlink(zipfile), add = TRUE)
  extra <- ifelse(insecure, "--insecure --silent", NULL)
  if (.verbose() && !is.null(dl_msg)) message(dl_msg)
  dl_code <- utils::download.file(
    url = url,
    destfile = zipfile,
    quiet = !.verbose(),
    method = "curl",
    extra = extra,
    ...
  )
  stopifnot(dl_code == 0)
  # I do want tempfile, so unzip makes the empty new directory
  zipdir <- tempfile()
  on.exit(unlink(zipdir), add = TRUE)
  dir.create(zipdir)
  optwarn <- options(warn = 10)
  on.exit(options(optwarn), add = FALSE)
  file_paths <- utils::unzip(zipfile, exdir = zipdir)
  options(optwarn)
  if (length(file_paths) == 0) {
    stop("No files found in zip: ", zipfile)
  }
  files <- list.files(zipdir)
  if (length(files) == 0) stop("No files in unzipped directory")
  if (missing(file_name)) {
    if (length(files) == 1) {
      file_name <- files
    } else {
      stop(
        "multiple files in zip, but no file name specified: ",
        paste(files, collapse = ", ")
      )
    }
  } else {
    if (!file_name %in% files) {
      message("files")
      print(files)
      message("file_name")
      print(file_name)
      stop(paste(file_name, " not found in ", paste(files, collapse = ", ")))
    }
  }
  ret <- file.copy(file.path(zipdir, file_name), save_path, overwrite = TRUE)
  unlink(zipdir, recursive = TRUE)
  ret
}

.dir_writable <- function(path) {
  dir.exists(path) && file.access(path, 2) == 0
}
