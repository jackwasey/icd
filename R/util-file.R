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
#' File is named \code{varname.suffix.rda}
#' @param var_name character name of variable or its symbol either of which
#'   would find \code{myvar} in the parent environment, and save it as
#'   \code{myvar.RData} in \code{package_root/data}.
#' @param suffix character scalar
#' @param compress gzip default because it is fastest to unzip
#' @param envir environment in which to look for the variable to save
#' @return invisibly returns the data
#' @keywords internal
#' @noRd
.save_in_data_dir <- function(var_name,
                              suffix = "",
                              compress = "gzip",
                              envir = parent.frame()) {
  warning("Saving to data dir: save to user's resource directory instead?")
  package_dir <- getwd()
  data_path <- "data"
  stopifnot(is.character("suffix"))
  if (!is.character(var_name)) {
    var_name <- as.character(substitute(var_name))
  }
  stopifnot(is.character(var_name))
  stopifnot(exists(var_name, envir = envir))
  save(
    list = var_name,
    envir = envir,
    file = file.path(
      package_dir,
      data_path,
      paste0(var_name, suffix, ".rda")
    ),
    compress = compress
  )
  message("Now reload package to enable updated/new data: ", var_name)
  invisible(get(var_name, envir = envir))
}

.save_in_resource_dir <- function(var_name,
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
#' @param insecure Logical value, wil disable certificate check which fails on
#'   some platforms for some ICD data from CDC and CMS, probably because of TLS
#'   version or certificate key length issues. Default is \code{TRUE}.
#' @template verbose
#' @param ... additional arguments passed to \code{utils::download.file}
#' @keywords internal
#' @noRd
.unzip_single <- function(url,
                          file_name,
                          save_path,
                          insecure = TRUE,
                          verbose = FALSE,
                          dl_msg = NULL,
                          ...) {
  stopifnot(is.character(url))
  stopifnot(is.character(file_name))
  stopifnot(is.character(save_path))
  if (file.exists(save_path)) return(TRUE)
  if (!.confirm_download()) return(FALSE)
  zipfile <- tempfile(fileext = ".zip")
  on.exit(unlink(zipfile), add = TRUE)
  extra <- ifelse(insecure, "--insecure --silent", NULL)
  if (verbose && !is.null(dl_msg)) message(dl_msg)
  dl_code <- utils::download.file(
    url = url,
    destfile = zipfile,
    quiet = !verbose,
    method = "curl",
    extra = extra,
    ...
  )
  stopifnot(dl_code == 0)
  # I do want tempfile, so I get an empty new directory
  zipdir <- tempfile()
  on.exit(unlink(zipdir), add = TRUE)
  dir.create(zipdir)
  file_paths <- utils::unzip(zipfile, exdir = zipdir)
  if (length(file_paths) == 0) stop("No files found in zip")
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
