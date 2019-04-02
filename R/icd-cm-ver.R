#' Get or set the annual version of ICD-10-CM to use
#' @template ver
#' @param check_exists \code{TRUE} by default, which forces a check that the
#'   requested version is actually available in this R session.
#' @export
set_icd10cm_active_ver <- function(ver, check_exists = TRUE) {
  old_v <- get_icd10cm_active_ver()
  v <- as.character(ver)
  stopifnot(grepl("^[[:digit:]]{4}$", v))
  v_name <- paste0("icd10cm", v)
  if (check_exists &&
    !exists(v_name, envir = asNamespace("icd"))) {
    stopifnot(v %in% names(.icd10cm_sources))
    stopifnot(.exists_in_ns(v_name))
  }
  options("icd.data.icd10cm_active_ver" = v)
  invisible(old_v)
}

#' @rdname set_icd10cm_active_ver
#' @export
get_icd10cm_active_ver <- function() {
  ver <- getOption("icd.data.icd10cm_active_ver", default = "2019")
  ver <- as.character(ver)
  if (!grepl("^[[:digit:]]+$", ver)) {
    stop(
      "Option \"icd.data.icd10cm_active_ver\" is not valid.\n",
      "Reset it with set_icd10cm_active_ver(\"2019\") ",
      "or other year version."
    )
  }
  ver
}

#' Get the data for a given version (four-digit year) of ICD-10-CM
#'
#' When called without an argument, it returns the currently active version as
#' set by \code{set_icd10cm_active_ver()}
#' @template ver
#' @template verbose
#' @examples
#' \dontrun{
#' get_icd10cm_version("2018")
#' }
#' @export
get_icd10cm_version <- function(ver,
                                verbose = .verbose()) {
  ver <- as.character(ver)
  stopifnot(grepl("^[[:digit:]]{4}$", ver))
  # don't use :: so we don't trigger every active binding at once!
  var_name <- paste0("icd10cm", ver)
  if (verbose) message("Trying package data env first")
  if (.exists(var_name)) {
    return(.get(var_name))
  }
  if (verbose) message("Resorting to normal package data")
  # try double checking it exists for bizarre R CMD check problem
  lazyenv <- asNamespace("icd")$.__NAMESPACE__.$lazydata
  if (exists(var_name, lazyenv)) {
    out <- getExportedValue("icd", var_name)
    .assign(var_name, out)
    out
  } else {
    NULL
  }
}

#' @describeIn get_icd10cm_version Get the currently active version of
#'   ICD-10-CM.
#' @export
get_icd10cm_active <- function(verbose = FALSE) {
  ver <- get_icd10cm_active_ver()
  if (verbose) message("Getting active version: ", ver)
  get_icd10cm_version(ver = ver)
}

#' Get the ICD-10-CM versions available in this package
#' @template pc
#' @param return_year Logical, which, if \code{TRUE}, will result in only a
#'   character vector of year (or year-like version) being returned.
#' @examples
#' # Diagnostic codes:
#' get_icd10cm_available()
#' # Just get the years avaiable for English language procedure codes
#' get_icd10cm_available(pc = TRUE, return_year = TRUE)
#' @export
get_icd10cm_available <- function(pc = FALSE,
                                  return_year = FALSE) {
  stopifnot(is.logical(pc), length(pc) == 1)
  pc_str <- ifelse(pc, "_pc", "")
  res <- as.character(2014:2019)
  if (return_year) {
    res
  } else {
    paste0("icd10cm", res, pc_str)
  }
}

#' Get the data for the latest ICD-10-CM version in this package.
#' @examples
#' a <- get_icd10cm_latest()
#' identical(a, icd10cm2019)
#' @keywords datasets
#' @export
get_icd10cm_latest <- function() {
  icd10cm2019
}

#' Get the 2019 ICD-10-CM codes and descriptions
#'
#' The most recent available data is also available as the package data,
#' currently \code{icd10cm2019}
#' @export
get_icd10cm2016 <- function() {
  icd10cm2016
}

#' Get the 2019 ICD-10-CM codes and descriptions
#'
#' This is also available as the package data \code{icd10cm2016}
#' @export
get_icd10cm2019 <- function() {
  icd10cm2019
}

#' Evaluate code with a particular version of ICD-10-CM
#'
#' Temporarily sets and restores the option \code{icd.data.icd10cm_active_ver}
#' @template ver
#' @param code Code block to execute
#' @export
with_icd10cm_version <- function(ver, code) {
  stopifnot(is.character(ver), length(ver) == 1)
  old <- options("icd.data.icd10cm_active_ver" = ver)
  on.exit(options(old), add = TRUE)
  force(code)
}

#' Internal function used to search and maybe prompt when active binding used.
#'
#' Tries to get from the local environment first, then from resource directory,
#' and failing that, if interactive, prompts user to download and parse.
#' @param interact Control whether functions thinks it is in interactive mode,
#'   for testing.
#' @keywords internal
#' @noRd
.get_icd10cm_ver <- function(ver,
                             dx,
                             interact = .interact()) {
  ver <- as.character(ver)
  stopifnot(grepl("^[[:digit:]]{4}$", ver))
  var_name <- paste0("icd10cm", ver)
  dat_path <- .rds_path(var_name)
  if (exists(var_name, .icd_data_env)) {
    return(get(var_name, .icd_data_env))
  }
  lazyenv <- asNamespace("icd")$.__NAMESPACE__.$lazydata
  if (exists(var_name, lazyenv)) {
    return(get(var_name, lazyenv))
  }
  if (file.exists(dat_path)) {
    dat <- readRDS(dat_path)
    assign(var_name, dat, envir = .icd_data_env)
    return(dat)
  }
  if (dx) {
    dat <- .parse_icd10cm_year(year = ver)
  } else {
    dat <- .icd10cm_parse_cms_pcs_year(ver, verbose = FALSE)
  }
  assign(var_name, dat, envir = .icd_data_env)
  return(dat)
}
