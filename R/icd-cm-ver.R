#' Get or set the annual version of ICD-10-CM to use
#' @template ver
#' @param check_exists \code{TRUE} by default, which forces a check that the
#'   requested version is actually available in this R session.
#' @export
set_icd10cm_active_year <- function(ver, check_exists = TRUE) {
  old_v <- get_icd10cm_active_year()
  v <- as.character(ver)
  stopifnot(grepl("^[[:digit:]]{4}$", v))
  v_name <- paste0("icd10cm", v)
  if (check_exists &&
    !exists(v_name, envir = asNamespace("icd"))) {
    stopifnot(v %in% names(.icd10cm_sources))
    stopifnot(.exists_in_cache(v_name))
  }
  options("icd.data.icd10cm_active_year" = v)
  invisible(old_v)
}

#' @rdname set_icd10cm_active_year
#' @export
get_icd10cm_active_year <- function() {
  ver <- .get_opt("icd10cm_active_year", default = "2019")
  .dbg("getting icd.data.icd10cm_active_year: ", ver)
  ver <- as.character(ver)
  if (!grepl("^[[:digit:]]+$", ver)) {
    stop(
      "Option \"icd.data.icd10cm_active_year\" is not valid.\n",
      "Reset it with set_icd10cm_active_year(\"2019\") ",
      "or other year version."
    )
  }
  ver
}

#' Get the data for a given version (four-digit year) of ICD-10-CM
#'
#' When called without an argument, it returns the currently active version as
#' set by \code{set_icd10cm_active_year()}
#' @template ver
#' @examples
#' \dontrun{
#' get_icd10cm_version("2018")
#' }
#' @export
get_icd10cm_version <- function(ver) {
  # TODO: use .get_anywhere
  stopifnot(length(ver) == 1L)
  ver <- as.character(ver)
  .stopifnot_year(ver)
  # don't use :: so we don't trigger every active binding at once!
  var_name <- .get_icd10cm_name(year = ver, dx = TRUE)
  .msg("Trying package data env first for ", var_name)
  if (.exists_in_cache(var_name)) {
    cached_dat <- .get_from_cache(var_name)
    if (!is.null(cached_dat)) {
      return(cached_dat)
    } else
    if (.verbose()) {
      message(
        "Cached data for ", sQuote(var_name), " is null!",
        " Refreshing..."
      )
    }
  }
  # for 2016 and 2019 (currently), we look in lazy data.
  .msg("Resorting to normal package data")
  # try double checking it exists for bizarre R CMD check problem
  lazyenv <- asNamespace("icd")$.__NAMESPACE__.$lazydata
  if (exists(var_name, lazyenv)) {
    out <- getExportedValue("icd", var_name)
    .assign(var_name, out)
    out
  } else {
    .absent_action_switch(
      paste(
        "ICD-10-CM", ver, " diagnostic codes not found anywhere.",
        "Consider icd::download_all_icd_data() or ask for help."
      ),
    )
  }
}

#' @describeIn get_icd10cm_version Get the currently active version of
#'   ICD-10-CM.
#' @export
get_icd10cm_active <- function() {
  ver <- get_icd10cm_active_year()
  ver_var <- .get_icd10cm_name(ver, TRUE)
  if (.exists_anywhere(ver_var)) {
    return(.get_anywhere(ver_var))
  }
  .msg("Getting active version: ", ver)
  .get_fetcher_fun(ver_var)()
}

#' Get the ICD-10-CM versions available in this package
#' @template dx
#' @param return_year Logical, which, if \code{TRUE}, will result in only a
#'   character vector of year (or year-like version) being returned.
#' @return By default, the names of all the data available, for diagnostic
#'   ICD-10-CM codes, e.g. \code{icd10cm2019}.
#' @examples
#' # Diagnostic codes:
#' get_icd10cm_available()
#' # Just get the years avaiable for ICD-10-CM procedure codes
#' get_icd10cm_available(dx = FALSE, return_year = TRUE)
#' # How to use the data name - most are not package data, due to severe CRAN
#' # package size limitations, so they are retrieved and cached as needed.
#' # The latest ICD-10-CM is included.
#' tail(get_icd10cm_available(), n = 1)
#' @export
get_icd10cm_available <- function(dx = TRUE,
                                  return_year = FALSE) {
  stopifnot(is.logical(dx), length(dx) == 1)
  stopifnot(is.logical(return_year), length(return_year) == 1)
  pc_str <- ifelse(dx, "", "_pc")
  res <- as.character(2014:2019)
  if (return_year) {
    res
  } else {
    paste0("icd10cm", res, pc_str)
  }
}

#' The latest available ICD-10-CM data in this package
#' @details This is an active binding, so is exported explicitly
#' @keywords datasets
#' @examples
#' a <- get_icd10cm_latest()
#' identical(a, icd10cm2019)
#' @export
get_icd10cm_latest <- function() {
  icd10cm2019
}

#' Evaluate code with a particular version of ICD-10-CM
#'
#' Temporarily sets and restores the option \code{icd.data.icd10cm_active_year},
#' analogous to functions in \CRANpkg{withr}.
#' @template ver
#' @param code Code block to execute, may be in braces, or a single statement
#'   without braces.
#' @examples
#' icd:::.show_options()
#' with_icd10cm_version("2014", icd:::.show_options())
#' @export
with_icd10cm_version <- function(ver, code) {
  stopifnot(is.character(ver), length(ver) == 1)
  old <- options("icd.data.icd10cm_active_year" = ver)
  on.exit(options(old), add = TRUE)
  force(code)
}

#' Get a particular annual version of diagnostic or procedure codes
#'
#' Tries to get from the local environment first, then from resource directory,
#' and failing that, if interactive, prompts user to download and parse.
#' @param interact Control whether functions thinks it is in interactive mode,
#'   for testing.
#' @keywords internal
#' @noRd
.get_icd10cm_ver <- function(ver, dx) {
  ver <- as.character(ver)
  stopifnot(grepl("^[[:digit:]]{4}$", ver))
  var_name <- .get_icd10cm_name(ver, dx = dx)
  if (.exists_in_cache(var_name)) {
    return(.get_from_cache(var_name))
  }
  if (dx) {
    dat <- .parse_icd10cm_year(year = ver)
  } else {
    dat <- .icd10cm_parse_cms_pcs_year(ver)
  }
  assign(var_name, dat, envir = .icd_data_env)
  return(dat)
}
