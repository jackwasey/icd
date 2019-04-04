# Prefer CMS? NCHS actually generates the ICD-10-CM codes, at least the
# diagnostic ones. http://www.cdc.gov/nchs/data/icd/icd10cm/

#' Get annual version of ICD-10-CM
#' @param year four-digit
#' @template verbose
#' @param ... passed through, e.g., \code{offline = FALSE}
#' @keywords internal
#' @noRd
.icd10cm_get_flat_file <- function(year, verbose = .verbose(), ...) {
  if (verbose) message("Trying to get flat file for year: ", year)
  y <- .icd10cm_sources[[as.character(year)]]
  .unzip_to_data_raw(
    url = paste0(y$base_url, y$dx_zip),
    # dx_leaf is same, just leaves
    file_name = y$dx_hier,
    save_name = .get_versioned_raw_file_name(y$dx_hier, year),
    verbose = verbose,
    ...
  )
}

#' Fetch ICD-10-CM data from the CMS web site
#'
#' YEAR-ICD10-Code-Descriptions has flat files, YEAR-ICD10-Code-Tables-Index has
#' XML
#' @keywords internal
#' @noRd
.dl_icd10cm_all <- function(verbose = FALSE, ...) {
  for (year in names(.icd10cm_sources)) {
    for (dx in c(TRUE, FALSE)) {
      if (verbose) {
        message(
          "Working on year ", year,
          ifelse(dx, "diagnostic", "procedure"),
          "codes"
        )
      }
      .parse_icd10cm_year(year,
        dx = dx,
        verbose = verbose,
        ...
      )
    }
  }
  invisible()
}

#' Downlaod a specific ICD-10-CM version
#' @return file path and name, in list, or NULL if offline or failure
#' @keywords internal
#' @noRd
.dl_icd10cm_year <- function(year,
                             dx,
                             ...) {
  stopifnot(is.numeric(year) || is.character(year), length(year) == 1)
  year <- as.character(year)
  stopifnot(is.logical(dx), length(dx) == 1)
  stopifnot(as.character(year) %in% names(.icd10cm_sources))
  if (.verbose()) {
    message(
      "Downloading or finding ICD-10-CM ",
      ifelse(dx, "dx", "pcs")
    )
  }
  s <- .icd10cm_sources[[year]]
  url <- paste0(s$base_url, s$dx_zip)
  # fox dx codes, get either the hier or just leaf flat file here:
  file_name <- s$dx_hier
  if (!dx) {
    if ("pcs_zip" %nin% names(s) || is.na(s$pcs_zip)) {
      if (.verbose()) message("No PCS flat file zip name.")
      return()
    }
    url <- paste0(s$base_url, s$pcs_zip)
    file_name <- s$pcs_flat
  }
  stopifnot(!is.null(file_name))
  if (is.na(file_name)) {
    if (.verbose()) message("No PCS file name.")
    return()
  }
  save_name <- .get_versioned_raw_file_name(file_name, year)
  if (.verbose()) {
    message(
      "url = ", url,
      "\nfile_name = ", file_name,
      "\nsave_name = ", save_name
    )
  }
  # if (!file.exists(save_name) && !.confirm_download()) return()
  fp <- .unzip_to_data_raw(
    url = url,
    file_name = file_name,
    save_name = save_name,
    dl_msg = "Please wait a moment to download (or use cached) ~1-10MB of data...",
    ...
  )
  fp
}

.make_icd10cm_parse_fun <- function(year, dx, verbose) {
  # can't have the function in the loop otherwise it inherits the environment of
  # the loop, doesn't have it's own unique one?
  #
  # Must force, so that the values to the arguments are not promises which are
  # later evaluated in the loop environment.
  force(year)
  force(dx)
  force(verbose)
  parse_fun <- function() {
    .parse_icd10cm_year(year = year, dx = dx)
  }
  parse_fun_env <- environment(parse_fun)
  parse_fun_env$year <- as.character(year)
  parse_fun_env$dx <- dx
  parse_fun_env$verbose <- verbose
  parse_fun
}

# run in zzz onload
.make_icd10cm_parsers <- function(env = parent.frame(),
                                  verbose = FALSE) {
  for (y in 2014:2019) {
    for (dx in c(TRUE, FALSE)) {
      if (dx && (y %in% c(2016, 2019))) next
      parse_fun_name <- .get_parser_icd10cm_name(y, dx)
      parse_fun <- .make_icd10cm_parse_fun(y, dx, verbose)
      assign(parse_fun_name, parse_fun, envir = env)
    }
  }
}
