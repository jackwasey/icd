# quick sanity checks - full tests of x in test-parse.R
.icd9cm_hierarchy_sanity <- function(x) {
  stopifnot(
    all(
      icd::is_valid(x[["code"]], short_code = TRUE)
    )
  )
  if (!any(
    vapply(
      x,
      FUN = is.na,
      FUN.VALUE = logical(nrow(x))
    )
  )) {
    return(invisible())
  }
  print(colSums(vapply(x, is.na, logical(nrow(x)))))
  print(x[which(is.na(x$major)), ])
  print(x[which(is.na(x$three_digit)), ])
  print(sum(is.na(x$sub_chapter)))
  print(unique(x[which(is.na(x$sub_chapter)), "three_digit"]))
  print(x[which(is.na(x$sub_chapter))[1:10], ]) # just top ten
  print(x[which(is.na(x$chapter)), ])
  stop("should not have any NA values in the ICD-9-CM flatten hierarchy")
}

#' Get billable codes from all available years
#'
#' For versions 23 to 32, those which are on the CMS web site, get any codes
#' with long or short descriptions. Earlier years only have abbreviated
#' descriptions, not long descriptions.
#' @return data frame with \code{icd9}, \code{short_desc} and \code{long_desc}
#'   columns. \code{NA} is placed in \code{long_desc} when not available.
#' @source
#' http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
#' @keywords internal datagen
#' @noRd
.icd9cm_parse_leaf_descs <- function(...) {
  versions <- .icd9cm_sources$version
  if (.verbose()) {
    message(
      "Available versions of sources are: ",
      paste(versions, collapse = ", ")
    )
  }
  icd9cm_leaf_v32 <- .parse_icd9cm_leaf_year(
    year = "2014",
    ...
  )
  invisible(icd9cm_leaf_v32)
}

.dl_icd9cm_leaf_year <- function(year, ...) {
  year <- as.character(year)
  .msg("Downloading ICD-9-CM leaf/billable year: ", year)
  stopifnot(year %in% .icd9cm_sources$f_year)
  dat <- .icd9cm_sources[.icd9cm_sources$f_year == year, ]
  fn_short_orig <- dat$short_filename
  fn_long_orig <- dat$long_filename
  f_info_long <- NA
  f_info_short <- .unzip_to_data_raw(
    url = dat$url,
    file_name = fn_short_orig,
    save_name = .get_versioned_raw_file_name(fn_short_orig,
      ver = year
    ),
    ...
  )
  if (!is.na(fn_long_orig)) {
    f_info_long <- .unzip_to_data_raw(
      url = dat$url,
      file_name = fn_long_orig,
      save_name = .get_versioned_raw_file_name(fn_long_orig,
        ver = year
      ),
      ...
    )
  }
  list(short = f_info_short, long = f_info_long)
}

#' Read the ICD-9-CM leaf description data as provided by the Center for
#' Medicaid Services (CMS).
#'
#' The full ICD-9 specification is unfortunately only available in an RTF file,
#' but CMS also distributes a space-separated text file with just the
#' definitions for 'leaf' or 'billable' codes. Note that this canonical data
#' doesn't specify non-diagnostic higher-level codes, just the specific
#' diagnostic codes.
#'
#' The RTFs are parsed by this package to produce the icd9cm2011 etc data,
#' formerly called icd9cm_hierarchy.
#' @param version character vector of length one containing the ICD-9 version,
#'   e.g., \code{"32"}.
#' @param path Absolute path in which to save parsed data
#' @param ... Arguments passed to other functions, e.g., \code{offline} for
#'   \code{.unzip_to_data_raw}
#' @return invisibly return the result
#' @keywords internal datagen
#' @noRd
.parse_icd9cm_leaf_year <- function(year, ...) {
  stopifnot(length(year) == 1L)
  year <- as.character(year)
  stopifnot(grepl("^[[:digit:]]{4}$", year))
  .msg("Fetching billable codes version: ", year)
  # 2009 version 27 is exceptionally badly formatted:
  if (year == "2009") {
    return(invisible(.parse_leaf_desc_icd9cm_v27(...)))
  }
  stopifnot(year %in% .icd9cm_sources$f_year)
  f_info <- .dl_icd9cm_leaf_year(year = year, ...)
  # Below, yes, specify encoding twice, once to declare the source format, and
  # again to tell R to flag (apparently only where necessary), the destination
  # strings: in our case this is about ten accented character in long
  # descriptions of disease names
  #
  # shortlines should always exist
  shortlines <- readLines(f_info$short$file_path)
  # longlines may not, and may have more complicated encoding
  if (!is.na(f_info["long"])) {
    longlines <- readLines(f_info$long$file_path, encoding = "latin1")
  } else {
    longlines <- NA_character_
  }
  shortlines <- strsplit(shortlines, "[[:space:]]+")
  longlines <- strsplit(longlines, "[[:space:]]+")
  # no need to trim: we just split on "space', so no extra spaces
  short_codes <- vapply(shortlines,
    FUN = function(x) x[1],
    FUN.VALUE = character(1)
  )
  short_descs <- vapply(shortlines,
    FUN = function(x) paste(x[-1], collapse = " "),
    FUN.VALUE = character(1)
  )
  long_descs <- NA_character_
  if (!is.na(longlines[1])) {
    long_descs <- trimws(
      vapply(longlines,
        FUN = function(x) paste(x[-1], collapse = " "),
        FUN.VALUE = character(1)
      )
    )
  }
  .msg("codes and descs separated")
  out <- data.frame(
    code = short_codes,
    short_desc = short_descs,
    long_desc = long_descs,
    stringsAsFactors = FALSE
  )
  .msg("now sort so that E is after V")
  new_order <- order.icd9(out[["code"]])
  stopifnot(!anyNA(out[["code"]]))
  stopifnot(!anyNA(new_order))
  stopifnot(!any(grepl(out[["code"]], pattern = "[[:space:]]")))
  stopifnot(!anyDuplicated(new_order))
  stopifnot(all(seq_len(nrow(out)) %in% new_order))
  # catches a mistaken zero-indexed reorder result
  stopifnot(length(setdiff(seq_len(nrow(out)), new_order)) == 0)
  stopifnot(length(setdiff(new_order, seq_len(nrow(out)))) == 0)
  out <- out[new_order, ]
  oldwarn <- options(warn = ifelse(.verbose(), 1, -1))
  on.exit(options(oldwarn), add = TRUE)
  if (!is.na(f_info["long"])) {
    encs <- Encoding(out[["long_desc"]])
    if (.verbose()) {
      message(
        "Found labelled encodings in long_desc: ",
        paste(unique(encs), collapse = ", ")
      )
      message(
        "non-ASCII rows of long descriptions are: ",
        paste(get_non_ascii(out[["long_desc"]]), collapse = ", ")
      )
      message(
        "Encodings found in long_desc: ",
        unique(
          Encoding(out[["long_desc"]][is_non_ascii(out[["long_desc"]])])
        )
      )
    }
  }
  out$short_desc <- enc2utf8(out$short_desc)
  out$long_desc <- enc2utf8(out$long_desc)
  var_name <- paste0("icd9cm", year, "_leaf")
  out <- out[order.icd9(out$code), ]
  out$code <- as.icd9cm(out$code)
  rownames(out) <- NULL
  .save_in_resource_dir(var_name = var_name, x = out)
  invisible(out)
}

#' Parse billable codes for ICD-9-CM version 27
#'
#' These have a quirk which needs a different approach
#' @keywords internal datagen
#' @noRd
.parse_leaf_desc_icd9cm_v27 <- function(...) {
  message("working on version 27 (2009)  quirk")
  v27_dat <- .icd9cm_sources[.icd9cm_sources$version == "27", ]
  fn_orig <- v27_dat$other_filename
  url <- v27_dat$url
  message("original v27 file name = '", fn_orig, "'. URL = ", url)
  f27_info <- .unzip_to_data_raw(
    url = url,
    file_name = fn_orig,
    ...
  )
  f <- file(f27_info$file_path, encoding = "latin1")
  icd9cm_billable27 <-
    utils::read.csv(f27_info$file_path,
      stringsAsFactors = FALSE,
      colClasses = "character", encoding = "latin1"
    )
  close(f)
  names(icd9cm_billable27) <- c("code", "long_desc", "short_desc")
  icd9cm_billable27 <- icd9cm_billable27[c(1, 3, 2)] # reorder columns
  reorder <- order.icd9(icd9cm_billable27[["code"]])
  invisible(icd9cm_billable27[reorder, ])
}

.make_icd9cm_parse_leaf_fun <- function(year) {
  # Must force, so that the values to the arguments are not promises which are
  # later evaluated in a different environment.
  force(year)
  parse_fun <- function() {
    .msg("Calling ICD-9-CM leaf parser for year:", year)
    .parse_icd9cm_leaf_year(year = year)
  }
  parse_fun_env <- environment(parse_fun)
  parse_fun_env$ver <- as.character(year)
  parse_fun
}

.make_icd9cm_leaf_parsers <- function(env = parent.frame(),
                                      verbose = .verbose()) {
  for (y in .icd9cm_sources$f_year) {
    # TODO: special case for 2011 / v32?
    parse_fun_name <- .get_parser_icd9cm_leaf_name(y)
    if (verbose) message("Making ICD-9-CM leaf parser: ", parse_fun_name)
    parse_fun <- .make_icd9cm_parse_leaf_fun(y)
    assign(parse_fun_name, parse_fun, envir = env)
  }
}
