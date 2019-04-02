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
#' @param save_data single logical value, if \code{TRUE} the source text or CSV
#'   file will be saved in the raw data directory, otherwise (the default) the
#'   data is simply returned invisibly.
#' @template verbose
#' @template offline
#' @return data frame with \code{icd9}, \code{short_desc} and \code{long_desc}
#'   columns. \code{NA} is placed in \code{long_desc} when not available.
#' @source
#' http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
#' @keywords internal datagen
#' @noRd
.icd9cm_parse_leaf_descs <- function(save_data = TRUE,
                                     verbose = FALSE,
                                     ...) {
  stopifnot(is.logical(save_data), length(save_data) == 1)
  stopifnot(is.logical(verbose), length(verbose) == 1)
  versions <- .icd9cm_sources$version
  if (verbose) {
    message(
      "Available versions of sources are: ",
      paste(versions, collapse = ", ")
    )
  }
  icd9cm_leaf_v32 <- .parse_icd9cm_leaf_year(
    ver = "2014",
    save_data = TRUE,
    verbose = verbose,
    ...
  )
  invisible(icd9cm_leaf_v32)
}

.dl_icd9cm_leaf_year <- function(year,
                                 verbose = .verbose(),
                                 ...) {
  year <- as.character(year)
  if (verbose) message("Downloading ICD-9-CM leaf/billable year: ", year)
  stopifnot(year %in% .icd9cm_sources$f_year)
  dat <- .icd9cm_sources[.icd9cm_sources$f_year == year, ]
  fn_short_orig <- dat$short_filename
  fn_long_orig <- dat$long_filename
  f_info_long <- NA
  f_info_short <- .unzip_to_data_raw(
    url = dat$url,
    file_name = fn_short_orig,
    verbose = verbose,
    save_name = .get_versioned_raw_file_name(fn_short_orig,
      ver = year
    ),
    ...
  )
  if (!is.na(fn_long_orig)) {
    f_info_long <- .unzip_to_data_raw(
      url = dat$url,
      file_name = fn_long_orig,
      verbose = verbose,
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
#'   e.g. \code{"32"}.
#' @template save_data
#' @param path Absolute path in which to save parsed data
#' @param ... Arguments passed to other functions, e.g., \code{offline} for
#'   \code{.unzip_to_data_raw}
#' @return invisibly return the result
#' @keywords internal datagen
#' @noRd
.parse_icd9cm_leaf_year <- function(year,
                                    verbose = .verbose(),
                                    ...) {
  stopifnot(length(year) == 1L)
  year <- as.character(year)
  stopifnot(grepl("^[[:digit:]]{4}$", year))
  if (verbose) message("Fetching billable codes version: ", year)
  # 2009 version 27 is exceptionally badly formatted:
  if (year == "2009") {
    return(invisible(.parse_leaf_desc_icd9cm_v27(...)))
  }
  stopifnot(year %in% .icd9cm_sources$f_year)
  f_info <- .dl_icd9cm_leaf_year(year = year, verbose = verbose, ...)
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
  if (verbose) message("codes and descs separated")
  out <- data.frame(
    code = short_codes,
    short_desc = short_descs,
    long_desc = long_descs,
    stringsAsFactors = FALSE
  )
  if (verbose) message("now sort so that E is after V")
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
  oldwarn <- options(warn = ifelse(verbose, 1, -1))
  on.exit(options(oldwarn), add = TRUE)
  if (!is.na(f_info["long"])) {
    encs <- Encoding(out[["long_desc"]])
    if (verbose) {
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
  out[order.icd9(out$code), ]
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
  message("working on version 27 quirk")
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

#' Fix NA sub-chapters in RTF parsing
#'
#' Fixes a couple of corner cases in parsing the 2011 ICD-9-CM RTF
#' @keywords internal datagen
#' @noRd
.fix_sub_chap_na <- function(x, start, end) {
  stop("no longer needed: icd9_sub_chapters.R in data is now correct")
  # 280, 740 (CONGENITAL ANOMALIES) are chapters with no sub-chapters defined.
  # For consistency, assign the same name to sub-chapters
  rng <- icd::expand_range(icd::as.icd9cm(start),
    icd::as.icd9cm(end),
    short_code = TRUE,
    defined = FALSE
  )
  congenital <- x[["code"]] %in% rng
  # assert all the same:
  stopifnot(all(x[congenital[1], "chapter"] == x[congenital[-1], "chapter"]))
  # insert a new level into the sub-chapter factor in the right place
  previous_sub <- .as_char_no_warn(x[(which(congenital) - 1)[1], "sub_chapter"])
  previous_sub_pos <- which(levels(x$sub_chapter) == previous_sub)
  congenital_title <- .as_char_no_warn(x[which(congenital)[1], "chapter"])
  new_subs <- .as_char_no_warn(x$sub_chapter)
  new_subs[congenital] <- congenital_title
  new_levels <- append(
    levels(x$sub_chapter),
    congenital_title,
    previous_sub_pos
  )
  x$sub_chapter <- factor(new_subs, new_levels)
  x
}

#' Generate ICD-9-CM hierarchy
#'
#' For each row of billing code, give the chapter, sub-chapter, major code and
#' description, and short and long descriptions. Currently this is specifically
#' for the 2011 ICD-9-CM after which there have been minimal changes.
#' Thankfully, ICD-10-CM has machine readable data available.
#' @template save_data
#' @template verbose
#' @template offline
#' @keywords internal datagen
#' @noRd
.icd9cm_gen_chap_hier <- function(save_pkg_data = FALSE,
                                  offline = .offline(),
                                  perl = TRUE,
                                  use_bytes = TRUE) {
  # TODO: Someday change 'billable' to 'leaf', and make consistent ICD-9 and
  # ICD-10, e.g. icd9cm2011 instead of icd9cm_hierarchy lookup tables
  stopifnot(is.logical(save_pkg_data), length(save_pkg_data) == 1)
  stopifnot(is.logical(offline), length(offline) == 1)
  stopifnot(is.logical(perl), length(perl) == 1)
  stopifnot(is.logical(use_bytes), length(use_bytes) == 1)
  icd9_rtf <- .parse_icd9cm_rtf_year(
    year = "2011",
    perl = perl,
    useBytes = use_bytes,
    save_pkg_data = FALSE
  )
  chaps <- .icd9_get_chapters(
    x = icd9_rtf$code,
    short_code = TRUE
  )
  icd9_order <- order.icd9
  chaps <- chaps[icd9_order(.as_char_no_warn(chaps$three_digit)), ]
  icd9_rtf <- icd9_rtf[icd9_order(icd9_rtf$code), ]
  out <- cbind(
    data.frame(
      "code" = icd9_rtf$code,
      "long_desc" = icd9_rtf$desc,
      stringsAsFactors = FALSE
    ),
    # the following can and should be factors:
    chaps
  )
  # insert the short descriptions from the billable codes text file. Where there
  # is no short description, e.g. for most Major codes, or intermediate codes,
  # just copy the long description over.

  # need ICD-9 codes to build this, right now just working off the final published edition.
  bill32 <- get_icd9cm2014_leaf()
  billable_codes <- bill32$code
  billable_rows <- which(out[["code"]] %in% billable_codes)
  title_rows <- which(out[["code"]] %nin% billable_codes)
  stopifnot(setdiff(
    c(billable_rows, title_rows),
    seq_along(out$code)
  ) == integer(0))
  out[billable_rows, "short_desc"] <- bill32$short_desc
  # for rows without a short description (i.e. titles, non-billable),
  # useexisting long desc
  out[title_rows, "short_desc"] <- out[title_rows, "long_desc"]
  # the billable codes list (where available) currently has better long
  # descriptions than the RTF parse. For previous years, there is no long desc
  # in billable, so careful when updating this.
  out[billable_rows, "long_desc"] <- bill32$long_desc
  # now put the short description in the right column position
  out <- out[c(
    "code", "short_desc", "long_desc", "three_digit",
    "major", "sub_chapter", "chapter"
  )]
  out[["short_desc"]] <- enc2utf8(out[["short_desc"]])
  out[["long_desc"]] <- enc2utf8(out[["long_desc"]])
  .icd9cm_hierarchy_sanity(out)
  billable <- out$code %in% bill32$code
  icd9cm_hierarchy <- cbind(out[1], billable, out[-1])
  row.names(icd9cm_hierarchy) <- NULL
  if (save_pkg_data) {
    .save_in_data_dir(icd9cm_hierarchy)
  }
  invisible(icd9cm_hierarchy)
}

.make_icd9cm_parse_leaf_fun <- function(year, verbose) {
  # Must force, so that the values to the arguments are not promises which are
  # later evaluated in a different environment.
  force(year)
  parse_fun <- function() {
    if (.verbose()) message("Calling ICD-9-CM leaf parser for year:", year)
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
    parse_fun <- .make_icd9cm_parse_leaf_fun(y, verbose = verbose)
    assign(parse_fun_name, parse_fun, envir = env)
  }
}
