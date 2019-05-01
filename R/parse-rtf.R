#' Fetch RTF for a given year
#'
#' Will return NULL if offline and not available
#' @param year character vector of length one, e.g. "2011"
#' @keywords internal datagen
#' @noRd
.dl_icd9cm_rtf_year <- function(year, ...) {
  year <- as.character(year)
  rtf_dat <- .icd9cm_sources[.icd9cm_sources$f_year == year, ]
  fn <- rtf_dat$rtf_filename
  url <- rtf_dat$rtf_url
  .unzip_to_data_raw(
    url = url,
    save_name = .get_versioned_raw_file_name(fn, ver = year),
    file_name = fn,
    ...
  )
}

#' Parse RTF description of entire ICD-9-CM for a specific year
#'
#' Currently only the most recent update is implemented. Note that CMS have
#' published additional ICD-9-CM billable code lists since the last one from the
#' CDC: I think these have been the same every year since 2011, though. The last
#' CDC release is \code{Dtab12.rtf} from 2011.
#'
#' The file itself is 7 bit ASCII, but has its own internal encoding using
#' \sQuote{CP1252}. Test \sQuote{Meniere's disease} with lines 24821 to 24822
#' from 2012 RTF
#'
#' \code{save_pkg_majors} will save the majors it finds in the package data, the
#' rest is cached as resource data.
#' @param year from 1996 to 2012 (this is what CDC has published). Only 2012
#'   implemented thus far
#' @param save_pkg_majors logical
#' @source \url{https://www.cdc.gov/nchs/icd/icd9cm.htm} Navigate to
#'   'Dtab12.zip' in the 2011 data. and similar files run from 1996 to 2011.
#' @keywords internal datagen
#' @noRd
.parse_icd9cm_rtf_year <- function(year = "2014",
                                   save_pkg_majors = FALSE) {
  year <- as.character(year)
  stopifnot(year %in% .icd9cm_sources$f_year)
  f_info_rtf <- .dl_icd9cm_rtf_year(year)
  if (is.null(f_info_rtf)) {
    stop("RTF data for year ", year, " unavailable.")
  }
  fp <- f_info_rtf$file_path
  fp_conn <- file(fp, encoding = "ASCII")
  on.exit(close(fp_conn))
  rtf_lines <- readLines(fp_conn, warn = FALSE, encoding = "ASCII")
  out <- .rtf_parse_lines(rtf_lines,
    year = year,
    save_pkg_majors = save_pkg_majors
  )
  out <- icd::as.icd9cm(.swap_names_vals(out))
  out_df <- data.frame(
    code = icd::as.icd9cm(icd::decimal_to_short(unname(out))),
    desc = names(out),
    stringsAsFactors = FALSE
  )
  out_df <- out_df[order.icd9(out), ]
  out_df <- .lookup_icd9_hier(out_df, short_code = TRUE)
  out_df$code <- as.icd9cm(out_df$code)
  # replace desc with desc_short and desc_long (or short_desc etc?)
  leaves <- .get_fetcher_fun(
    .get_icd9cm_name(year = year, leaf = TRUE)
  )()
  leaves[["leaf"]] <- TRUE
  out_df <- merge.data.frame(
    x = out_df,
    y = leaves,
    by = "code",
    all = TRUE,
    sort = FALSE
  )
  out_df[is.na(out_df$short_desc), "short_desc"] <-
    out_df[is.na(out_df$short_desc), "desc"]
  out_df[is.na(out_df$long_desc), "long_desc"] <-
    out_df[is.na(out_df$long_desc), "desc"]
  out_df[is.na(out_df$leaf), "leaf"] <- FALSE
  out_df$desc <- NULL
  out_df <- out_df[
    order.icd9(out_df$code),
    c(
      "code",
      "leaf",
      "short_desc",
      "long_desc",
      "three_digit",
      "major",
      "sub_chapter",
      "chapter"
    )
  ]
  rownames(out_df) <- NULL
  if (anyNA(out_df$code) || anyNA(out_df$three_digit)) {
    print(out_df[which(with(out_df, is.na(code) | is.na(three_digit))), ])
    print(summary(is.na(out_df)))
  }
  out_df$three_digit <- factor_sorted_levels(as.icd9cm(out_df$three_digit))
  invisible(out_df)
}

.lookup_icd9_hier <- function(x,
                              year = "2014",
                              short_code) {
  # keep in order, so merges don't screw things up, although this slows things
  dat <- x
  if (!short_code) dat$code <- icd::decimal_to_short(dat$code)
  dat <- dat[order.icd9(dat$code), , drop = FALSE]
  dat$code <- as.character(dat$code)
  dat$three_digit <- get_major.icd9(dat$code, short_code = TRUE)
  .msg("Generating sub-chapter lookup for year: ", year)
  sc_lookup <- .icd9_generate_subchap_lookup()
  .msg("Generating chap lookup for year: ", year)
  chap_lookup <- .icd9_generate_chap_lookup()
  mismatch_sub_chap <-
    dat$three_digit[which(dat$three_digit %nin% sc_lookup$sc_major)]
  if (length(mismatch_sub_chap[!is.na(mismatch_sub_chap)]) != 0L) {
    stop(
      "mismatch: ",
      paste(mismatch_sub_chap, collapse = ", ")
    )
  }
  mj_lookup <- data.frame(
    three_digit = unname(icd::icd9_majors),
    mj_major = names(icd::icd9_majors)
  )
  major_merge <- merge(
    x = dat["three_digit"],
    y = mj_lookup,
    by.x = "three_digit",
    by.y = "three_digit",
    all.x = TRUE
  )
  sub_chap_merge <- merge(
    x = dat["three_digit"],
    y = sc_lookup,
    by.x = "three_digit",
    by.y = "sc_major",
    all.x = TRUE
  )
  chap_merge <- merge(
    dat["three_digit"],
    chap_lookup,
    by.x = "three_digit",
    by.y = "chap_major",
    all.x = TRUE
  )
  major_merge <- major_merge[order.icd9(major_merge$three_digit), ]
  sub_chap_merge <- sub_chap_merge[order.icd9(sub_chap_merge$three_digit), ]
  chap_merge <- chap_merge[order.icd9(chap_merge$three_digit), ]
  dat[["major"]] <- factor_nosort(major_merge[["mj_major"]])
  dat[["sub_chapter"]] <- factor_nosort(sub_chap_merge[["sc_desc"]])
  dat[["chapter"]] <- factor_nosort(chap_merge[["chap_desc"]])
  # levels specified to keep ICD-9 ordering 0-9VE?
  dat[["three_digit"]] <- factor(dat[["three_digit"]],
    levels = unique(dat[["three_digit"]])
  )
  class(dat[["three_digit"]]) <- c("icd9cm", "icd9", "factor")
  dat
}

# just wrap the new function in the old name
.icd9_get_chapters <- function(x, short_code = TRUE) {
  dframe <- data.frame(
    code = x,
    stringsAsFactors = FALSE
  )
  .lookup_icd9_hier(dframe,
    short_code = short_code
  )[-1]
}

.rtf_pre_filter <- function(filtered) {
  stopifnot(is.character(filtered))
  # merge any line NOT starting with "\\par" on to previous line
  non_par_lines <- grep(
    pattern = "^\\\\par",
    x = filtered,
    invert = TRUE
  )
  # in reverse order, put each non-par line on end of previous, then filter out
  # all non-par lines
  for (i in rev(non_par_lines))
    filtered[i - 1] <- paste(filtered[i - 1], filtered[i], sep = "")
  filtered <- grep("^\\\\par", filtered, value = TRUE)
  filtered <- .rtf_fix_unicode(filtered)
  # extremely long terminal line in primary source is junk
  longest_lines <- which(nchar(filtered) > 3000L)
  filtered <- filtered[-longest_lines]
  filtered <- .rtf_strip(filtered)
  grep("^[[:space:]]*$", filtered, value = TRUE, invert = TRUE)
}

#' parse lines of RTF
#'
#' parse a character vector containing RTF strings
#'
#' \code{...} might include: \code{perl = TRUE, useBytes = TRUE}
#' @param rtf_lines character vector containing RTF. Encoding?
#' @return named character vector, with names being the ICD-9 codes, and the
#'   contents being the descriptions from the RTF source. Elsewhere I do this
#'   the other way around, but the tests are now wired for this layout. 'Tidy'
#'   data would favour having an unnamed two-column data frame.
#' @keywords internal datagen
#' @noRd
.rtf_parse_lines <- function(rtf_lines,
                             year,
                             save_pkg_majors = FALSE) {
  stopifnot(is.character(rtf_lines))
  filtered <- .rtf_pre_filter(rtf_lines)
  majors <- .rtf_make_majors(filtered, save_pkg_majors = save_pkg_majors)
  .msg("Have ", length(majors), " majors.")
  # this is so ghastly: find rows with square brackets containing definition of
  # subset of fourth or fifth digit codes. Need to pull code from previous row,
  # and create lookup, so we can exclude these when processing the fourth and
  # fifth digits
  invalid_qual <- .rtf_make_invalid_qual(filtered)
  # fix annual quirks apparing from the invalid exclusion step: e.g., 2008 has
  # typo in the original RTF: fix it here, which is the only reason to pass year
  # to this function
  if (year == "2008") {
    # replace 945.09 with 945.0{7,8}
    for (f945 in 0:5) {
      splice_idx <- which(invalid_qual == sprintf("945.%d9", f945))
      invalid_qual <- c(
        invalid_qual[seq_len(splice_idx - 1)],
        sprintf(c("945.%d7", "945.%d8"), f945),
        invalid_qual[seq.int(splice_idx + 1, length(invalid_qual))]
      )
    }
  }
  # Several occurances of "Requires fifth digit", referring back to the previous
  # higher-level definition, without having the parent code in the line itself
  re_fifth_range_other <- "fifth +digit +to +identify +stage"
  fifth_backref <- grep(re_fifth_range_other, filtered)
  # for these, construct a string which will be captured in the next block
  # e.g. "Requires fifth digit to identify stage:" becomes
  # "Requires fifth digit to identify stage: 634 Spontaneous abortion"
  filtered[fifth_backref] <-
    paste(filtered[fifth_backref], filtered[fifth_backref - 1])
  re_fourth_range <- "fourth-digit.+categor"
  fourth_rows <- grep(re_fourth_range, filtered)
  # at least two examples of "Use 0 as fourth digit for category 672"
  re_fourth_digit_zero <- "Use 0 as fourth digit for category"
  fourth_digit_zero_lines <- grep(re_fourth_digit_zero, filtered)
  fourth_digit_zero_categories <- unname(
    .str_pair_match(
      filtered[fourth_digit_zero_lines],
      "(.*category )([[:digit:]]{3})$"
    )
  )
  # deal with 657 and 672 (in the default RTF), by appending the elements to the
  # end of the input list. argh.
  for (categ in fourth_digit_zero_categories) {
    parent_row <- grep(paste0("^", categ, " .+"), filtered, value = TRUE)
    filtered[length(filtered) + 1] <-
      paste0(categ, ".0 ", .str_pair_match(
        parent_row,
        "([[:digit:]]{3} )(.+)"
      ))
  }
  lookup_fifth <- .rtf_make_lookup_fifth(filtered, re_fifth_range_other)
  filtered_excl <- .rtf_filter_excludes(filtered)
  out <- .rtf_main_filter(filtered_excl)
  out <- c(
    out,
    .rtf_lookup_fourth(
      out = out,
      lookup_fourth = .rtf_generate_fourth_lookup(filtered, fourth_rows)
    )
  )
  out <- c(out, .rtf_lookup_fifth(out, lookup_fifth))
  out <- .rtf_fix_duplicates(out)
  out <- out[-which(names(out) %in% invalid_qual)]
  .rtf_fix_quirks_2015(out)
}

#' exclude some unwanted rows from filtered RTF
#'
#' Ignore excluded codes, and fix some odd-balls so they don't get dropped
#' "707.02Upper back", "066.40West Nile fever, unspecified", etc
#' @keywords internal datagen
#' @noRd
.rtf_filter_excludes <- function(filtered) {
  # drop excludes: lines with bracketed codes (removes chapter headings)
  re_bracketed <- paste0(
    "\\((", re_icd9_decimal_bare,
    ")-(", re_icd9_decimal_bare, ")\\)"
  )
  filtered <- grep(re_bracketed,
    filtered,
    value = TRUE,
    invert = TRUE
  )
  filtered <- grep("Exclude",
    filtered,
    value = TRUE,
    invert = TRUE
  )
  sub(
    "((70[[:digit:]]\\.[[:digit:]]{2})|066\\.40)([[:alpha:]])", "\\1 \\2",
    filtered
  )
}

#' filter RTF for actual ICD-9 codes
#'
#' Keep some more information, but we'll just take the primary description for
#' each item, i.e. where a code begins a line. Some codes have ten or so
#' alternative descriptions, e.g. 410.0
#' @keywords internal datagen
#' @noRd
.rtf_main_filter <- function(filtered) {
  filtered <- grep(paste0("^[[:space:]]*(", re_icd9_decimal_strict_bare, ") "),
    filtered,
    value = TRUE
  )
  # spaces to single
  filtered <- gsub("[[:space:]]+", " ", filtered)
  # fix a few things, e.g. "040. 1 Rhinoscleroma", "527 .0 Atrophy"
  filtered <-
    sub(
      "^([VvEe]?[[:digit:]]+) ?\\. ?([[:digit:]]) (.*)", "\\1\\.\\2 \\3",
      filtered
    )
  # and high-level headings like "210-229 Benign neoplasms"
  filtered <- grep("^[[:space:]]*[[:digit:]]{3}-[[:digit:]]{3}.*",
    filtered,
    value = TRUE,
    invert = TRUE
  )
  # "2009 H1 N1 swine influenza virus"
  filtered <- grep("^2009", filtered, value = TRUE, invert = TRUE)
  # "495.7 \"Ventilation\" pneumonitis"
  re_code_desc <- paste0("^(", re_icd9_decimal_bare, ") +([ \"[:graph:]]+)")
  # out is the start of the eventual output of code to description pairs. seems
  # to be quicker with perl and useBytes both FALSE
  .str_pair_match(filtered, re_code_desc, perl = FALSE, useBytes = FALSE)
}

.rtf_make_majors <- function(filtered, save_pkg_majors = FALSE) {
  major_lines <- grep(paste0("^(", re_icd9_major_strict_bare, ") "),
    filtered,
    value = TRUE
  )
  re_major_split <- "([^[:space:]]+)[[:space:]]+(.+)"
  icd9_majors <- gsub(
    pattern = re_major_split, replacement = "\\1",
    x = major_lines, perl = TRUE
  )
  names(icd9_majors) <- gsub(
    pattern = re_major_split, replacement = "\\2",
    x = major_lines, perl = TRUE
  )
  # this sub-chapter is simply missing from the otherwise consistent RTF way
  # 'major' types are reported:
  icd9_majors[["Place of occurrence"]] <- "E849"
  # There are some duplicates created by the major search, mostly E001 to E030
  # which are just listed twice in RTF. Also 199 (with punctuation difference),
  # 209 and 239.
  icd9_majors <- icd9_majors[!duplicated(icd9_majors)]
  .save_in_resource_dir("icd9_majors", x = icd9_majors)
  if (save_pkg_majors) .save_in_data_dir("icd9_majors")
  invisible(icd9_majors)
}

# Find the codes associated with lines in which a range is specified for a
# terminal digit, e.g., [0-6] or [0-5,9]
.rtf_make_invalid_qual <- function(filtered) {
  re_qual_subset <- "\\[[-, [:digit:]]+\\]"
  qual_subset_lines <- grep(re_qual_subset, filtered)
  invalid_qual <- c()
  for (ql in qual_subset_lines) {
    # get prior code to the range specified
    m1 <- .str_match_all(
      filtered[ql - 1],
      paste0("(", re_icd9_decimal_bare, ") (.*)")
    )
    # get the code to which the range is appended, e.g, 945.0[0-8], which is a
    # typo in the RTF original document for 2008, and fixed in 2009.
    code <- unlist(m1)[[2]]
    # the following doesn't correctly find 945.09 in 2008 data
    sb <- .rtf_parse_qualifier_subset(filtered[ql])
    inv_sb <- setdiff(as.character(0:9), sb)
    if (length(inv_sb) == 0) {
      next
    }
    if (grepl("\\.", code)) {
      invalid_qual <- c(invalid_qual, paste0(code, inv_sb))
    } else {
      invalid_qual <- c(invalid_qual, paste0(code, ".", inv_sb))
    }
  }
  invalid_qual
}

#' Generate look-up for four-digit codes
#'
#' \code{lookup_fourth} will contain vector of suffices, with names being the
#' codes they augment
#' @return named character vector, names are the ICD codes, values are the
#'   descriptions
#' @keywords internal datagen
#' @noRd
.rtf_generate_fourth_lookup <- function(filtered, fourth_rows) {
  lookup_fourth <- c()
  for (f in fourth_rows) {
    range <- .rtf_parse_fifth_digit_range(filtered[f])
    fourth_suffices <- .str_pair_match(
      string = filtered[seq(f + 1, f + 37)],
      pattern = "^([[:digit:]])[[:space:]](.*)"
    )
    re_fourth_defined <-
      paste(c("\\.[", names(fourth_suffices), "]$"), collapse = "")
    # drop members of range which don't have defined fourth digit
    range <- grep(re_fourth_defined, range, value = TRUE)
    # now replace value with the suffix, with name of item being the code itself
    names(range) <- range
    last <- -1
    for (fourth in names(fourth_suffices)) {
      if (last > as.integer(fourth)) break
      re_fourth <- paste0("\\.", fourth, "$")

      range[grep(re_fourth, range)] <- fourth_suffices[fourth]
      last <- fourth
    }
    lookup_fourth <- c(lookup_fourth, range)
  }
  if (.verbose()) {
    message("lookup_fourth has length: ", length(lookup_fourth), ", head: ")
    print(lookup_fourth[1:5])
  }
  lookup_fourth
}

#' apply fourth digit qualifiers
#'
#' use the lookup table of fourth digit
#'
#' @keywords internal datagen
#' @noRd
.rtf_lookup_fourth <- function(out, lookup_fourth) {
  out_fourth <- c()
  out_env <- list2env(as.list(out))
  for (f_num in seq_along(lookup_fourth)) {
    lf <- lookup_fourth[f_num]
    f <- names(lf)
    parent_code <- get_major.icd9(f, short_code = FALSE)
    if (!is.null(out_env[[parent_code]])) {
      pair_fourth <- paste(out[parent_code], lf, sep = ", ")
      names(pair_fourth) <- f
      out_fourth <- append(out_fourth, pair_fourth)
    }
  }
  if (.verbose()) {
    message("fourth output lines: length = ", length(out_fourth), ", head: ")
    print(out_fourth[1:5])
  }
  rm(out_env)
  out_fourth
}

.rtf_make_lookup_fifth <- function(filtered,
                                   re_fifth_range_other) {
  re_fifth_range <- "ifth-digit subclas|fifth-digits are for use with codes"
  re_fifth_rows <- paste(re_fifth_range, re_fifth_range_other, sep = "|")
  fifth_rows <- grep(pattern = re_fifth_rows, x = filtered)
  # lookup_fifth will contain vector of suffices, with names being the codes
  # they augment
  lookup_fifth <- c()
  for (f in fifth_rows) {
    .msg("working on fifth-digit row:", f)
    range <- .rtf_parse_fifth_digit_range(filtered[f])
    f1 <- filtered[seq(f + 1, f + 20)]
    f2 <- grep(pattern = "^[[:digit:]][[:space:]].*", f1, value = TRUE)
    fifth_suffices <- .str_pair_match(f2, "([[:digit:]])[[:space:]](.*)")
    re_fifth_defined <- paste(c("\\.[[:digit:]][", names(fifth_suffices), "]$"),
      collapse = ""
    )
    # drop members of range which don't have defined fifth digit
    range <- grep(re_fifth_defined, range, value = TRUE)
    # now replace value with the suffix, with name of item being the code itself
    names(range) <- range
    last <- -1L
    for (fifth in names(fifth_suffices)) {
      if (last > as.integer(fifth)) break
      re_fifth <- paste0("\\.[[:digit:]]", fifth, "$")
      range[grep(re_fifth, range)] <- fifth_suffices[fifth]
      last <- fifth
    }
    lookup_fifth <- c(lookup_fifth, range)
  }
  # V30-39 are a special case because combination of both fourth and fifth
  # digits are specified
  re_fifth_range_v30v39 <-
    "The following two fifths-digits are for use with the fourth-digit \\.0"
  re_v30v39_fifth <- "V3[[:digit:]]\\.0[01]$"
  lines_v30v39 <- grep(re_fifth_range_v30v39, filtered)
  stopifnot(length(lines_v30v39) == 1)
  f1 <- filtered[seq(from = lines_v30v39 + 1, to = lines_v30v39 + 3)]
  f2 <- grep(f1, pattern = "^[[:digit:]][[:space:]].*", value = TRUE)
  suffices_v30v39 <- .str_pair_match(f2, "([[:digit:]])[[:space:]](.*)")
  range <- c(
    icd::expand_range("V30", "V37",
      short_code = FALSE, defined = FALSE
    ),
    # TODO: just undefined
    icd9_children_decimal_unordered_undefined_rcpp("V39")
  )
  range <- grep(re_v30v39_fifth, range, value = TRUE)
  names(range) <- range
  for (fifth in names(suffices_v30v39)) {
    # only applies to .0x (in 2015 at least), but .1 also exists without 5th
    # digit
    re_fifth <- paste0("\\.0", fifth, "$")
    range[grep(re_fifth, range)] <- suffices_v30v39[fifth]
  }
  c(lookup_fifth, range)
}

.rtf_lookup_fifth <- function(out, lookup_fifth) {
  out_fifth <- character(5000) # 2011 data is 4870 long
  n <- 1L
  out_env <- list2env(as.list(out))
  for (f_num in seq_along(lookup_fifth)) {
    lf <- lookup_fifth[f_num]
    f <- names(lf)
    parent_code <- substr(f, 0, nchar(f) - 1)
    if (!is.null(out_env[[parent_code]])) {
      out_fifth[n] <- paste(out[parent_code], lf, sep = ", ")
      names(out_fifth)[n] <- f
      n <- n + 1L
    }
  }
  out_fifth <- out_fifth[1:n - 1]
  if (.verbose()) {
    message("fifth output lines: length = ", length(out_fifth), ", head: ")
    print(out_fifth[1:5])
  }
  out_fifth
}

#' Fix Unicode characters in RTF
#'
#' fix ASCII, Code Page 1252 and Unicode horror: some character definitions are
#' split over lines. This needs care in Windows, or course. Maybe Mac, too?
#'
#' First: c cedilla, e grave, e acute Then:  n tilde, o umlaut
#' @keywords internal datagen
#' @noRd
.rtf_fix_unicode <- function(filtered) {
  filtered <- gsub("\\\\'e7", "\u00e7", filtered) # c cedilla
  filtered <- gsub("\\\\'e8", "\u00e8", filtered) # e gravel
  filtered <- gsub("\\\\'e9", "\u00e9", filtered) # e acute
  filtered <- gsub("\\\\'f1", "\u00f1", filtered) # n tilde
  filtered <- gsub("\\\\'f6", "\u00f6", filtered) # o umlaut
  enc2utf8(filtered)
}

#' Fix duplicates detected in RTF parsing
#'
#' Clean up duplicates (about 350 in 2015 data), mostly one very brief
#' description and a correct longer one; or the descriptions are identical.
#' @keywords internal datagen
#' @noRd
.rtf_fix_duplicates <- function(out) {
  dupes <- out[duplicated(names(out)) | duplicated(names(out), fromLast = TRUE)]
  dupes <- unique(names(dupes))
  for (d in dupes) {
    dupe_rows <- which(names(out) == d)
    if (all(out[dupe_rows[1]] == out[dupe_rows[-1]])) {
      .trc("dropping simple dupes, d = ", d)
      out <- out[-dupe_rows[-1]]
      .trc("next d")
      next
    }
    .dbg("about to get nchar dupe descs")
    desclengths <- nchar(out[dupe_rows])
    max_len <- max(desclengths)
    if (.verbose()) {
      message(
        "removing differing duplicates: ",
        paste(unique(names(out[dupe_rows])), collapse = ", "),
        ", with values: ",
        paste(out[dupe_rows])
      )
      .trc("desclengths: ", paste(desclengths, collapse = ", "))
      .trc("max_len: ", max_len)
      .trc(
        "which(desclengths != max_len): ",
        which(desclengths != max_len)
      )
      .trc("dupe_rows: ", paste(dupe_rows, collapse = ", "))
      .trc(
        "dupe_rows[-which(desclengths != max_len)]: ",
        dupe_rows[-which(desclengths != max_len)]
      )
      .trc(
        "out[-dupe_rows[-which(desclengths != max_len)]]",
        out[dupe_rows[-which(desclengths != max_len)]]
      )
    }
    out <- out[-dupe_rows[-which(desclengths != max_len)]]
  }
  out
}

#' Fix quirks for 2015 RTF parsing
#'
#' 2015 quirks (many more are baked into the parsing: try to splinter out the
#' most specific) some may well apply to other years 650-659 ( and probably many
#' others don't use whole subset of fourth or fifth digit qualifiers) going to
#' have to parse these, e.g. [0,1,3], as there are so many...
#' @keywords internal datagen
#' @noRd
.rtf_fix_quirks_2015 <- function(out) {
  out <- out[grep("65[12356789]\\.[[:digit:]][24]", names(out), invert = TRUE)]

  # 657 just isn't formatted like any other codes
  out["657.0"] <- "Polyhydramnios"
  out["657.00"] <-
    "Polyhydramnios, unspecified as to episode of care or not applicable"
  out["657.01"] <-
    "Polyhydramnios, delivered, with or without mention of antepartum condition"
  out["657.03"] <- "Polyhydramnios, antepartum condition or complication"

  out["719.69"] <- "Other symptoms referable to joint, multiple sites"
  out["807.19"] <- "Open fracture of multiple ribs, unspecified"
  out["E849"] <- "Place of occurence"
  out
}

#' Parse a row of RTF source data for ranges to apply fifth digit
#'
#' Sub-classifications returns all the possible 5 digit codes encompassed by the
#' given definition. This needs to be whittled down to just those matching fifth
#' digits, but we haven't parsed them yet.
#' @keywords internal datagen
#' @noRd
.rtf_parse_fifth_digit_range <- function(row_str) {
  stopifnot(is.character(row_str))
  out <- c()
  # get numbers and number ranges
  vals <- grep(unlist(strsplit(row_str, "[, :;]")),
    pattern = "[VvEe]?[0-9]", value = TRUE
  )
  .msg("vals are:", paste(vals, collapse = ", "))
  # sometimes  we get things like:
  # [1] "345.0" ".1"    ".4-.9"
  grepl(pattern = "^\\.[[:digit:]]+.*", vals) -> decimal_start
  if (any(decimal_start)) {
    base_code <- vals[1] # assume first is the base
    stopifnot(icd::is_valid(base_code, short_code = FALSE))
    for (dotmnr in vals[-1]) {
      .msg("dotmnr is: ", dotmnr)
      if (grepl("-", dotmnr)) {
        # range of minors
        pair <- unlist(strsplit(dotmnr, "-", fixed = TRUE))
        first <- paste0(get_major.icd9(base_code, short_code = FALSE), pair[1])
        last <- paste0(get_major.icd9(base_code, short_code = FALSE), pair[2])
        .dbg("expanding specified minor range from ", first, " to ", last)
        # TODO: do I need expand range variant with defined always turned off,
        # so I never need to have circular reference to
        # icd9cm_hierarchy/icd9cm2014?
        out <- c(out, icd::expand_range(first,
          last,
          short_code = FALSE,
          defined = FALSE
        ))
      } else {
        single <- paste0(get_major.icd9(base_code, short_code = FALSE), dotmnr)
        # call just undefined
        out <- c(out, icd9_children_decimal_unordered_undefined_rcpp(single))
      }
    }
    vals <- vals[1] # still need to process the base code
  }
  for (v in vals) {
    # take care of ranges
    if (grepl("-", v)) {
      pair <- unlist(strsplit(v, "-", fixed = TRUE))
      # sanity check
      stopifnot(all(icd::is_valid(pair, short_code = FALSE)))
      if (.verbose()) {
        message("expanding explicit range ", pair[1], " to ", pair[2])
      }
      # formatting errors can lead to huge range expansions, e.g. "8-679".
      # quickly strip off V or E part for comparison
      pair_one <- gsub("[^[:digit:]]", "", pair[1])
      pair_two <- gsub("[^[:digit:]]", "", pair[2])
      if (as.integer(pair_two) - as.integer(pair_one) > 10) {
        warning("probable formatting misinterpretation: huge range expansion")
      }
      out <- c(out, icd::expand_range(pair[1], pair[2],
        short_code = FALSE, defined = FALSE
      ))
    } else {
      # take care of single values
      if (!icd::is_valid(v, short_code = FALSE)) {
        stop(paste(
          "invalid code is: ",
          icd::get_invalid(v, short_code = FALSE)
        ))
      }
      # TODO: call just undefined
      out <- c(out, icd9_children_decimal_unordered_undefined_rcpp(v))
    }
  }
  out
}

.rtf_parse_qualifier_subset <- function(qual) {
  stopifnot(is.character(qual), length(qual) == 1)
  out <- c()
  s1 <- strsplit(strip(qual), "[]\\[,]")
  s2 <- grep(pattern = "[[:digit:]]", unlist(s1), value = TRUE)
  vals <- unlist(strsplit(s2, ","))
  for (v in vals) {
    if (grepl("-", v)) {
      pair <- as.integer(unlist(strsplit(v, "-")))
      out <- c(out, seq(pair[1], pair[2]))
      next
    }
    out <- c(out, as.integer(v))
  }
  as.character(out)
}

#' Strip RTF
#'
#' Take a vector of character strings containing RTF, replace each \\tab with a
#' space and eradicate all other RTF symbols
#'
#' just for \\tab, replace with space, otherwise, drop RTF tags entirely
#' @param x vector of character strings containing RTF
#' @keywords internal datagen
#' @noRd
.rtf_strip <- function(x) {
  # nolint start
  x <- gsub("\\\\tab ", " ", x)
  x <- gsub("\\\\[[:punct:]]", "", x) # control symbols, not control words
  x <- gsub("\\\\lsdlocked[ [:alnum:]]*;", "", x) # special case
  x <- gsub("\\{\\\\bkmk(start|end).*?\\}", "", x)
  x <- gsub("\\{\\\\xmlopen\\\\xmlns2\\{\\\\factoidname[[:space:]A-Za-z]+\\}\\}", "", x)
  x <- gsub("\\{\\\\\\*\\\\xmlclose\\}", "", x)
  # no backslash in this next list, others removed from
  # http://www.regular-expressions.info/posixbrackets.html
  # punct not quite the same: [!"\#$%&'()*+,\-./:;<=>?@\[\\\]^_`{|}~]
  x <- gsub("\\\\[-[:alnum:]]*[ !\"#$%&'()*+,-./:;<=>?@^_`{|}~]?", "", x)
  x <- gsub(" *(\\}|\\{)", "", x)
  trimws(x)
  # nolint end
}

.make_icd9cm_parse_rtf_fun <- function(year) {
  # Must force, so that the values to the arguments are not promises which are
  # later evaluated in a different environment.
  force(year)
  parse_fun <- function() {
    .msg("Calling ICD-9-CM RTF parser for year:", year)
    .parse_icd9cm_rtf_year(year = year)
  }
  parse_fun_env <- environment(parse_fun)
  parse_fun_env$ver <- as.character(year)
  parse_fun
}

.make_icd9cm_rtf_parsers <- function(env = parent.frame()) {
  for (y in .icd9cm_sources$f_year) {
    # TODO: special case for 2011 / v32?
    parse_fun_name <- .get_parser_icd9cm_rtf_name(y)
    .msg("Making ICD-9-CM RTF parser: ", parse_fun_name)
    parse_fun <- .make_icd9cm_parse_rtf_fun(y)
    assign(parse_fun_name, parse_fun, envir = env)
  }
}
