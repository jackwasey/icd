# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

# try parsing the RTF, and therefore get subheadings, as well as billable codes.
# ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/
#
# see https://github.com/LucaFoschini/ICD-9_Codes for a completely different
# approach in python

#' Fetch RTF for a given year
#'
#' Will return NULL if offline and not available
#' @param year character vector of length one, e.g. "2011"
#' @param offline single logical value
#' @keywords internal
fetch_rtf_year <- function(year, offline = TRUE) {
  assert_string(year)
  assert_flag(offline)

  rtf_dat <- icd9_sources[icd9_sources$f_year == year, ]
  fn <- rtf_dat$rtf_filename

  unzip_to_data_raw(rtf_dat$rtf_url, file_name = fn, offline = offline)
}

#' parse RTF description of entire ICD-9-CM for a specific year
#'
#' Currently only the most recent update is implemented. Note that
#'   CMS have published additional ICD-9-CM billable code lists since the last
#'   one from the CDC: I think these have been the same every year since 2011,
#'   though. The last CDC release is \code{Dtab12.rtf} from 2011.
#' @param year from 1996 to 2012 (this is what CDC has published). Only 2012
#'   implemented thus far
#' @template save_data
#' @template verbose
#' @source
#' http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab12.zip
#' and similar files run from 1996 to 2011.
#' @keywords internal
parse_rtf_year <- function(year = "2011", save_data = FALSE, verbose = FALSE, offline = TRUE) {
  assert_string(year)
  assert_flag(save_data)
  assert_flag(verbose)
  assert_flag(offline)

  f_info_rtf <- fetch_rtf_year(year, offline = offline)

  if (is.null(f_info_rtf))
    stop("RTF data for year ", year, " unavailable.")

  fp <- f_info_rtf$file_path
  # the file is basically 7-bit ASCII, but non-ASCII characters are encoded with
  # escape sequences. There is a code page embedded in the document, in this
  # case CP-1252.
  fp_conn <- file(fp, encoding = "ASCII")
  on.exit(close(fp_conn))
  rtf_lines <- readLines(fp_conn, warn = FALSE, encoding = "ASCII")

  # the file itself is 7 bit ASCII, but has its own internal encoding using
  # CP1252. test meniere's disease with lines  24821 to 24822 from 2012 RTF

  out <- parse_rtf_lines(rtf_lines, verbose = verbose,
                         save_extras = save_data) %>%
    swap_names_vals %>%
    icd_sort.icd9(short_code = FALSE)

  invisible(
    data.frame(
      code  = out %>% unname %>% icd_decimal_to_short.icd9 %>% icd9cm,
      desc = names(out),
      stringsAsFactors = FALSE)
  )
}

#' parse lines of RTF
#'
#' parse a character vector containing RTF strings
#' @param rtf_lines character vector containing RTF. Encoding?
#' @template verbose
#' @return named character vector, with names being the ICD-9 codes, and the
#'   contents being the descriptions from the RTF source. Elsewhere I do this
#'   the other way around, but the tests are now wired for this layout. 'Tidy'
#'   data would favour having an unnamed two-column data frame.
#' @keywords internal
parse_rtf_lines <- function(rtf_lines, verbose = FALSE, save_extras = FALSE) {

  assert_character(rtf_lines)
  assert_flag(verbose)

  # filtered <- iconv(rtf_lines, from = "ASCII", to = "UTF-8", mark = TRUE) I
  # think the first 127 characters of ASCII are the same in Unicode, but we must
  # make sure R treats the lines as Unicode.
  filtered <- rtf_lines
  # merge any line NOT starting with "\\par" on to previous line
  non_par_lines <- grep(pattern = "^\\\\par", x = filtered, invert = TRUE)
  # in reverse order, put each non-par line on end of previous, then filter out
  # all non-par lines
  if (verbose)
    message("joining lines split on par")

  for (i in rev(non_par_lines)) {
    filtered[i - 1] <- paste(filtered[i - 1], filtered[i], sep = "")
  }

  filtered <- grep("^\\\\par", filtered, value = TRUE)

  # fix ASCII/CP1252/Unicode horror: of course, some char defs are split over
  # lines... This needs care in Windows, or course. Maybe Mac, too?
  filtered <- gsub("\\\\'e7", "\u00e7", filtered) # c cedila
  filtered <- gsub("\\\\'e8", "\u00e8", filtered) # e gravel
  filtered <- gsub("\\\\'e9", "\u00e9", filtered) # e acute
  filtered <- gsub("\\\\'f1", "\u00f1", filtered) # n tilde
  filtered <- gsub("\\\\'f16", "\u00f6", filtered) # o umlaut

  # drop stupid long line at end:
  longest_lines <- nchar(filtered) > 3000
  # if none, then -c() returns no rows, so we have to test first
  if (any(longest_lines))
    filtered <- filtered[-c(which(longest_lines))]

  filtered <- rtf_strip(filtered)

  filtered <- grep("^[[:space:]]*$", filtered, value = TRUE, invert = TRUE)

  # somewhere around here, we can extract sub-chapters:

  # actually, these are ICD-9-CM sub-chapters, but I think this is a superset of
  # ICD 9

  # either range or a single value (which overlaps with the majors definition)
  paste0("^[-()A-Z,[:space:]]+", "(", "[[:space:]]+\\(", "|", "\\(", ")",
         "(", re_icd9_major_strict_bare, ")",
         "(-(", re_icd9_major_strict_bare, "))?",
         "\\)") -> re_subchap_either

  paste0("^(", re_icd9_major_strict_bare, ") ") -> re_major_start

  filtered %>%
    str_subset(re_subchap_either) %>%
    chapter_to_desc_range.icd9 -> icd9_sub_chapters

  # The entire "E" block is incorrectly identified here, so make sure it is gone:
  icd9_sub_chapters["Supplementary Classification Of Factors Influencing Health Status And Contact With Health Services"] <- NULL
  icd9_sub_chapters["Supplementary Classification Of External Causes Of Injury And Poisoning"] <- NULL

  filtered %>%
    str_subset(re_major_start) %>%
    str_split_fixed(" ", n = 2) -> majors_matrix

  icd9_majors <- majors_matrix[, 1]

  cap_first <- function(name) {
    paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))
  }

  names(icd9_majors) <- majors_matrix[, 2] %>% str_trim %>% cap_first

  # this sub-chapter is simply missing from the otherwise consistent RTF way
  # 'major' types are reported:
  icd9_majors[["Place of occurrence"]] <- "E849"

  # There are some duplicates created by the major search, mostly E001 to E030
  # which are just listed twice in RTF. Also 199 (with punctuation difference),
  # 209 and 239.
  icd9_majors <- icd9_majors[!duplicated(icd9_majors)]

  if (save_extras) {
    save_in_data_dir(icd9_sub_chapters)
    save_in_data_dir(icd9_majors)
  }

  # this is so ghastly: find rows with sequare brackets containing definition of
  # subset of fourth or fifth digit codes. Need to pull code from previous row,
  # and create lookup, so we can exclude these when processing the fourth an
  # fifth digits
  re_qual_subset <- "\\[[-, [:digit:]]+\\]"
  qual_subset_lines <- grep(re_qual_subset, filtered)
  invalid_qual <- c()
  for (ql in qual_subset_lines) {
    # get prior code
    filtered[ql - 1] %>%
      str_match_all(paste0("(", re_icd9_decimal_bare, ") (.*)")) %>%
      unlist %>% extract2(2) -> code
    sb <- rtf_parse_qualifier_subset(filtered[ql])
    inv_sb <- setdiff(as.character(0:9), sb)
    if (length(inv_sb) == 0)
      next
    if (grepl("\\.", code))
      invalid_qual <- c(invalid_qual, paste0(code, inv_sb))
    else
      invalid_qual <- c(invalid_qual, paste0(code, ".", inv_sb))
  }

  # grab fifth digit ranges now:
  re_fifth_range_other <- "fifth +digit +to +identify +stage"
  re_fifth_range <- "ifth-digit subclas|fifth-digits are for use with codes"
  re_fifth_range_V30V39 <-
    "The following two fifths-digits are for use with the fourth-digit \\.0"
  re_fifth_rows <- paste(re_fifth_range, re_fifth_range_other, sep = "|")
  filtered %>% str_detect(re_fifth_rows) %>% which -> fifth_rows

  # several occurances of "Requires fifth digit", referring back to the previous
  # higher-level definition, without having the parent code in the line itself
  fifth_backref <- grep(re_fifth_range_other, filtered)
  # for these, construct a string which will be captured in the next block
  filtered[fifth_backref] <- paste(filtered[fifth_backref], filtered[fifth_backref - 1])

  # fourth-digit qualifiers:
  re_fourth_range <- "fourth-digit.+categor"
  fourth_rows <- grep(re_fourth_range, filtered)

  # lookup_fourth will contain vector of suffices, with names being the codes
  # they augment
  lookup_fourth <- c()
  for (f in fourth_rows) {
    if (verbose)
      message("working on fourth-digit row:", f)
    range <- rtf_parse_fifth_digit_range(filtered[f])
    filtered[seq(f + 1, f + 37)] %>%
      str_subset("^[[:digit:]][[:space:]].*") %>%
      str_pair_match("([[:digit:]])[[:space:]](.*)") -> fourth_suffices
    re_fourth_defined <- paste(c("\\.[", names(fourth_suffices), "]$"), collapse = "")
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

  # at least two examples of "Use 0 as fourth digit for category 672"
  re_fourth_digit_zero <- "Use 0 as fourth digit for category"
  fourth_digit_zero_lines <- grep(re_fourth_digit_zero, filtered)
  filtered[fourth_digit_zero_lines] %>%
    str_pair_match("(.*category )([[:digit:]]{3})$") %>%
    unname -> fourth_digit_zero_categories

  for (categ in fourth_digit_zero_categories) {
    parent_row <- grep(paste0("^", categ, " .+"), filtered, value = TRUE)
    filtered[length(filtered) + 1] <-
      paste0(categ, ".0 ", str_pair_match(parent_row, "([[:digit:]]{3} )(.+)"))
  }


  # lookup_fifth will contain vector of suffices, with names being the codes
  # they augment
  lookup_fifth <- c()
  for (f in fifth_rows) {
    if (verbose) message("working on fifth-digit row:", f)
    range <- rtf_parse_fifth_digit_range(filtered[f], verbose = verbose)
    fifth_suffices <- filtered[seq(f + 1, f + 20)] %>%
      str_subset("^[[:digit:]][[:space:]].*") %>%
      str_pair_match("([[:digit:]])[[:space:]](.*)", warn_pattern = TRUE)

    re_fifth_defined <- paste(c("\\.[[:digit:]][", names(fifth_suffices), "]$"), collapse = "")
    # drop members of range which don't have defined fifth digit
    range <- grep(re_fifth_defined, range, value = TRUE)
    # now replace value with the suffix, with name of item being the code itself
    names(range) <- range
    last <- -1
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
  re_V30V39_fifth <- "V3[[:digit:]]\\.0[01]$"

  lines_V30V39 <- grep(re_fifth_range_V30V39, filtered)
  stopifnot(length(lines_V30V39) == 1)
  filtered[seq(from = lines_V30V39 + 1, to = lines_V30V39 + 3)] %>%
    str_subset("^[[:digit:]][[:space:]].*") %>%
    str_pair_match("([[:digit:]])[[:space:]](.*)") -> suffices_V30V39
  range <- c("V30" %i9da% "V37", icd_children.icd9("V39", short_code = FALSE, defined = FALSE))
  range <- grep(re_V30V39_fifth, range, value = TRUE)
  names(range) <- range
  for (fifth in names(suffices_V30V39)) {
    # only applies to .0x (in 2015 at least), but .1 also exists without 5th
    # digit
    re_fifth <- paste0("\\.0", fifth, "$")
    range[grep(re_fifth, range)] <- suffices_V30V39[fifth]
  }
  lookup_fifth <- c(lookup_fifth, range)

  # now here we could potentially capture chapter headings, but I can drop
  # excludes easily by removing lines with bracketed codes
  filtered <- grep(paste0("\\((", re_icd9_decimal_bare, ")-(", re_icd9_decimal_bare, ")\\)"),
                   filtered, value = TRUE, invert = TRUE)
  filtered <- grep(paste0("Exclude"), filtered, value = TRUE, invert = TRUE)

  # fix some odd-balls so they don't get dropped
  # "707.02Upper back", "066.40West Nile fever, unspecified", etc

  filtered <- sub("((70[[:digit:]]\\.[[:digit:]]{2})|066\\.40)([[:alpha:]])", "\\1 \\2", filtered)

  ##################
  # next step is the main filter for codes
  ##################

  # again, we can keep some more information, but we'll just take the primary
  # description for each item, i.e. where a code begins a line. Some codes have
  # ten or so alternative descriptions, e.g. 410.0
  filtered <- grep(paste0("^[[:space:]]*(", re_icd9_decimal_strict_bare, ") "), filtered, value = TRUE)

  # spaces to single
  filtered <- gsub("[[:space:]]+", " ", filtered)
  # fix a few things, e.g. "040. 1 Rhinoscleroma", "527 .0 Atrophy"
  filtered <-
    sub("^([VvEe]?[[:digit:]]+) ?\\. ?([[:digit:]]) (.*)", "\\1\\.\\2 \\3",
        filtered)
  # and high-level headings like "210-229 Benign neoplasms"
  filtered <- grep("^[[:space:]]*[[:digit:]]{3}-[[:digit:]]{3}.*", filtered,
                   value = TRUE, invert = TRUE)
  # "2009 H1 N1 swine influenza virus"
  filtered <- grep("^2009", filtered, value = TRUE, invert = TRUE)
  # "495.7 \"Ventilation\" pneumonitis"
  re_code_desc <- paste0("^(", re_icd9_decimal_bare, ") +([ \"[:graph:]]+)")
  out <- str_pair_match(filtered, re_code_desc)

  out_fourth <- c()
  # apply fourth digit qualifiers
  for (f_num in seq_along(lookup_fourth)) {
    if (verbose)
      message("applying fourth digits to lookup row: ", f_num)
    lf <- lookup_fourth[f_num]
    f <- names(lf)
    parent_code <- icd_get_major.icd9(f, short_code = FALSE)
    if (parent_code %fin% names(out)) {
      pair_fourth <- paste(out[parent_code], lookup_fourth[f_num], sep = ", ")
      names(pair_fourth) <- f
      out_fourth <- append(out_fourth, pair_fourth)
    }
  }
  out <- append(out, out_fourth)

  out_fifth <- c()
  # apply fifth digit qualifiers:
  for (f_num in seq_along(lookup_fifth)) {
    if (verbose)
      message("applying fifth digits to lookup row: ", f_num)
    lf <- lookup_fifth[f_num]
    f <- names(lf)
    parent_code <- substr(f, 0, nchar(f) - 1)

    # repeated lookup in same table, so can benefit from fast match %fin%
    # instead of %in%
    if (parent_code %fin% names(out)) {
      # add just the suffix with name being the five digit code
      pair_fifth <- paste(out[parent_code], lf, sep = ", ")
      names(pair_fifth) <- f
      out_fifth <- append(out_fifth, pair_fifth)
    } else {
      # this is really superfluous since we don't expect to match these, keep
      # for debugging
      if (FALSE)
        message("parent code ", parent_code, " missing when looking up ", f)
    }
  }
  out <- append(out, out_fifth)

  # clean up duplicates (about 350 in 2015 data), mostly one very brief
  # description and a correct longer one; or, identical descriptions

  out[duplicated(names(out)) | duplicated(names(out), fromLast = TRUE)] %>%
    names %>%
    unique -> dupes

  for (d in dupes) {
    dupe_rows <- which(names(out) == d)
    if (all(out[dupe_rows[1]] == out[dupe_rows[-1]])) {
      out <- out[-dupe_rows[-1]]
      next
    }
    desclengths <- out[dupe_rows] %>% nchar
    longestlength <- desclengths %>% max
    if (verbose)
      message("removing differing duplicates: ", paste(out[dupe_rows]))
    out <- out[-dupe_rows[-which(desclengths != longestlength)]]
  }

  # drop all the codes not specified by 5th digits in square brackets, which are
  # applied over a range of codes.
  out <- out[-which(names(out) %in% invalid_qual)]

  # 2015 quirks (many more are baked into the parsing: try to splinter out the
  # most specific) some may well apply to other years

  # 650-659 ( and probably many others don't use whole subset of fourth or fifth
  # digit qualifiers) going to have to parse these, e.g. [0,1,3], as there are
  # so many...
  out <- out[grep("65[12356789]\\.[[:digit:]][24]", names(out), invert = TRUE)]

  #657 just isn't formatted like any other codes
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

#' parse a row of RTF source data for ranges to apply fifth digit
#'
#'   sub-classifications
#' returns all the possible 5 digit codes encompassed by the given
#'   definition. This needs to be whittled down to just those matching fifth
#'   digits, but we haven't parsed them yet.
#' @template verbose
#' @keywords internal
rtf_parse_fifth_digit_range <- function(row_str, verbose = FALSE) {
  assert_string(row_str)
  assert_flag(verbose)

  out <- c()
  # get numbers and number ranges
  row_str %>%
    str_split("[, :;]") %>%
    unlist %>%
    str_subset("[VvEe]?[0-9]") -> vals

  if (verbose)
    message("vals are:", paste(vals, collapse = ", "))

  # sometimes  we get things like:
  # [1] "345.0" ".1"    ".4-.9"
  grepl(pattern = "^\\.[[:digit:]]+.*", vals) -> decimal_start
  if (any(decimal_start)) {
    base_code <- vals[1] # assume first is the base
    stopifnot(icd_is_valid.icd9(base_code, short_code = FALSE))
    for (dotmnr in vals[-1]) {
      if (verbose)
        message("dotmnr is: ", dotmnr)
      if (grepl("-", dotmnr)) {
        # range of minors
        strsplit(dotmnr, "-", fixed = TRUE) %>% unlist -> pair
        first <- paste0(icd_get_major.icd9(base_code, short_code = FALSE), pair[1])
        last <- paste0(icd_get_major.icd9(base_code, short_code = FALSE), pair[2])
        if (verbose)
          message("expanding specified minor range from ", first, " to ", last)
        out <- c(out, first %i9da% last)
      } else {
        single <- paste0(icd_get_major.icd9(base_code, short_code = FALSE), dotmnr)
        out <- c(out, icd_children.icd9(single, short_code = FALSE, defined = FALSE))
      }
    }
    vals <- vals[1] # still need to process the base code
  }

  for (v in vals) {
    # take care of ranges
    if (grepl("-", v)) {
      v %>%  strsplit("-", fixed = TRUE) %>% unlist -> pair
      # sanity check
      stopifnot(all(icd_is_valid.icd9(pair, short_code = FALSE)))
      if (verbose)
        message("expanding explicit range ", pair[1], " to ", pair[2])
      # formatting errors can lead to huge range expansions, e.g. "8-679"

      # quickly strip off V or E part for comparison
      pair_one <- gsub("[^[:digit:]]", "", pair[1])
      pair_two <- gsub("[^[:digit:]]", "", pair[2])
      if (as.integer(pair_two) - as.integer(pair_one) > 10) {
        warning("probable formatting misinterpretation: huge range expansion")
      }

      out <- c(out, pair[1] %i9da% pair[2])
    } else {
      # take care of single values
      if (!icd_is_valid.icd9(v, short_code = FALSE))
        stop(paste("invalid code is: ",
                   icd_get_invalid.icd9(v, short_code = FALSE)))
      out <- c(out, icd_children.icd9(v, short_code = FALSE, defined = FALSE))
    }

  }
  out
}

rtf_parse_qualifier_subset <- function(qual) {
  assert_string(qual) # one at a time

  out <- c()

  qual %>% strip %>%
    strsplit("[]\\[,]") %>%
    unlist %>%
    #grep("[[:digit:]]", ., value = TRUE) %>%
    str_subset("[[:digit:]]") %>%
    strsplit(",") %>% unlist -> vals
  for (v in vals) {
    if (grepl("-", v)) {
      strsplit(v, "-") %>% unlist %>% as.integer -> pair
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
#' @param x vector of character strings containing RTF
#' @keywords internal
rtf_strip <- function(x) {
  x %>%
    # just for \tab, replace with space, otherwise, drop rtf tags entirely
    # nolint start
    str_replace_all("\\\\tab ", " ") %>%
    str_replace_all("\\\\[[:punct:]]", "") %>% # control symbols only, not control words
    str_replace_all("\\\\lsdlocked[ [:alnum:]]*;", "") %>% # special case, still needed?
    str_replace_all("\\{\\\\bkmkstart.*?\\}", "") %>%
    str_replace_all("\\{\\\\bkmkend.*?\\}", "") %>%
    # no backslash in this next list, others removed from
    # http://www.regular-expressions.info/posixbrackets.html
    str_replace_all("\\\\[-[:alnum:]]*[ !\"#$%&'()*+,-./:;<=>?@^_`{|}~]?", "") %>%
    str_replace_all(" *(\\}|\\{)", "") %>%
    # nolint end
    str_trim
}
