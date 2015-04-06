# try parsing the RTF, and therefore get subheadings, as well as billable codes.
# ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/
#
# see https://github.com/LucaFoschini/ICD-9_Codes for a completely different approach in python
#
# setdiff(icd9ShortToDecimal(icd9Hierarchy$icd9), names(parseRtf()))
. <- "magrittr" # paper over rstudio warnings
parseRtf <- function(lines = readLines(system.file("extdata", "Dtab12.rtf", package = "icd9"),
                                       encoding = "CP1252", warn = FALSE),
                     verbose = FALSE) {

  filtered <- lines
  # merge any line NOT starting with "\\par" on to previous line
  non_par_lines <- grep("^\\\\par", filtered, invert = TRUE)
  # in reverse order, put each non-par line on end of previous, then filter out all non-par lines
  for (i in rev(non_par_lines)) {
    filtered[i-1] <- paste(filtered[i-1], filtered[i], sep = "")
  }
  filtered <- grep("^\\\\par", filtered, value = TRUE)

  # drop stupid long line at end:
  longest_lines <- nchar(filtered) > 3000
  # if none, then -c() returns no rows, so we have to test first
  if (any(longest_lines))
    filtered <- filtered[-c(which(longest_lines))]

  filtered %<>% stripRtf

  filtered <- grep("\\[[-[:digit:]]+\\]", filtered, value = TRUE, invert = TRUE) # references e.g. [0-6]
  filtered <- grep("^[[:space:]]*$", filtered, value = TRUE, invert = TRUE) # empty lines

  re_anycode <- "(([Ee]?[[:digit:]]{3})|([Vv][[:digit:]]{2}))(\\.[[:digit:]]{1,2})?"

  # grab fifth digit ranges now:
  re_fifth_range_other <- "fifth +digit +to +identify +stage"
  re_fifth_range <- "ifth-digit subclas|fifth-digits are for use with codes"
  re_fifth_range_V30V39 <- "The following two fifths-digits are for use with the fourth-digit \\.0"
  fifth_rows <- grep(paste(re_fifth_range, re_fifth_range_other, sep = "|"), filtered)

  # several occurances of "Requires fifth digit", referring back to the previous
  # higher-level definition, without having the parent code in the line itself
  fifth_backref <- grep(re_fifth_range_other, filtered)
  # for these, construct a string which will be captured in the next block
  filtered[fifth_backref] <- paste(filtered[fifth_backref], filtered[fifth_backref - 1], sep = " ")

  # fourth-digit qualifiers:
  re_fourth_range <- "fourth-digit.+categor"
  fourth_rows <- grep(re_fourth_range, filtered)

  # lookup_fourth will contain vector of suffices, with names being the codes they augment
  lookup_fourth <- c()
  for (f in fourth_rows) {
    if (verbose) message("working on fourth-digit row:", f)
    range <- parseRtfFifthDigitRanges(filtered[f])
    #if ("V30" %in% range) browser()
    filtered[seq(f + 1, f + 37)] %>%
      grep("^[[:digit:]][[:space:]].*", ., value = TRUE) %>%
      strPairMatch("([[:digit:]])[[:space:]](.*)", .) -> fourth_suffices
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
   strPairMatch("(.*category )([[:digit:]]{3})$", filtered[fourth_digit_zero_lines]) %>%
     unname -> fourth_digit_zero_categories

   for (categ in fourth_digit_zero_categories) {
     parent_row <- grep(paste0("^", categ, " .+"), filtered, value = TRUE)
     filtered[length(filtered)+ 1] <- paste0(categ, ".0 ", strPairMatch("([[:digit:]]{3} )(.+)", parent_row))
   }


  # lookup_fifth will contain vector of suffices, with names being the codes they augment
  lookup_fifth <- c()
  for (f in fifth_rows) {
    if (verbose) message("working on fifth-digit row:", f)
      range <- parseRtfFifthDigitRanges(filtered[f], verbose = verbose)
    # if ("941.00" %in% range) browser()
    filtered[seq(f + 1, f + 20)] %>%
        grep("^[[:digit:]][[:space:]].*", ., value = TRUE) %>%
        strPairMatch("([[:digit:]])[[:space:]](.*)", .) -> fifth_suffices

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

  # TODO: V30-39 are a special case because combination of both fourth and fifth digits are specified
  re_V30V39 <- "V3[[:digit:]]\\.((0[01]?$)|(1$)|(2$))"
  re_V30V39_fifth <- "V3[[:digit:]]\\.0[01]$"

  lines_V30V39 <- grep(re_fifth_range_V30V39, filtered)
  stopifnot(length(lines_V30V39) == 1)
  filtered[(lines_V30V39 + 1):(lines_V30V39 + 3)] %>%
    grep("^[[:digit:]][[:space:]].*", ., value = TRUE) %>%
    strPairMatch("([[:digit:]])[[:space:]](.*)", .) -> suffices_V30V39
  re_fifth_definedV3039 <- paste(c("\\.[[:digit:]][", names(fifth_suffices), "]$"), collapse = "")
  range <- c("V30" %i9da% "V37", icd9ChildrenDecimal("V39"))
  range <- grep(re_V30V39_fifth, range, value = TRUE)
  names(range) <- range
  for (fifth in names(suffices_V30V39)) {
    re_fifth <- paste0("\\.0", fifth, "$") # only applies to .0x (in 2015 at least), but .1 also exists without 5th digit
    range[grep(re_fifth, range)] <- suffices_V30V39[fifth]
  }
  lookup_fifth <- c(lookup_fifth, range)

  # now here we could potentially capture chapter headings, but I can drop
  # excludes easily by removing lines with bracketed codes
  filtered <- grep(paste0("\\((", re_anycode, ")+[-[:digit:]]*\\)"), filtered, value = TRUE, invert = TRUE)
  filtered <- grep(paste0("Exclude"), filtered, value = TRUE, invert = TRUE)
  # again, we can keep some more information, but we'll just take the primary
  # description for each item, i.e. where a code begins a line. Some codes have
  # ten or so alternative descriptions, e.g. 410.0
  filtered <- grep(paste0("^[[:space:]]*", re_anycode), filtered, value = TRUE)
  # spaces to single
  filtered <- gsub("[[:space:]]+", " ", filtered)
  # fix a few things, e.g. "040. 1 Rhinoscleroma", "527 .0 Atrophy"
  filtered <- sub("^([VvEe]?[[:digit:]]+) ?\\. ?([[:digit:]]) (.*)", "\\1\\.\\2 \\3", filtered)
  # and high-level headings like "210-229 Benign neoplasms"
  filtered <- grep("^[[:space:]]*[[:digit:]]{3}-[[:digit:]]{3}.*", filtered, value = TRUE, invert = TRUE)
  # "707.02Upper back"
  filtered <- sub("([[:digit:]])([[:alpha:]])", "\\1 \\2", filtered)
  # "2009 H1 N1 swine influenza virus"
  filtered <- grep("^2009", filtered, value = TRUE, invert = TRUE)
  # "495.7 \"Ventilation\" pneumonitis"
  re_code_desc <- paste0("^(", re_anycode, ") +([ \"[:graph:]]+)")
  out <- strPairMatch(re_code_desc, filtered, pos = c(1, 6))

  # apply fourth digit qualifiers
  for (f in names(lookup_fourth)) {
    if (verbose) message("applying fourth digits to: ", f)
    parent_code <- icd9GetMajor(f, isShort = FALSE)
    if (parent_code %in% names(out)) {
      out <- c(out, lookup_fourth[f])
      out[f] <- paste(out[parent_code], lookup_fourth[f], sep = ", ")
    }
  }

  # apply fifth digit qualifiers:
  for (f in names(lookup_fifth)) {
    if (verbose) message("applying fifth digits to: ", f)
    #if (f == "345.00") browser()
    parent_code <- substr(f, 0, nchar(f) - 1)

    if (parent_code %in% names(out)) {
      # add just the suffix with name being the five digit code
      out <- c(out, lookup_fifth[f])
      # then update to have the parent code in description
      out[f] <- paste(out[parent_code], lookup_fifth[f], sep = ", ")
    }
  }

  # 2015 quirks (many more are baked into the parsing: try to splinter out the most specific)
  # some may well apply to other years

  # 650-659 ( and probably many others don't use whole subset of fourth or fifth digit qualifiers)
  # TODO: going to have to parse these, e.g. [0,1,3], as there are so many...
  out <- out[grep("65[12356789]\\.[[:digit:]][24]", names(out), invert = TRUE)]

  #657 just isn't formatted like any other codes
  out["657.0"] <- "Polyhydramnios"
  out["657.00"] <- "Polyhydramnios, unspecified as to episode of care or not applicable"
  out["657.01"] <- "Polyhydramnios, delivered, with or without mention of antepartum condition"
  out["657.03"] <- "Polyhydramnios, antepartum condition or complication"

  out["719.69"] <- "Other symptoms referable to joint, multiple sites"
  out["807.19"] <- "Open fracture of multiple ribs, unspecified"
  out[order(names(out))]
}

#' @title parse a row of RTF source data for ranges to apply fifth digit
#'   sub-classifications
#' @description returns all the possible 5 digit codes encompassed by the given
#'   definition. This needs to be whittled down to just those matching fifth
#'   digits, but we haven't parsed them yet.
#' @examples
#' row_str <- "The following fifth-digit subclassification is for use with category 493.0-493.2, 493.9:"
#' parseRtfFifthDigitRanges(row_str)
#'
#' row_str <- "The following fifth-digit subclassification is for use with categories 67\\hich\\af1\\dbch\\af31505\\loch\\f1 8-679 to denote the current episode of care:"
#' parseRtfFifthDigitRanges(row_str)
#'
#' row_str <- "The following fifth-digit subclassification is for use with category 711; valid digits are in [brackets] under each code. see list at beginning of chapter for definitions:"
#' parseRtfFifthDigitRanges(row_str)
#' @keywords internal
parseRtfFifthDigitRanges <- function(row_str, verbose = FALSE) {
  out <- c()
  # get numbers and number ranges
  row_str %>% strsplit("[, :;]") %>% unlist %>% grep("[VvEe]?[0-9]", ., value = TRUE) -> vals
  if (verbose) { message("vals are:"); print(vals) }
  # sometimes  we get things like:
  # [1] "345.0" ".1"    ".4-.9"
  grepl(pattern = "^\\.[[:digit:]]+.*", vals) -> decimal_start
  if (any(decimal_start)) {
    base_code <- vals[1] # assume first is the base
    stopifnot(icd9IsValidDecimal(base_code))
    for (dotmnr in vals[-1]) {
      if (verbose) message("dotmnr is: ", dotmnr)
      if (grepl("-", dotmnr)) {
        # range of minors
        dotmnr %>%  strsplit("-", fixed = TRUE) %>% unlist -> pair
        first <- paste0(icd9GetMajor(base_code, isShort = FALSE), pair[1])
        last <- paste0(icd9GetMajor(base_code, isShort = FALSE), pair[2])
        if (verbose) message("expanding specified minor range from ", first, " to ", last)
        out <- c(out, first %i9da% last)
      } else {
        single <- paste0(icd9GetMajor(base_code, isShort = FALSE), dotmnr)
        out <- c(out, icd9ChildrenDecimal(single, onlyReal = FALSE))
      }
    }
    #return(out)
    vals <- vals[1] # still need to process the base code
  }

  for (v in vals) {
    # take care of ranges
    if (grepl("-", v)) {
      v %>%  strsplit("-", fixed = TRUE) %>% unlist -> pair
      # sanity check
      stopifnot(all(icd9IsValidDecimal(pair)))
      if (verbose) message("expanding explicit range ", pair[1], " to ", pair[2])
      # formatting errors can lead to huge range expansions, e.g. "8-679"

      # quickly strip off V or E part for comparison
      pair_one <- gsub("[^[:digit:]]", "", pair[1])
      pair_two <- gsub("[^[:digit:]]", "", pair[2])
      if (as.integer(pair_two) - as.integer(pair_one) > 10) {
        warning("probable formatting misinterpretation because huge range expansion is requested")
      }

      out <- c(out, pair[1] %i9da% pair[2])
    } else {
      # take care of single values
      if (!icd9IsValidDecimal(v)) stop(paste("invalid code is: ", icd9GetInvalidDecimal(v)))
      out <- c(out, icd9ChildrenDecimal(v))
    }

  }

  out
}

#' Strip RTF
#'
#' Take a vector of character strings containing RTF, replace each \tab with a
#' space and eradicate all other RTF symbols
#' @param x vector of character strings containing RTF
#' @keywords internal
stripRtf <- function(x) {
  x %>%
    # just for \tab, replace with space, otherwise, drop rtf tags entirely
    gsub("\\\\tab ", " ", .) %>%
    gsub("\\\\[[:punct:]]", "", .) %>% # control symbols only, not control words
    gsub("\\\\lsdlocked[ [:alnum:]]*;", "", .) %>% # special case, still needed?
    #gsub("\\\\[-[:alnum:]]+[ ;:,.]?", "", .) %>%
    gsub("\\{\\\\bkmkstart.*?\\}", "", .) %>%
    gsub("\\{\\\\bkmkend.*?\\}", "", .) %>%
    #gsub("\\\\[[:alnum:]]*[ [:punct:]]", "", .) %>%
    gsub("\\\\[-[:alnum:]]*[ !\"#$%&'()*+,-./:;<=>?@^_`{|}~]?", "", .) %>% # no backslash in this list, others removed from http://www.regular-expressions.info/posixbrackets.html
    gsub(" *(\\}|\\{)", "", .) %>%
    trim
}
