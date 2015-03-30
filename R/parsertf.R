# try parsing the RTF, and therefore get subheadings, as well as billable codes.
# ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/
#
# see https://github.com/LucaFoschini/ICD-9_Codes for a completely different approach in python
#
# setdiff(icd9ShortToDecimal(icd9Hierarchy$icd9), names(parseRtf()))

parseRtf <- function(lines = readLines(system.file("extdata", "Dtab12.rtf", package = "icd9"),
                                          encoding = "CP1252", warn = FALSE),
                     verbose = TRUE) {

  filtered <- lines
  #merge any line NOT starting with "\\par" on to previous line
  non_par_lines <- grep("^\\\\par", filtered, invert = TRUE)
  # in reverse order, put each non-par line on end of previous, then filter out all non-par lines
  for (i in rev(non_par_lines)) {
    filtered[i-1] <- paste(filtered[i-1], filtered[i])
  }
  filtered <- grep("^\\\\par", filtered, value = TRUE)

  # drop stupid long line at end:
  filtered <- filtered[-c(which(nchar(filtered) > 1000))]

  filtered %<>% stripRtf

  filtered <- grep("\\[[-[:digit:]]+\\]", filtered, value = TRUE, invert = TRUE) # references e.g. [0-6]
  filtered <- grep("^[[:space:]]*$", filtered, value = TRUE, invert = TRUE) # empty lines

  re_anycode <- "(([Ee]?[[:digit:]]{3})|([Vv][[:digit:]]{2}))(\\.[[:digit:]]{1,2})?"

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
  # and "210-229 Benign neoplasms"
  filtered <- grep("[[:digit:]]{3}-[[:digit:]]{3}.*", filtered, value = TRUE, invert = TRUE)

  # "707.02Upper back"
  filtered <- sub("([[:digit:]])([[:alpha:]])", "\\1 \\2", filtered)

  # "2009 H1 N1 swine influenza virus"

  filtered <- grep("^2009", filtered, value = TRUE, invert = TRUE)

  # "495.7 \"Ventilation\" pneumonitis"

  re_code_desc <- paste0("^(", re_anycode, ") +([ \"[:alnum:]]+)")
  out <- strPairMatch(re_code_desc, filtered, pos = c(1, 6))

}





crap <- function() {


  # some regexes. It is not going to completely validate every possible code,
  # e.g. posn of decimal with E, but should be 100% sensitive.
  re_anycodeatend <- paste0(re_anycode, "\\\\*tab *$")

  # now strip the RTF, find the ICD-9 codes, then group with the descriptions
  alllines %>% stripRtf %>%
    # drop leading spaces
    gsub("^ +", "", .) %>%
    #strMultiMatch(" *([VvEe]?[0-9]{1,3}(\\.[0-9]{1,2})) (.+)", ., dropEmpty = TRUE) -> out_list
    strMultiMatch(paste0(".*?(", re_anycode, ") (.+)"), ., dropEmpty = TRUE) -> out_list
  # ungroup the code from the description
  out <- vapply(out_list, "[", FUN.VALUE = character(1), 6)
  names(out) <- vapply(out_list, "[", FUN.VALUE = character(1), 1)

  # capture TB fifth digit: alllines[c(673:682)]
  # range is delimited by:
  # # first a space then number, desc is at end of row (?always)
  # # line after last line has a close brace
  rows_starting_fifth_list <- grep("fifth-digit subclassification", alllines, ignore.case = TRUE)
  # the string contains either a comma separated list of codes, comma-separated
  # list of ranges, a single code, and the codes may or may not have decimal
  # places
  out_fifth <- c()
  for (sr in rows_starting_fifth_list) {
    row_str <- alllines[sr]
    out_possible <- parseRtfFifthDigitRanges(row_str, verbose = verbose)
    out_possible_five <- out_possible[nchar(out_possible) == 6] # five + decimal point
    if (verbose) {
      message("possible 5-digit codes")
      print(out_possible_five)
    }

    # first look ahead until we get a number-desc pair:
    re_numSubDesc <- "([[:digit:]]) +([[:alpha:][:punct:]])+"
    srn <- sr
    # can't just advance until we find something good, because the first line may be split in the middle!!
    #while(!grepl(re_numSubDesc, stripRtf(alllines[srn])))
    #      srn = srn + 1
    while(stripRtf(alllines[srn]) != "") {
      if (verbose) message("looking forward for first fifth desc row, at: ", srn)
      srn = srn + 1L
    }
    srn = srn + 1L
    # now look ahead in each row until we can't parse "number additional_desc" pair
    lookup_fifth <- c()
    #while (alllines[srn] %>% stripRtf %>% grepl(re_numSubDesc, .)) {
    while (alllines[srn] %>% stripRtf != "") {
      # annoyingly, like much of this RTF parsing (which would be unnecessary if
      # the CMS published machine readable data), sometimes the fifth digit
      # description flows onto the next line. It seems each description is
      # terminated by \par, so we merge lines then drop everything after the first par
      #complete_row <- gsub(".+\\\\par.*", "", alllinespairs[srn])

      line <- alllines[srn] %>% stripRtf
      split_fifth_line <- !grepl(re_numSubDesc, line)
      if (verbose) message("using line: ", line)

      lookup_row <- strPairMatch(re_numSubDesc, line)
      if (is.na(lookup_row)) {
        lookup_row <- paste("**************** PARSING QUIRK AT ROW ", srn)
        warning(lookup_row)
      }
      if (verbose) print(lookup_row)
      lookup_fifth <- c(lookup_fifth, lookup_row)
      srn = srn + 1
    } # next row in this block of fifth digit suffixes
    if (length(lookup_fifth) == 0)
      stop("Failed to get any fifth-digit definitions starting at line ", sr)

    # now the name of the result is the 5th digit. we drop all the five-digit
    # codes in the range that don't match the fifth digit, then append the descriptions for the matches against this list

    # out_possible is the subset of codes which contain the codes with appended
    # fifth digit, for the current range, and extraneous ones. Filter only the
    # real ones, then match them to append the descriptions
    re_fifth_defined <- paste(c("[", names(lookup_fifth), "]$"), collapse = "")
    if (verbose) message("re_fifth_defined = ", re_fifth_defined)
    if (length(names(lookup_fifth)) == 0) {
      warning("didn't get list of fifth digits to lookup")
      browser()
    }
    out_real_five <- grep(re_fifth_defined, out_possible_five, value = TRUE)
    out_fifth <- c()
    for (i in seq_along(out_real_five)) {
      #if (verbose) message("getting fifth digit desc for ", i)
      fifth_char <- substr(i, nchar(i), nchar(i))
      first_four <- substr(i, 1, nchar(i) - 1)
      parent_desc <- out[first_four]
      val <- paste(parent_desc, lookup_fifth[fifth_char])
      names(val) <- i
      out_fifth <- c(out_fifth, val)
    }
  }

  # TODO: sort on name
  c(out, out_fifth)
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
  row_str %>% stripRtf %>% strsplit("[, :;]")  %>% unlist %>% grep("[0-9]", ., value = T) -> vals
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
        if (verbose) message("expanding range ", first, " to ", last)
        out <- c(out, first %i9da% last)
      } else {
        single <- paste0(icd9GetMajor(base_code, isShort = FALSE), dotmnr)
        out <- c(out, icd9ChildrenDecimal(single, onlyReal = FALSE))
      }
    }
    return(out)
  }

  for (v in vals) {
    # take care of ranges
    if (grepl("-", v)) {
      v %>%  strsplit("-", fixed = TRUE) %>% unlist -> pair
      # sanity check
      stopifnot(icd9IsValidDecimal(pair[1]), icd9IsValidDecimal(pair[2]))
      if (verbose) message("expanding explicit range ", pair[1], " to ", pair[2])
      # formatting errors can lead to huge range expansions, e.g. "8-679"
      if (as.integer(pair[2]) - as.integer(pair[1]) > 10)
        stop("probable formatting misinterpretation because huge range expansion is requested")

      out <- c(out, pair[1] %i9da% pair[2])
    } else {
      # take care of single values
      if (!icd9IsValidDecimal(v)) stop(icd9GetInvalidDecimal(v))
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

#' @title try to parse a fifth digit definition from raw RTF
#' @description if there is no name/desc pair, then return NA or "" or something.
#' @examples
#' library(magrittr)
#' # "\\rtlch\\fcs1 \\af1\\afs20\\alang1025 \\ltrch\\fcs0 \\fs20\\cf1\\lang1033\\langfe1033\\loch\\af1\\hich\\af1\\dbch\\af31505\\cgrid\\langnp1033\\langfenp1033 {\\rtlch\\fcs1 \\ab\\af1 \\ltrch\\fcs0 \\b\\insrsid2429293 0\\tab \\hich\\af1\\dbch\\af31505\\loch\\f1 unspecified" %>% parseFifthDigitDef
#'
#' "\\par \\hich\\af1\\dbch\\af31505\\loch\\f1 3\\tab tubercle bacilli found (in sputum) by microscopy" %>% parseFifthDigitDef
#' @keywords internal
parseFifthDigitDef <- function(x) {
  stripped <- stripRtf(x)
  n <- substr(stripped, 1, 1)
  desc <- substr(stripped, 2, nchar(stripped))
  c(n, trim(desc))
}

parseUnrtf <- function(alllines = readLines(system.file("extdata", "Dtab12.unrtf.txt", package = "icd9"),
                                            encoding = "CP1252", warn = FALSE),
                       verbose = TRUE) {
  # don't bother with vectorization, as there are so many position dependent decisions?
  # some things can be filtered quickly:
  re_anycode <- "(([Ee]?[[:digit:]]{3})|([Vv][[:digit:]]{2}))(\\.[[:digit:]]{1,2})?"
  # excludes e.g. positive PPD (795.51), but also gets chapter and subchapter
  # headings, which I don't think we need to keep at this point.
  filtered <- alllines
  #filtered <- filtered[-c(grep("Excludes:", filtered))]
  filtered <- filtered[-c(grep(paste0("\\((", re_anycode, ")+[-[:digit:]]*\\)"), filtered))] # this gets some middle level headings, and some exclude ranges
  filtered <- filtered[-c(grep("\\[[-[:digit:]]+\\]", filtered))] # references e.g. [0-6]
  filtered <- filtered[-c(grep("^[[:space:]]*$", filtered))] # empty lines
  # header
  filtered <- filtered[-c(grep("^----*", filtered))]
  filtered <- filtered[-c(grep("^##*", filtered))]
  filtered <- filtered[-c(grep("^AUTHOR.*", filtered))]
  filtered <- filtered[-c(grep("^TITLE.*", filtered))]
  filtered <- filtered[-c(grep("^CLASSIFICATION OF DISEASES AND INJURIES.*", filtered))]

  filtered <- trim(filtered)

  parsed <- c()
  n <- 1
  while (n <= length(filtered)) {
    message("loop, n = ", n)
    # skip any textual sections. Exlucdes easier because they all have bracketed codes.
    if (grepl("Includes|Excludes|Note", filtered[n])) {
      while(!grepl(paste0("^", re_anycode), filtered[n]))
        n <- n + 1
    }
    # now n should point to a row starting with a code, next line should be description
    code <- filtered[n]
    if (verbose) message("working on code ", code, " at line ", n)
    desc <- c()
    n <- n + 1
    while (!grepl(paste0("^", re_anycode), filtered[n])) {
      if (verbose) message("working on part ", filtered[n], " at line ", n)
      desc <- paste(desc, filtered[n])
      n <- n + 1
    }
    if (is.null(desc))
      stop("got NULL with code ", code, " at line ", n)

    names(desc) <- code
    parsed <- c(parsed, pair)
  }
}
