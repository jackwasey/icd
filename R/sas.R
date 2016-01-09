# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

#' @title extract assignments from a SAS FORMAT definition.
#' @description this is modelled entirely on a single chunk of SAS code, but
#'   hopefully will have some generalizability. It relies heavily on lists and
#'   regex, but, as you will see from the code, R is not a great language with
#'   which to write a SAS parser.
#' @param sas_lines is a character vector, with one item per line, e.g. from
#'   \code{readLines}
#' @examples
#'   \dontrun{
#'   sas_format_extract(readLines('data-raw/comformat2012-2013.txt'))
#'   }
#' @references
#' \url{http://support.sas.com/documentation/cdl/en/proc/61895/HTML/default/viewer.htm#a002473474.htm}
#' \url{https://communities.sas.com/thread/47571?start=0&tstart=0}
#' \url{https://communities.sas.com/message/165945}
#' @return list (of lists)
#' @keywords programming list internal
sas_format_extract <- function(sas_lines) {

  # collapse everything onto one big line, so we can filter multi-line
  # commments. No ability to do multiline regex along a vector.
  sas_lines <- paste(sas_lines, collapse = " \\n")

  # sas comments are in the form /* ... */ inline/multiline, or * ... ;
  sas_lines <- gsub(pattern = "/\\*.*?\\*/", replacement = "", x = sas_lines) # nolint
  sas_lines <- gsub(pattern = "\\n\\*.*?;", replacement = "\\n", x = sas_lines) # nolint

  sas_lines <- strsplit(sas_lines, split = "\\;")[[1]]

  #strip whitespace and ?undetected newline characters, replace with single
  #spaces.
  sas_lines <- gsub(pattern = "\\\\n", "", sas_lines) # nolint
  sas_lines <- gsub(pattern = "[[:space:]]+", " ", sas_lines)
  sas_lines <- str_trim(sas_lines)

  # drop everything except VALUE statements
  sas_lines <- grep(pattern = "^VALUE.*", x = sas_lines, value = TRUE)

  # put each VALUE declaration in a vector element
  all_sas_assignments <- str_match_all(
    pattern = "^VALUE[[:space:]]+([[:graph:]]+)[[:space:]]+(.+)[[:space:]]*$",
    string = sas_lines) %>% lapply(`[`, c(2, 3))

  out <- list()

  for (m in all_sas_assignments) {
    out[m[[1]]] <- list(sasParseAssignments(m[[2]]))
  }

  out
}

#' @title get assignments from a character string strings.
#' @description   #form is aaa-bbb, ccc-ddd, eee, etc. = "name" abc-def, ghi,
#'   etc. = "anothername" there is no delimiter between each assignment. '
#' @param x is a character string containing space delimited assignments, in SAS
#'   declaration format.
#' @param strip_whitespace will strip all whitespace from the returned values
#' @param strip_quotes will strip all double quotation marks from the returned
#'   values
#' @return list with each list item containing a matrix of "char ranges",
#'   "assigned value" pairs
#' @keywords internal programming list
sasParseAssignments <- function(x, strip_whitespace = TRUE, strip_quotes = TRUE) {
  assertString(x)
  assertFlag(strip_whitespace)
  assertFlag(strip_quotes)
  # splitting with clever regex to separate each pair of assignments seems
  # tricky, so doing it in steps.
  # n.b. this is a list with list per input row.
  halfway <- as.list(unlist(
    strsplit(x, split = "[[:space:]]*=[[:space:]]*")
  ))

  # we need to match the first unquoted space to get the boundary between the
  # previous definition and the next variable name
  if (length(halfway) == 2) {
    # we have just a single name value pair so just set name to value and return
    # list of one item.
    if (strip_whitespace) halfway <- gsub(pattern = "[[:space:]]*",
                                         replacement = "",
                                         halfway)
    if (strip_quotes) halfway <- gsub(pattern = '"', replacement = "", halfway)
    out <- list()
    out[[halfway[[2]]]] <- unlist(strsplit(x = halfway[[1]], split = ","))
    return(out)
  }

  threequarters <- c(halfway[[1]],
                     halfway[seq(2, length(halfway) - 1)] %>%
                       str_match_all(pattern = '^([^"]|"[^"]*")*? (.*)') %>%
                       lapply(`[`, -1) %>%
                       unlist,
                     halfway[[length(halfway)]])

  if (strip_quotes)
    threequarters <- gsub(pattern = '"', replacement = "", threequarters)

  #spaces may matter still, so don't randomly strip them?


  out <- list()
  for (pair in seq(from = 1, to = length(threequarters), by = 2)) {
    if (strip_whitespace) {
      outwhite <- gsub(pattern = "[[:space:]]*",
                       replacement = "",
                       threequarters[pair])
    } else {
      outwhite <- threequarters[pair]
    }
    out[[threequarters[pair + 1]]] <- unlist(strsplit(x = outwhite,
                                                      split = ","))
  }
  out
}

#' @title drop superfluous assignment name when the name is already defined.
#' @description this is happening in the DRG definitions for AHRQ comorbidities.
#'   We have data like: "HTNDRG" 079,305 = "YES" I would like this to be
#'   list(HTNDRG="079,305")
#' @param x in this case is '079,305 = "YES"' (quotes may be present in the
#'   string itself)
#' @keywords internal manip util
sasDropOtherAssignment <- function(x) {
  stopifnot(sapply(regmatches(x, gregexpr("=", x)), length) == 1)
  lapply(x, function(y) strsplit(y, split = "[[:space:]]*=")[[1]][1])
}

#' @title extract quoted or unquoted SAS string definitions
#' @description Finds all the LET statements in some SAS code and writes them to
#'   an R list. The list item name is the SAS variable name, and each list item
#'   is a character vector of the contents. This is specifically for string
#'   assignements, but probably easy to adapter to numbers if ever needed.
#' @param x is a vector of character strings, typically taken from something
#'   like \code{readLines(someSasFilePath)}
#' @keywords internal programming list
sasExtractLetStrings <- function(x) {
  a <- x %>% str_match_all(
    "%LET ([[:alnum:]]+)[[:space:]]*=[[:space:]]*%STR\\(([[:print:]]+?)\\)")

  # drop empty elements after matching
  a <- a[vapply(a, FUN = function(x) length(x) != 0, FUN.VALUE = logical(1))]

  vls <- vapply(a, FUN = `[[`, 3, FUN.VALUE = "")
  splt <- strsplit(vls, split = ",")
  result <- lapply(splt, strip, pattern = "'") # strip single quotes
  result <- lapply(result, strip, pattern = '"') # strip double quotes
  names(result) <- vapply(a, FUN = function(x) x[[2]], FUN.VALUE = "")
  result
}

# horrible kludge for difficult source data
icd9ExpandRangeForSas <- function(start, end) {
  if (end == "0449") end <- start # HIV codes changed
reals <- icd_expand_range.icd9(start, end, short_code = TRUE, defined = TRUE,
                                # hmmm, maybe get the diff and test all children of ambigs present later
                                ex_ambig_start = FALSE,
                                ex_ambig_end = TRUE)
  real_parents <- icd_condense.icd9(reals, defined = TRUE, short_code = TRUE)
  merged <- unique(c(reals, real_parents))
  real_parents_of_merged <- icd_condense.icd9(merged, defined = TRUE, short_code = TRUE)
  halfway <- icd_children.icd9(real_parents_of_merged, defined = FALSE, short_code = TRUE)
  nonrealrange <- icd_expand_range.icd9(start, end, defined = FALSE, short_code = TRUE,
                                       ex_ambig_start = TRUE,
                                       ex_ambig_end = TRUE)
  icd_sort.icd9(unique(c(halfway, nonrealrange)), short_code = TRUE)
}
