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
#' @param sasTxt is a character vector, with one item per line, e.g. from
#'   \code{readLines}
#' @examples
#'   \dontrun{
#'   sasFormatExtract(readLines('inst/extdata//comformat2012-2013.txt'))
#'   }
#' @references
#' \url{http://support.sas.com/documentation/cdl/en/proc/61895/HTML/default/viewer.htm#a002473474.htm}
#' \url{https://communities.sas.com/thread/47571?start=0&tstart=0}
#' \url{https://communities.sas.com/message/165945}
#' @return list (of lists)
#' @keywords programming list internal
sasFormatExtract <- function(sasTxt) {

  # collapse everything onto one big line, so we can filter multi-line
  # commments. No ability to do multiline regex along a vector.
  sasTxt <- paste(sasTxt, collapse = " \\n")

  # sas comments are in the form /* ... */ inline/multiline, or * ... ;
  sasTxt <- gsub(pattern = "/\\*.*?\\*/", replacement = "", x = sasTxt) # nolint
  sasTxt <- gsub(pattern = "\\n\\*.*?;", replacement = "\\n", x = sasTxt) # nolint

  sasTxt <- strsplit(sasTxt, split = "\\;")[[1]]

  #strip whitespace and ?undetected newline characters, replace with single
  #spaces.
  sasTxt <- gsub(pattern = "\\\\n", "", sasTxt) # nolint
  sasTxt <- gsub(pattern = "[[:space:]]+", " ", sasTxt)
  sasTxt <- trim(sasTxt)

  # drop everything except VALUE statements
  sasTxt <- grep(pattern = "^VALUE.*", x = sasTxt, value = TRUE)

  # put each VALUE declaration in a vector element
  allAssignments <- strMultiMatch(
    pattern = "^VALUE[[:space:]]+([[:graph:]]+)[[:space:]]+(.+)[[:space:]]*$",
    text = sasTxt)

  out <- list()

  for (m in allAssignments) {
    out[m[[1]]] <- list(sasParseAssignments(m[[2]]))
  }

  out
}

#' @title get assignments from a character string strings.
#' @description   #form is aaa-bbb, ccc-ddd, eee, etc. = "name" abc-def, ghi,
#'   etc. = "anothername" there is no delimiter between each assignment. '
#' @param x is a character string containing space delimited assignments, in SAS
#'   declaration format.
#' @param stripWhiteSpace will strip all whitespace from the returned values
#' @param stripQuotes will strip all double quotation marks from the returned
#'   values
#' @return list with each list item containing a matrix of "char ranges",
#'   "assigned value" pairs
#' @keywords internal programming list
sasParseAssignments <- function(x, stripWhiteSpace = TRUE, stripQuotes = TRUE) {

  assertString(x)
  assertFlag(stripWhiteSpace)
  assertFlag(stripQuotes)
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
    if (stripWhiteSpace) halfway <- gsub(pattern = "[[:space:]]*",
                                         replacement = "",
                                         halfway)
    if (stripQuotes) halfway <- gsub(pattern = '"', replacement = "", halfway)
    out <- list()
    out[[halfway[[2]]]] <- unlist(strsplit(x = halfway[[1]], split = ","))
    return(out)
  }

  threequarters <- c(
    halfway[[1]],
    unlist(
      strMultiMatch(
        pattern = '^([^"]|"[^"]*")*? (.*)',
        text = halfway[seq(2, length(halfway) - 1)]
      )
    ),
    halfway[[length(halfway)]]
  )

  if (stripQuotes) threequarters <- gsub(pattern = '"',
                                         replacement = "",
                                         threequarters)

  #spaces may matter still, so don't randomly strip them?


  out <- list()
  for (pair in seq(from = 1, to = length(threequarters), by = 2)) {
    if (stripWhiteSpace) {
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
  a <- strMultiMatch(
    "%LET ([[:alnum:]]+)[[:space:]]*=[[:space:]]*%STR\\(([[:print:]]+?)\\)",
    text = x, dropEmpty = TRUE)
  vls <- vapply(a, FUN = function(x) x[[2]], FUN.VALUE = "")
  splt <- strsplit(vls, split = ",")
  result <- lapply(splt, strip, pattern = "'") # strip single quotes
  result <- lapply(result, strip, pattern = '"') # strip double quotes
  names(result) <- vapply(a, FUN = function(x) x[[1]], FUN.VALUE = "")
  result
}
