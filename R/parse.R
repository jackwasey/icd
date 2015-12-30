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

# nocov start

# data_sources is defined in this file and saved in sysdata.rda
utils::globalVariables(c("data_sources"))

parseEverythingAndSave <- function() {
  # this is not strictly a parsing step, but is quite slow. It relies on picking
  # up already saved files from previous steps. It can take hours to complete,
  # but only needs to be done rarely. This is only intended to be run from
  # development tree, not as installed package
  loadNamespace("devtools")
  generate_sysdata()
  devtools::load_data(pkg = ".") # reload the newly saved data
  parseAndSaveQuick()
  devtools::load_data(pkg = ".") # reload the newly saved data
  icd9BuildChaptersHierarchy(save_data = TRUE) # depends on icd9cm_billable

}

#' @title parse quickly parsable source data
#' @description Parses (and downloads if necessary) CDC annual revisions of
#'   ICD-9-CM to get the 'billable' codes. Also parses the AHRQ and Quan/Deyo
#'   comorbidity mappings from the source SAS data. Elixhauser and
#'   Quan/Elixhauser mappins are generated from transcribed codes.
#' @keywords internal
parseAndSaveQuick <- function() {
  loadNamespace("devtools")
  message("Parsing RTF file(s) to create icd9Desc descriptions of entire hierarchy")
  devtools::load_data(pkg = ".")

  # plain text billable codes
  message("Parsing plain text billable codes to create icd9cm_billable list of
                       data frames with descriptions of billable codes only.
                       No dependencies on other data.")
  parseLeafDescriptionsAll(save_data = TRUE)
  devtools::load_data(pkg = ".")

  message("Parsing comorbidity mappings from SAS and text sources.
                       (Make sure lookup files are updated first.)
                       Depends on icd9_hierarchy being updated.")
  parse_ahrq_sas(save_data = TRUE)
  parse_quan_deyo_sas(save_data = TRUE)
  icd9_generate_map_quan_elix(save_data = TRUE)
  icd9_generate_map_elix(save_data = TRUE)
}
# nocov end

#' @title get billable codes from all available years
#' @description for versions 23 to 32, those which are on the CMS web site, get
#'   any codes with long or short descriptions. Earlier years only have
#'   abbreviated descriptions.
#' @param save_data single logical value, if \code{TRUE} the source text or CSV file
#'   will be saved in \code{data-raw}, otherwise (the default) the data is
#'   simply returned invisibly.
#' @template offline
#' @return data frame with icd9, descShort and descLong columns. NA is placed in
#'   descLong when not available.
#' @examples
#'   # To populate the data-raw directory with the ICD-9 source:
#'   # not included in installed package, run using the full source from github,
#'   # e.g. using devtools::load_all()
#'   \dontrun{
#'   parseLeafDescriptionsAll(save_data = TRUE, offline = TRUE)
#'   }
#' @source
#' http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
#' @keywords internal
parseLeafDescriptionsAll <- function(save_data = FALSE, offline = FALSE, save = NULL) {

  if (!missing(save)) {
    warning("use save_data instead of save")
    save_data <- save
  }
  assertFlag(save_data)
  assertFlag(offline)

    # data_sources is in sysdata.RData
  versions <- data_sources$version
  message("Available versions of sources are: ", paste(versions, collapse = ", "))
  icd9cm_billable <- list()
  for (v in versions)
    icd9cm_billable[[v]] <- parseLeafDescriptionsVersion(version = v, save_data = save_data,
                                                      offline = offline)

  # and in my utils.R  getNonASCII(charactervector)
  if (save_data)
    save_in_data_dir("icd9cm_billable")
  invisible(icd9cm_billable)
}

#' @title read the ICD-9-CM description data as provided by the Center for
#'   Medicaid Services.
#' @description ICD9-CM data unfortunately has no comma separation, so have to
#'   pre-process. Note that this canonical data doesn't specify non-diagnostic
#'   higher-level codes, just the specific diagnostic 'child' codes.
#'
#'   The file can be pulled from the zip files on the CMS web site or from
#'   within the package. Pulled data can be saved to the package development
#'   tree.
#' @param icd9path path of the source data which is typically in \code{data-raw}
#' @param save logical whether to attempt to re-save source files in inst
#' @param path Absolute path in which to save parsed data
#' @return invisibly return the result
#' @keywords internal
parseLeafDescriptionsVersion <- function(version = icd9cm_latest_edition(), save_data = FALSE,
                                         offline = FALSE, save = NULL) {
  if (!missing(save)) {
    warning("use save_data instead of save")
    save_data <- save
  }
  assertString(version)
  assertFlag(save_data)
  assertFlag(offline)

  message("Fetching billable codes version: ", version)

  if (version == "27")
    return(invisible(parseIcd9LeafDescriptions27(save_data = save_data,
                                                 fromWeb = fromWeb)))
  stopifnot(version %in% data_sources$version)
  dat <- data_sources[data_sources$version == version, ]
  url <- dat$url
  fn_short_orig <- dat$short_filename
  fn_long_orig <- dat$long_filename
  fn_short <- make.names(fn_short_orig)
  fn_long <- make.names(fn_long_orig)
  path_short <- file.path("data-raw", fn_short)
  path_long <- file.path("data-raw", fn_long)

  if (!save_data && !file.exists(path_short)) {
    # not saving, so we can read-only get the path from the installed package:
    path_short <- system.file("data-raw", fn_short, package = get_pkg_name())
    path_long <- system.file("data-raw", fn_long, package = get_pkg_name())
  }

    message("short filename = ", fn_short, "\n long filename = ", fn_long)
    message("short path = ", path_short, "\n long path = ", path_long)

  assertPathForOutput(path_short, overwrite = TRUE)

  either_file_missing <- !file.exists(path_short) || !file.exists(path_long)
  if (fromWeb || either_file_missing) {
    # don't do any fancy encoding stuff, just dump the file
    unzip_single(url, fn_short_orig, path_short)
    if (!is.na(fn_long_orig))
      unzip_single(url, fn_long_orig, path_long)
  }
  # yes, specify encoding twice, once to declare the source format, and again
  # to tell R to flag (apparently only where necessary), the destination
  # strings: in our case this is about ten accented character in long
  # descriptions of disease names
  short_conn <- file(path_short)
  readLines(short_conn) -> shortlines
  close(short_conn)
  if (!is.na(fn_long_orig)) {
    file_long <- file(path_long, encoding = "latin1")
    longlines <- readLines(path_long, encoding = "latin1")
    close(file_long)
  } else
    longlines <- NA_character_

  shortlines <- strsplit(shortlines, "[[:space:]]")
  longlines <- strsplit(longlines, "[[:space:]]")

  # the encoding was stated, but is dropped by my trim function...
  trim_regex <- function(x) gsub("(^[[:space:]])|([[:space:]]$)", "", x)

  icd9ShortCode <- lapply(shortlines, FUN = function(x) trim_regex(x[1]))
  icd9ShortDesc <- lapply(shortlines, FUN = function(x) trim_regex(paste(x[-1], collapse = " ")))
  if (!is.na(longlines[1]))
    icd9LongDesc <- lapply(longlines, FUN = function(x) trim_regex(paste(x[-1], collapse = " ")))
  else
    icd9LongDesc <- NA

  out <- data.frame(icd9 = unlist(icd9ShortCode),
                    descShort = unlist(icd9ShortDesc),
                    descLong = unlist(icd9LongDesc),
                    stringsAsFactors = FALSE)

  # now sort so that E is after V:
  reorder <- icd9_order_short(out[["icd9"]])
  out <- out[reorder, ]

  # warn as we go:
  oldwarn <- options(warn = 1)
  on.exit(options(oldwarn))
  if (!is.na(fn_long_orig)) {
    encs <- Encoding(out[["descLong"]])
    message("Found labelled encodings: ", paste(unique(encs), collapse = ", "))
    message("non-ASCII rows of long descriptions are: ",
            paste(getNonASCII(out[["descLong"]]), collapse = ", "))
    message(Encoding(out[["descLong"]][isNonASCII(out[["descLong"]])]))

  }
  invisible(out)
}

parseIcd9LeafDescriptions27 <- function(save_data = FALSE, fromWeb = NULL) {
  message("working on version 27 quirk")
  assertFlag(save_data)
  assertFlag(fromWeb)
  v27 <- data_sources$version == "27"
  fn <- make.names(data_sources[v27, "other_filename"])
  fp <- file.path("data-raw", fn)
  url <- data_sources[v27, "url"]

  if (!save_data && !file.exists(fp))
    fp <- system.file("data-raw", fn, package = get_pkg_name())

  message("v27 file name = '", fn,
                       "', and path = '", fp,
                       "'. URL = ", url)

  if (save_data || fromWeb || !file.exists(fp)) unzip_single(url, fn, fp)
  unzip_single(url, fn, fp)
  f <- file(fp, encoding = "latin1")
  icd9cm_billable27 <- read.csv(fp, stringsAsFactors = FALSE,
                                colClasses = "character", encoding = "latin1")
  close(f)
  names(icd9cm_billable27) <- c("icd9", "descLong", "descShort")
  icd9cm_billable27 <- icd9cm_billable27[c(1, 3, 2)] # reorder columns
  reorder <- icd9_order_short(icd9cm_billable27[["icd9"]])
  invisible(icd9cm_billable27[reorder, ])
}

#' @title Read higher-level ICD-9 structure from a reliable web site
#' @description This is rather slow, queries a web page repeatedly, which is
#'   both non-reproducible, and perhaps bad form. Aim to deprecate and replace
#'   with my own RTF parsing of canonical documents, which is now working
#'   reasonably well, at least for 'major' codes from 2015.
#' @details This is not almost obsolete. The only remaining use is the chapter
#'   and sub-chapter names and ranges, or validation.
#' @keywords internal
parseIcd9Chapters <- function(year = NULL,
                              save_data = FALSE, save = NULL) {
  if (!missing(save)) {
    warning("use save_data instead of save")
    save_data <- save
  }
  assertFlag(save_data)
  if (is.null(year))
    year <- "2014"
  else {
    assertScalar(year)
    if (is.character(year)) {
      year <- as.integer(year)
    }
    # version 23 dates back to 2005, but I don't have access on web for versions
    # before 23. Where are these?
    # http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
    assertIntegerish(year, lower = 2005, upper = 2015,
                     any.missing = FALSE, len = 1)
    year <- as.character(year)
  }
  if (save_data && format(Sys.time(), "%Y") != year)
    warning(sprintf("Getting ICD-9 data for %s which is not the current year.
                    Tests were written to validate extraction of 2014 data.", year))

  # if either XML or memoise are not installed, an error will be given by R
  memReadHtmlList <- memoise::memoise(XML::readHTMLList)

  icd9Chapters <- icd9WebParseGetList(year, memfun = memReadHtmlList)
  icd9ChaptersSub <- list()
  icd9ChaptersMajor <- list()
  for (chap in names(icd9Chapters)) {
    if (chap == "Diseases Of The Blood And Blood-Forming Organs" ||
        chap == "Congenital Anomalies") {
      # these have no subchapter, straight into the three-digit codes
      icd9ChaptersMajor <- c(icd9ChaptersMajor,
                             icd9WebParseGetList(year, memfun = memReadHtmlList,
                                                 icd9Chapters[[chap]]))
    } else {
      # construct URL for next level and get the sub chapters
      subchaps <- icd9WebParseGetList(year, memReadHtmlList, icd9Chapters[[chap]])
      icd9ChaptersSub <- c(icd9ChaptersSub, subchaps)
      # loop through each subchapter to get the majors:
      for (subchap in names(subchaps)) {
        icd9ChaptersMajor <- c(icd9ChaptersMajor,
                               icd9WebParseGetList(year, memfun = memReadHtmlList,
                                                   icd9Chapters[[chap]],
                                                   icd9ChaptersSub[[subchap]]))
      }
    }
  }
  # there are multiple use-cases to be served here. One is to look up an ICD-9
  # from the CMS list, and find the higher level groups it belongs to. Another
  # is to do the same for an arbitrary code. One approach would be to construct
  # a data frame with a row for each known code, and a factor for each hierarchy
  # level: this would not enable matching an arbitrary code, but this is
  # probably a limited problem for the rare cases of obsolete codes, or new
  # codes, when the coding was done in a different year from this analysis.

  # nocov start
  if (save_data) {
    # top level chapters are hand-written in data/
    save_in_data_dir("icd9ChaptersSub")
    save_in_data_dir("icd9ChaptersMajor")
  }
  invisible(list(icd9Chapters = icd9Chapters,
                 icd9ChaptersSub = icd9ChaptersSub,
                 icd9ChaptersMajor = icd9ChaptersMajor))
}
# nocov end

icd9WebParseStartEndToRange <- function(v)
  paste(v[["start"]], v[["end"]], sep = "-")

icd9WebParseGetList <- function(year, memfun, chapter = NULL, subchap = NULL) {
  icd9url <- NULL
  if (is.null(chapter)) {
    icd9url <- sprintf("http://www.icd9data.com/%s/Volume1/default.htm", year)
  } else {
    chapter <- icd9WebParseStartEndToRange(chapter)
    if (is.null(subchap)) {
      icd9url <- sprintf("http://www.icd9data.com/%s/Volume1/%s/default.htm",
                         year, chapter)
    } else {
      subchap <- icd9WebParseStartEndToRange(subchap)
      icd9url <- sprintf("http://www.icd9data.com/%s/Volume1/%s/%s/default.htm",
                         year, chapter, subchap)
    }
  }
  li <-  memfun(doc = icd9url, which = 1)
  # swap so descriptions (second on web page) become the vector names
  v <- str_pair_match(li, "^([VvEe0-9-]*)[[:space:]]*(.*)$", swap = TRUE)
  lapply(v,
         FUN = function(x) {
           x %>% str_match_all(pattern = "^([VvEe0-9]+)-?([VvEe0-9]+)?$") %>%
             lapply(`[`, c(2,3)) %>% unlist -> y
           if (length(y) != 2)
             stop("y should be length 2, when processing ", x)
           names(y) <- c("start", "end")
           #y <- y[-2]
           if (y[["end"]] == "")
             names(y) <- "major"
           y
         }
  )
}

icd9BuildChaptersHierarchy <- function(save_data = FALSE) {
  if (!missing(save)) {
    warning("use save_data instead of save")
    save_data <- save
  }
  assertFlag(save_data)

  icd9Desc <- parse_rtf_year(year = "2011", save_data = FALSE, verbose = TRUE)

  message("working on slow step of web scrape to build icd9 Chapters Hierarchy.")
  chaps <- icd9GetChapters(icd9 = icd9Desc$icd9, isShort = TRUE, verbose = FALSE)

  # could also get some long descs from more recent billable lists, but not
  # older ones which only have short descs
  icd9_hierarchy <- cbind(
    data.frame("icd9" = icd9Desc$icd9,
               "descLong" = icd9Desc$desc,
               stringsAsFactors = FALSE),
    # the following can and should be factors:
    chaps
  )

  # fix congenital abnormalities not having subchapter defined:
  # ( this might be easier to do when parsing the chapters themselves...)
  icd9_hierarchy <- fixSubchapterNa(icd9_hierarchy, 740, 759)
  # and hematopoietic organs
  icd9_hierarchy <- fixSubchapterNa(icd9_hierarchy, 280, 289)

  # insert the short descriptions from the billable codes text file. Where there
  # is no short description, e.g. for most Major codes, or intermediate codes,
  # just copy the long description over.

  bill32 <- icd9::icd9cm_billable[["32"]]

  billable_codes <- icd9GetBillableShort(icd9_hierarchy$icd9) # or from bill32
  billable_rows <- which(icd9_hierarchy$icd9 %in% billable_codes)
  title_rows <- which(icd9_hierarchy$icd9 %nin% billable_codes)
  icd9_hierarchy[billable_rows, "descShort"] <- bill32$descShort
  # for rows without a short description (i.e. titles, non-billable),
  # useexisting long desc
  icd9_hierarchy[title_rows, "descShort"] <- icd9_hierarchy[title_rows, "descLong"]
  # the billable codes list (where available) currently has better long
  # descriptions than the RTF parse. For previous years, there is no long desc
  # in billable, so careful when updating this.
  icd9_hierarchy[billable_rows, "descLong"] <- bill32$descLong

  # now put the short description in the right column position
  icd9_hierarchy <- icd9_hierarchy[c("icd9", "descShort", "descLong", "threedigit",
                                   "major", "subchapter", "chapter")]

  # quick sanity checks - full tests in test-parse.R
  stopifnot(all(icd9IsValidShort(icd9_hierarchy$icd9)))
  stopifnot(!any(sapply(icd9_hierarchy, is.na)))

  if (save_data)
    save_in_data_dir("icd9_hierarchy") # nocov
}

fixSubchapterNa <- function(x, start, end) {
  # 740 CONGENITAL ANOMALIES is a chapter with no sub-chapters defined. For
  # consistency, assign the same name to sub-chapters
  congenital <- x$icd9 %in% (start %i9sa% end)
  # assert all the same:
  stopifnot(all(x[congenital[1], "chapter"] == x[congenital[-1], "chapter"]))
  # now some work to insert a new level into the sub-chapter factor in the right place
  previous_sub <- asCharacterNoWarn(x[(which(congenital) - 1)[1], "subchapter"])
  previous_sub_pos <- which(levels(x$subchapter) == previous_sub)
  congenital_title <- asCharacterNoWarn(x[which(congenital)[1], "chapter"])
  new_subs <- asCharacterNoWarn(x$subchapter)
  new_subs[congenital] <- congenital_title
  new_levels <- append(levels(x$subchapter), congenital_title, previous_sub_pos)
  x$subchapter <- factor(new_subs, new_levels)
  x
}
