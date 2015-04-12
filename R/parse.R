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

# EXCLUDE COVERAGE START

parseEverythingAndSave <- function(verbose = TRUE) {
  # this is not strictly a parsing step, but is quite slow. It relies on picking
  # up already saved files from previous steps. It can take hours to complete,
  # but only needs to be done rarely. This is only intended to be run from
  # development tree, not as installed package

  generateSysData()
  devtools::load_data(pkg = ".") # reload the newly saved data
  parseAndSaveQuick(verbose = verbose)
  devtools::load_data(pkg = ".") # reload the newly saved data
  icd9BuildChaptersHierarchy(save = TRUE, verbose = verbose) # depends on icd9Desc and icd9Billable

}

#' @title parse almost everything
#' @keywords internal
parseAndSaveQuick <- function(verbose = FALSE) {
  #if (verbose) message("Parsing RTF file(s) to create icd9Desc descriptions of entire hierarchy")
  #parseRtfYear(year = "2011", save = TRUE, verbose = verbose) # creates icd9Desc
  devtools::load_data(pkg = ".")

  # plain text billable codes
  if (verbose) message("Parsing plain text billable codes to create icd9Billable list of
                       data frames with descriptions of billable codes only.
                       No dependencies on other data.")
  parseIcd9LeafDescriptionsAll(save = TRUE, verbose = verbose)
  devtools::load_data(pkg = ".")

  if (verbose) message("Parsing comorbidity mappings from SAS and text sources.
                       (Make sure lookup files are updated first.)
                       Depends on icd9Hierarchy being updated.")
  parseAhrqSas(save = TRUE)
  parseElix(save = TRUE)
  parseQuanDeyoSas(save = TRUE)
  parseQuanElix(save = TRUE)
}
# EXCLUDE COVERAGE END

#' @title get billable codes from all available years
#' @description for versions 23 to 32, those which are on the CMS web site, get
#'   any codes with long or short descriptions. Earlier years only have
#'   abbreviated descriptions.
#' @param save single logical value, if \code{TRUE} the source text or CSV file
#'   will be saved in \code{inst/extdata}, otherwise (the default) the data is
#'   simply returned invisibly.
#' @return data frame with icd9, descShort and descLong columns. NA is placed in
#'   descLong when not available.
#' @source
#' http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
#' @keywords internal
parseIcd9LeafDescriptionsAll <- function(save = FALSE, fromWeb = FALSE, verbose = FALSE) {
  versions <- data_sources$version
  if (verbose) message("Available versions of sources are: ", paste(versions, collapse = ", "))
  icd9Billable <- list()
  for (v in versions) {
    icd9Billable[[v]] <- parseIcd9LeafDescriptionsVersion(version = v, save = save, fromWeb = fromWeb, verbose = verbose)
  }
  if (save) saveInDataDir("icd9Billable")
  invisible(icd9Billable)
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
#' @param icd9path path of the source data which is in /extddata in the
#'   installed package, but would be in inst/extdata in development tree.
#' @param save logical whether to attempt to re-save source files in inst
#' @param path Absolute path in which to save parsed data
#' @return invisibly return the result
#' @keywords internal
parseIcd9LeafDescriptionsVersion <- function(version = getLatestBillableVersion(), save = FALSE,
                                             fromWeb = FALSE, verbose = FALSE) {
  assertString(version)
  assertFlag(save)
  assertFlag(fromWeb)
  assertFlag(verbose)


  # test encodings with
  # readLines("inst/extdata/CMS32_DESC_LONG_DX.txt")[4510] %T>% cat

  if (verbose) message("Fetching billable codes version: ", version)

  if (version == "27") return(invisible(parseIcd9LeafDescriptions27(save = save, fromWeb = fromWeb,
                                                                    verbose = verbose)))
  stopifnot(version %in% data_sources$version)
  dat <- data_sources[data_sources$version == version, ]
  url <- dat$url
  fn_short_orig <- dat$short_filename
  fn_long_orig <- dat$long_filename
  fn_short <- make.names(fn_short_orig)
  fn_long <- make.names(fn_long_orig)
  path_short <- file.path("inst", "extdata", fn_short)
  path_long <- file.path("inst", "extdata", fn_long)

  if (!save && !file.exists(path_short)) {
    # not saving, so we can read-only get the path from the installed package:
    path_short <- system.file("extdata", fn_short, package = "icd9")
    path_long <- system.file("extdata", fn_long, package = "icd9")
  }

  if (verbose) {
    message("short filename = ", fn_short, "\n long filename = ", fn_long)
    message("short path = ", path_short, "\n long path = ", path_long)
  }

  assertCharacter(path_short, min.chars = 10, any.missing = FALSE, len = 1)

  either_file_missing <- !file.exists(path_short) || !file.exists(path_long)
  if (fromWeb || either_file_missing) {
    # don't do any fancy encoding stuff, just dump the file, then re-read.
    zip_shortlines <- read.zip.url(url, fn_short_orig)
    writeLines(zip_shortlines, path_short, useBytes = TRUE)
    if (!is.na(fn_long_orig)) {
      zip_longlines <- read.zip.url(url, fn_long_orig)
      writeLines(zip_longlines, path_long, useBytes = TRUE)
    }
  }
    # yes, specify encoding twice, once to declare the source format, and again
    # to tell R to flag (apparently only where necessary), the destination
    # strings: in our case this is about ten accented character in long
    # descriptions of disease names
    short_conn <- file(path_short, encoding = dat$short_encoding)
    readLines(short_conn, encoding = "UTF-8") -> shortlines
    close(short_conn)
    if (!is.na(fn_long_orig)) {
      long_conn <- file(path_long, encoding = dat$long_encoding)
      readLines(long_conn, encoding = "UTF-8") -> longlines
      close(long_conn)
    } else
      longlines <- NA_character_

  shortlines %<>% strsplit("[[:space:]]")
  longlines %<>% strsplit("[[:space:]]")

  icd9ShortCode <- lapply(shortlines, FUN = function(x) trim(x[1]))
  icd9ShortDesc <- lapply(shortlines, FUN = function(x) trim(paste(x[-1], collapse = " ")))
  if (!is.na(longlines[1]))
    icd9LongDesc <- lapply(longlines, FUN = function(x) trim(paste(x[-1], collapse = " ")))
  else
    icd9LongDesc <- NA

  var_name <- paste0("icd9Billable", version)

  assign(var_name,
         data.frame(
           icd9 = unlist(icd9ShortCode),
           descShort = unlist(icd9ShortDesc),
           descLong = unlist(icd9LongDesc),
           stringsAsFactors = FALSE)
  )

  # now sort so that E is after V:
  reorder <- sortOrderShort(get(var_name)[["icd9"]])
  assign(var_name, get(var_name)[reorder, ])

  # warn as we go:
  oldwarn <- options(warn = 1)
  on.exit(options(oldwarn))
  if (!is.na(fn_long_orig) && verbose) {
    encs <- Encoding(get(var_name, inherits = FALSE)[["descLong"]])
    # tools::: has better options here, e.g.:
    # for (f in list.files("inst/extdata", full.names = T)) { message("Checking ", f, " for non-ASCII"); tools:::showNonASCIIfile(f) }

    message("Found labelled encodings: ", paste(unique(encs), collapse = ", "))
    utf <- grep(pattern = "UTF", encs)
    latin1 <- grep(pattern = "Latin", encs)
    nonASCII <- grep(pattern = "ASCII", invert = TRUE, encs)
    #     if (length(nonASCII) > 0 ) {
    #       warning("The following long descriptions contain non-ASCII characters: ",
    #               paste(get(var_name, inherits = FALSE)[nonASCII, ], sep = ", "))
    #     }
    if (length(utf) > 0 ) {
      warning("The following long descriptions contain Unicode characters: ",
              paste(get(var_name, inherits = FALSE)[utf, ], sep = ", "))
    }
    if (length(latin1) > 0 ) {
      warning("The following long descriptions contain Latin-1 characters: ",
              paste(get(var_name, inherits = FALSE)[latin1, ], sep = ", "))
    }
  }
  invisible(get(var_name, inherits = FALSE))
}

parseIcd9LeafDescriptions27 <- function(save = FALSE, fromWeb = NULL, verbose = FALSE) {
  if (verbose) message("working on version 27 quirk")
  assertFlag(save)
  assertFlag(fromWeb)
  assertFlag(verbose)
  fn <- make.names(data_sources[data_sources$version == 27, "other_filename"])
  fp <- file.path("inst", "extdata", fn)
  url <- data_sources[data_sources$version == 27, "url"]

  if (!save && !file.exists(fp))
    fp <- system.file("extdata", fn, package = "icd9")

  if (verbose) message("v27 file name = '", fn, "', and path = '", fp, "'. URL = ", url)

  if (save || fromWeb || !file.exists(fp))
    writeLines(
      read.zip.url(url, fn),
      fp, useBytes = TRUE)
  icd9Billable27 <- read.csv(fp, stringsAsFactors = FALSE, colClasses = "character")
  names(icd9Billable27) <- c("icd9", "descLong", "descShort")
  icd9Billable27 <- icd9Billable27[c(1, 3, 2)] # reorder columns

  # TODO, this is duplicated code: move to parent loop
  # now sort so that E is after V:
  reorder <- sortOrderShort(icd9Billable27[["icd9"]])
  icd9Billable27 <- icd9Billable27[reorder, ]
  if (save) saveInDataDir("icd9Billable27")
  invisible(icd9Billable27)
}

#' @title Read higher-level ICD-9 structure from a reliable web site
#' @description This is rather slow, queries a web page repeatedly, which is
#'   both non-reproducible, and perhaps bad form. Aim to deprecate and replace
#'   with my own RTF parsing of canonical documents, which is now working
#'   reasonably well, at least for 'major' codes from 2015. TODO: deprecate
#' @keywords internal
parseIcd9Chapters <- function(year = NULL,
                              save = FALSE) {
  assertFlag(save)
  if (is.null(year))
    year <- "2014"
  else {
    assertScalar(year)
    if (is.character(year)) {
      year <- as.integer(year)
    }
    # version 23 dates back to 2005, but I don't have access on web for versions
    # before 23. TODO: are these somewhere else?
    # http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
    assertIntegerish(year, lower = 2005, upper = 2015,
                                any.missing = FALSE, len = 1)
    year <- as.character(year)
  }
  if (save && format(Sys.time(), "%Y") != year)
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
  # EXCLUDE COVERAGE START
  if (save) {
    # saveInDataDir("icd9Chapters") # manually entered
    saveInDataDir("icd9ChaptersSub") # TODO: remove, now I have parsed from RTF
    saveInDataDir("icd9ChaptersMajor") # TODO: remove, now I have parsed from RTF
  }
  invisible(list(icd9Chapters = icd9Chapters,
                 icd9ChaptersSub = icd9ChaptersSub,
                 icd9ChaptersMajor = icd9ChaptersMajor))
}
# EXCLUDE COVERAGE END

icd9WebParseStartEndToRange <- function(v) {
  paste(v[["start"]], v[["end"]], sep = "-")
}

# internal only
icd9WebParseGetList <- function(year, memfun, chapter = NULL, subchap = NULL) {
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
  v <- strPairMatch("^([VvEe0-9-]*)[[:space:]]*(.*)$", li, swap = TRUE)
  lapply(v,
         FUN = function(x) {
           y <- unlist(strMultiMatch(pattern = "^([VvEe0-9]+)-?([VvEe0-9]+)?$", text = x))
           names(y) <- c("start", "end")
           if (y[["end"]] == "") names(y <- y[-2]) <- "major"
           y
         }
  )
}

# TODO: enable annual versions
icd9BuildChaptersHierarchy <- function(save = FALSE, verbose = FALSE) {
  assertFlag(save)
  assertFlag(verbose)

  icd9Desc <- parseRtfYear(year = "2011", save = TRUE, verbose = verbose)

  # TODO: now we get almost everything from the RTF, we can just pull the
  # chapter and sub-chapters from the web very quickly (or from the RTF)
  if (verbose) message("working on (possibly) slow step of web scrape to build icd9 Chapters Hierarchy.")
  chaps <- icd9GetChapters(icd9 = icd9Desc$icd9, isShort = TRUE, verbose = verbose)

  icd9Hierarchy <- cbind(
    data.frame("icd9" = icd9Desc$icd9,
               # TODO: could also get some long descs from more recent billable
               # lists, but not older ones which only have short descs
               "descLong" = icd9Desc$desc,
               stringsAsFactors = FALSE),
    # the following can and should be factors:
    chaps
  )

  # fix congenital abnormalities not having subchapter defined:
  # ( this might be easier to do when parsing the chapters themselves...)
  icd9Hierarchy %<>% fixSubchapterNa(740, 759)
  # and hematopoietic organs
  icd9Hierarchy %<>% fixSubchapterNa(280, 289)

  # insert the short descriptions from the billable codes text file. Where there
  # is no short description, e.g. for most Major codes, or intermediate codes,
  # just copy the long description over.
  # TODO need to match the annual RTF with the annual txt file

  bill32 <- icd9::icd9Billable[["32"]]

  billable_codes <- icd9GetBillableShort(icd9Hierarchy$icd9) # or from bill32
  billable_rows <- which(icd9Hierarchy$icd9 %in% billable_codes)
  title_rows <- which(icd9Hierarchy$icd9 %nin% billable_codes)
  icd9Hierarchy[billable_rows, "descShort"] <- bill32$descShort
  # for rows without a short description (i.e. titles, non-billable), useexisting long desc
  icd9Hierarchy[title_rows, "descShort"] <- icd9Hierarchy[title_rows, "descLong"]
  # the billable codes list (where available) currently has better long
  # descriptions than the RTF parse. For previous years, there is no long desc
  # in billable, so careful when updating this.
  icd9Hierarchy[billable_rows, "descLong"] <- bill32$descLong

  # now put the short description in the right column position
  icd9Hierarchy <- icd9Hierarchy[c("icd9", "descShort", "descLong", "threedigit",
                                   "major", "subchapter", "chapter")]

  # quick sanity checks - full tests in test-parse.R
  stopifnot(all(icd9IsValidShort(icd9Hierarchy$icd9)))
  stopifnot(!any(sapply(icd9Hierarchy, is.na)))

  if (save) saveInDataDir("icd9Hierarchy") # EXCLUDE COVERAGE
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

#' Generate sysdata.rda
#'
#' Generate correctly ordered look-up tables of numeric-only, V and E codes. This is
#' quick, but much too slow when it appears many times in a loop.
#' @keywords internal
generateSysData <- function(sysdata.path = file.path("R", "sysdata.rda"), save = TRUE) {
  c() -> icd9NShort -> icd9VShort -> icd9EShort
  for (i in as.character(1:999))
    icd9NShort <- c(icd9NShort, sort(icd9ChildrenShort(i, onlyReal = FALSE)))
  for (i in as.character(0:99))
    icd9VShort <- c(icd9VShort, sort(icd9ChildrenShort(paste0("V", i), onlyReal = FALSE)))
  for (i in as.character(0:999))
    icd9EShort <- c(icd9EShort, sort(icd9ChildrenShort(paste0("E", i), onlyReal = FALSE)))

  # we can either use the icd9IsReal functions on these lists, or just grep the
  # canonical list directly to get the numeric, V and E codes.
  icd9NShortReal <- grep("^[^VE]+", icd9::icd9Hierarchy$icd9, value = TRUE) # nolint
  icd9VShortReal <- grep("V", icd9::icd9Hierarchy$icd9, value = TRUE) # nolint
  icd9EShortReal <- grep("E", icd9::icd9Hierarchy$icd9, value = TRUE) # nolint

  # also consider doing this in the ranging functions, even though slower, so
  # version can be chosen each time.
  icd9NShortBillable <- icd9GetBillable(icd9NShortReal, version = "32")
  icd9VShortBillable <- icd9GetBillable(icd9VShortReal, version = "32")
  icd9EShortBillable <- icd9GetBillable(icd9EShortReal, version = "32")

  # some very quick sanity checks: (duplicate in a test in test-ranges.R)
  stopifnot(length(icd9NShortReal) < length(icd9NShort))
  stopifnot(length(icd9VShortReal) < length(icd9VShort))
  stopifnot(length(icd9EShortReal) < length(icd9EShort))
  stopifnot(all(icd9NShortReal %in% icd9NShort))
  stopifnot(all(icd9VShortReal %in% icd9VShort))
  stopifnot(all(icd9EShortReal %in% icd9EShort))

  # http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
  data_sources <- data.frame(
    version = as.character(c(32, 31, 30, 29, 28, 27, 26, 25, 24, 23)),
    f_year = c(as.character(seq(2014, 2005))),
    start_date = c("2014-10-01", "2013-10-01", "2012-10-01", "2011-10-01", "2010-10-01",
                   "2009-10-01", "2008-10-01", "2007-10-01", "2006-10-01", "2005-10-01"),
    long_filename = c(
      "CMS32_DESC_LONG_DX.txt",
      "CMS31_DESC_LONG_DX.txt",
      "CMS30_DESC_LONG_DX 080612.txt",
      "CMS29_DESC_LONG_DX.101111.txt",
      "CMS28_DESC_LONG_DX.txt",
      NA, # see other_filename
      NA, # no long descriptions available for these years
      NA,
      NA,
      NA),
    short_filename = c(
      "CMS32_DESC_SHORT_DX.txt",
      "CMS31_DESC_SHORT_DX.txt",
      "CMS30_DESC_SHORT_DX.txt",
      "CMS29_DESC_SHORT_DX.txt",
      "CMS28_DESC_SHORT_DX.txt",
      NA,
      "V26 I-9 Diagnosis.txt",
      "I9diagnosesV25.txt",
      "I9diagnosis.txt",
      "I9DX_DESC.txt"),
    other_filename = c(NA, NA, NA, NA, NA,
                       "V27LONG_SHORT_DX_110909.csv",
                       # "V27LONG_SHORT_DX_110909u021012.csv" is 'updated' but
                       # hasn't got correctly formatted <3digit codes. TODO, use
                       # codes from first table, and descs from second.
                       NA, NA, NA, NA),
    long_encoding = c("latin1", "latin1", "latin1", "latin1", "latin1", "latin1", NA, NA, NA, NA),
    short_encoding = rep_len("ASCII", 10),
    url = c("http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/ICD-9-CM-v32-master-descriptions.zip",
            "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/cmsv31-master-descriptions.zip",
            "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/cmsv30_master_descriptions.zip",
            "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/cmsv29_master_descriptions.zip",
            "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/cmsv28_master_descriptions.zip",
            "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/FY2010Diagnosis-ProcedureCodesFullTitles.zip",
            # but this one is in a different format! only contains short descs:
            # "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v27_icd9.zip",
            "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v26_icd9.zip",
            "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v25_icd9.zip",
            "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v24_icd9.zip",
            "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v23_icd9.zip"),
    rtf_url = c( # FY11,12,13,14 are the same?
      rep_len("http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab12.zip", 4),
      "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/DTAB11.zip",
      "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab10.zip",
      "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab09.zip",
      "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab08.zip",
      "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab07.zip",
      "http://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/Dtab06.zip"),
    rtf_filename = c(
      rep_len("Dtab12.rtf", 4), "DTAB11.RTF", "Dtab10.RTF", "Dtab09.RTF", "Dtab08.RTF",
      "Dtab07.RTF", "Dtab06.rtf"), # there are more, but not with corresponding CMS, back to 1990s
    stringsAsFactors = FALSE
  )

  # we assume we are in the root of the package directory. Save to sysdata.rda
  # because these are probably not of interest to a user and would clutter an
  # already busy namespace.
  lknames <- c("icd9NShort", "icd9VShort", "icd9EShort",
               "icd9NShortBillable", "icd9VShortBillable", "icd9EShortBillable",
               "icd9NShortReal", "icd9VShortReal", "icd9EShortReal",
               "data_sources")
  if (save) save(list = lknames,
                 file = sysdata.path, compress = "xz")
  invisible(mget(lknames))
}
