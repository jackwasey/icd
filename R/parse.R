# EXCLUDE COVERAGE START

parseEverythingAndSave <- function() {
  parseAndSaveQuick()

  # this is not strictly a parsing step, but is quite slow. It relies on picking
  # up already saved files from previous steps. It also takes many hours to
  # complete, but only needs to be done rarely.
  warning("starting multiple-hour process.
          Make sure package is reloaded before running this step.")
  icd9BuildChaptersHierarchy(save = TRUE)
}

#' @title parse almost everything
#' @keywords internal
parseAndSaveQuick <- function() {
  # comorbidity mappings
  parseAhrqSas(save = TRUE)
  parseElix(save = TRUE)
  parseQuanDeyoSas(save = TRUE)
  parseQuanElix(save = TRUE)

  # RTF file(s)
  parseRtfToDesc(save = TRUE)

  # plain text billable codes
  parseIcd9LeafDescriptionsAll(save = TRUE)

  # this queries a web page: TODO: use only for testing result of RTF extraction
  # parseIcd9Chapters(save = TRUE)
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
  versions <- icd9:::billable_sources$version
  icd9Billable <- list()
  for (v in as.character(versions)) {
    icd9Billable[[as.character(v)]] <- parseIcd9LeafDescriptionsVersion(version = v, fromWeb = fromWeb, verbose = verbose)
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
                                             fromWeb = NULL, verbose = FALSE, encoding = "latin1") {
  if (verbose) message("Fetching billable codes version: ", version)
  checkmate::assertScalar(version)
  checkmate::assertFlag(save)
  version <- as.character(version)
  if (as.character(version) == "27") return(invisible(parseIcd9LeafDescriptions27(save = save)))
  stopifnot(version %in% icd9:::billable_sources$version)
  dat <- icd9:::billable_sources[icd9:::billable_sources$version == version, ]
  url <- dat$url
  fn_short <- dat$short_filename
  fn_long <- dat$long_filename
  if (save) {
    path_short <- file.path("inst", "extdata", fn_short)
    path_long <- file.path("inst", "extdata", fn_long)
  } else{
    path_short <- system.file("extdata", package = "icd9", fn_short)
    path_long <- system.file("extdata", package = "icd9", fn_long)
  }

  if (verbose) message(fn_short, ", ", fn_long, "\n", path_short, ", ", path_long)


  if (!is.null(fromWeb))
    checkmate::assertFlag(fromWeb)
  else {
    fromWeb = FALSE
    if (!file.exists(path_short) || !file.exists(path_long)) {
      fromWeb = TRUE
      save = TRUE
    }
  }

  if (fromWeb) {
    shortlines <- read.zip.url(url, fn_short, encoding = encoding)
    if (!is.na(fn_long))
      longlines <- read.zip.url(url, fn_long, encoding = encoding)
    else
      longlines <- NA_character_

    if (save) {
      # rewrite lines to our package
      f <- file(path_short, "w+")
      writeLines(shortlines, f, useBytes = TRUE)
      close(f)
      if (!is.na(fn_long)) {
        f <- file(path_long, "w+")
        writeLines(longlines, f, useBytes = TRUE)
        close(f)
      }
    }
  } else {
    f <- file(path_short, "r")
    readLines(f, encoding = encoding) -> shortlines
    close(f)
    if (!is.na(fn_long)) {
      f <- file(path_long, "r")
      readLines(f, encoding = encoding) -> longlines
      close(f)
    } else {
      longlines <- NA_character_
    }
  }

  shortlines %<>% strsplit("[[:space:]]")
  longlines %<>% strsplit("[[:space:]]")

  icd9ShortCode <- lapply(shortlines, FUN = function(x) trim(x[1]))
  icd9ShortDesc <- lapply(shortlines, FUN = function(x) trim(paste(x[-1], collapse = " ")))
  if (!is.na(longlines[1])) {
    # icd9LongCode <- lapply(longlines, FUN = function(x) trim(x[1]))
    icd9LongDesc <- lapply(longlines, FUN = function(x) trim(paste(x[-1], collapse = " ")))
  } else {
    icd9LongDesc <- NA
  }

  var_name <- paste0("icd9Billable", make.names(version))

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

  if (save) saveInDataDir(var_name)

  if (!is.na(fn_long)) {
    utf8 <- grep(pattern = "UTF", Encoding(get(var_name, inherits = FALSE)[["descLong"]]))
    if (length(utf8) > 0 ) {
      message("The following long descriptions contain UTF-8 codes:")
      message(paste(get(var_name, inherits = FALSE)[utf8, ], sep = ", "))
    }
  }
  invisible(get(var_name, inherits = FALSE))
}

parseIcd9LeafDescriptions27 <- function(save = FALSE, fromWeb = NULL, verbose = FALSE, encoding = "latin1") {
  if (verbose) message("working on version 27 quirk")
  checkmate::assertFlag(save)
  fn <- icd9:::billable_sources[icd9:::billable_sources$version == 27, "other_filename"]
  fp <- file.path("inst", "extdata", fn)
  url <- icd9:::billable_sources[icd9:::billable_sources$version == 27, "url"]
  if (verbose) message(fn, ", ", fp, "\n", url)

  if (!is.null(fromWeb))
    checkmate::assertFlag(fromWeb)
  else {
    fromWeb = FALSE
    if (!file.exists(fp)) {
      fromWeb = TRUE
      save = TRUE
    }
  }

  if (fromWeb) {
    read.zip.url(url, fn, encoding = encoding) -> tmp_dat
    if (save) {
      save_path <- fp
      save_conn <- file(save_path, "w+")
      writeLines(tmp_dat, save_conn, useBytes = TRUE)
      close(save_conn)
    }
  }
  f <- file(fp, "r", encoding = encoding) # encoding for connection vs file itself?
  read.csv(f, stringsAsFactors = FALSE,
           colClasses = "character",
           encoding = encoding) -> x
  close(f)

  names(x) <- c("icd9", "descLong", "descShort")
  x <- x[c(1, 3, 2)]

  # TODO, this is duplicated code: move to parent loop
  # now sort so that E is after V:
  reorder <- sortOrderShort(x[["icd9"]])
  x <- x[reorder, ]
  x
}

#' @title Read higher-level ICD-9 structure from a reliable web site
#' @description This is rather slow, queries a web page repeatedly, which is
#'   both non-reproducible, and perhaps bad form. Aim to deprecate and replace
#'   with my own RTF parsing of canonical documents, which is now working
#'   reasonably well, at least for 'major' codes from 2015. TODO: deprecate
#' @keywords internal
parseIcd9Chapters <- function(year = NULL,
                              save = FALSE) {
  checkmate::assertFlag(save)
  if (is.null(year))
    year <- "2014"
  else {
    checkmate::assertScalar(year)
    if (is.character(year)) {
      year <- as.integer(year)
    }
    # version 23 dates back to 2005, but I don't have access on web for versions
    # before 23. TODO: are these somewhere else?
    # http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
    checkmate::assertIntegerish(year, lower = 2005, upper = 2015,
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

# this takes HOURS
# TODO: enable annual versions
icd9BuildChaptersHierarchy <- function(save = FALSE) {
  checkmate::assertFlag(save)

  # slow step
  chaps <- icd9GetChapters(icd9 = icd9::icd9Desc$icd9, isShort = TRUE)

  icd9Hierarchy <- cbind(
    data.frame("icd9" = icd9::icd9Desc$icd9,
               # TODO: could also get some long descs from more recent billable
               # lists, but not older ones which only have short descs
               "descLong" = icd9::icd9Desc$desc,
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
  previous_sub <- x[(which(congenital) - 1)[1], "subchapter"] %>% asCharacterNoWarn
  previous_sub_pos <- which(levels(x$subchapter) == previous_sub)
  congenital_title <- x[which(congenital)[1], "chapter"] %>% asCharacterNoWarn
  new_subs <- x$subchapter %>% asCharacterNoWarn
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
  billable_sources <- data.frame(
    version = c(32, 31, 30, 29, 28, 27,
                #"27 (abbrev only)",
                26, 25, 24, 23),
    start_date = c("2014-10-01", "2013-10-01", "2012-10-01", "2011-10-01", "2010-10-01",
                   NULL, "2009-10-01", "2008-10-01", "2007-10-01", "2006-10-01", "2005-10-01"),
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
    stringsAsFactors = FALSE
  )

  # we assume we are in the root of the package directory. Save to sysdata.rda
  # because these are probably not of interest to a user and would clutter an
  # already busy namespace.
  lknames <- c("icd9NShort", "icd9VShort", "icd9EShort",
               "icd9NShortBillable", "icd9VShortBillable", "icd9EShortBillable",
               "icd9NShortReal", "icd9VShortReal", "icd9EShortReal",
               "billable_sources")
  if (save) save(list = lknames,
                 file = sysdata.path, compress = "xz")
  invisible(mget(lknames))
}
