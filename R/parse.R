# EXCLUDE COVERAGE START
#' @title parse all known mappings and save to development tree
#' @keywords internal
icd9ParseAndSaveMappings <- function() {
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

  # this is not strictly a parsing step, but is quite slow. It relies on picking up already saved files from previous steps
  icd9BuildChaptersHierarchy(save = TRUE)
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
parseIcd9LeafDescriptionsAll <- function(save = FALSE) {

  versions <- cmsIcd9ZipUrls$version
  icd9Billable <- list()
  for (v in versions) {
    icd9Billable[v] <- parseIcd9LeafDescriptionsVersion(version = v)
  }
  if (save) saveInDataDir("icd9Billable")
  icd9Billable
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
parseIcd9LeafDescriptionsVersion <- function(version = "32", save = FALSE,
                                             fromWeb = NULL, verbose = TRUE) {
  if (verbose) message("Fetching billable codes version: ", version)
  checkmate::assertScalar(version)
  checkmate::assertFlag(save)
  version <- as.character(version)
  if (as.character(version) == "27") return(invisible(parseIcd9LeafDescriptions27(save = save)))
  stopifnot(version %in% cmsIcd9ZipUrls$version)
  dat <- cmsIcd9ZipUrls[cmsIcd9ZipUrls$version == version, ]
  url <- dat$url
  fn_short <- dat$short_filename
  fn_long <- dat$long_filename
  path_short <- file.path("inst", "extdata", fn_short)
  path_long <- file.path("inst", "extdata", fn_long)
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
    shortlines <- read.zip.url(url, fn_short)
    if (!is.na(fn_long))
      longlines <- read.zip.url(url, fn_long)
    else
      longlines <- NA_character_

    if (save) {
      # rewrite lines to our package
      f <- file(path_short, "w")
      writeLines(shortlines, f)
      close(f)
      if (!is.na(fn_long)) {
        f <- file(path_long, "w")
        writeLines(longlines, f)
        close(f)
      }
    }
  } else {
    f <- file(path_short, "r")
    readLines(f, encoding = "latin1") -> shortlines
    close(f)
    if (!is.na(fn_long)) {
      f <- file(path_long, "r")
      readLines(f, encoding = "latin1") -> longlines
      close(f)
    } else {
      longlines <- NA_character_
    }
  }

  shortlines %<>% strsplit(" ")
  longlines %<>% strsplit(" ")

  icd9ShortCode <- lapply(shortlines, FUN = function(x) trim(x[1]))
  icd9ShortDesc <- lapply(shortlines, FUN = function(x) trim(paste(x[-1], collapse = " ")))
  if (!is.na(longlines)) {
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

parseIcd9LeafDescriptions27 <- function(save = FALSE, fromWeb = NULL) {
  checkmate::assertFlag(save)
  fn <- cmsIcd9ZipUrls[cmsIcd9ZipUrls$version == 27, "other_filename"]
  fp <- file.path("inst", "extdata", fn)
  url <- cmsIcd9ZipUrls[cmsIcd9ZipUrls$version == 27, "url"]
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
    read.zip.url(url, fn) -> tmp_dat
    if (save)
      save_path <- fp
    else
      save_path <- tempfile()
    save_conn <- file(save_path, "w")
    writeLines(tmp_dat, save_conn)
    close(save_conn)
    read.csv(save_path, stringsAsFactors = FALSE) -> x
    if (!save) file.remove(save_path)
  } else {
    f <- file(fp, "r")
    # encoding = "latin1" ?
    read.csv(f, stringsAsFactors = FALSE) -> x
    close(f)
  }

  names(x) <- c("icd9", "descLong", "descShort")
  x <- x[c(1, 3, 2)]
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
           if (y[["end"]] == "") {
             y <- y[-2]
             names(y) <- "major"
           }
           y
         }
  )
}

# http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html
cmsIcd9ZipUrls <- data.frame(
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
                     "V27LONG_SHORT_DX_110909u021012.csv", # also V27LONG_SHORT_DX_110909.csv before update)
                     NA, NA, NA, NA),
  url = c("http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/ICD-9-CM-v32-master-descriptions.zip",
          "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/cmsv31-master-descriptions.zip",
          "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/cmsv30_master_descriptions.zip",
          "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/cmsv29_master_descriptions.zip",
          "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/cmsv28_master_descriptions.zip",
          "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/FY2010Diagnosis-ProcedureCodesFullTitles.zip", # but this one is in a different format!
          # this only contains abbreviated titles: "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v27_icd9.zip",
          "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v26_icd9.zip",
          "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v25_icd9.zip",
          "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v24_icd9.zip",
          "http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/v23_icd9.zip"),
  stringsAsFactors = FALSE
)
