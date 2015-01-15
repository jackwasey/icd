#' @title parse all known mappings and save to development tree
#' @param path directory to save in, default is \code{data}
#' @keywords internal
icd9ParseAndSaveMappings <- function() {
  parseAhrqSas(save = TRUE)
  parseElix(save = TRUE)
  parseQuanDeyoSas(save = TRUE)
  parseQuanElix(save = TRUE)

  #parseIcd9Descriptions(save = TRUE, path = path)
  parseIcd9Chapters(save = TRUE)
  # this is not strictly a parsing step, but is quite slow
  icd9GetChaptersHierarchy(save= TRUE)
}

#' @title parse AHRQ data
#' @description Takes the raw data taken directly from the AHRQ web site and
#'   parses into RData. It is then saved in the development tree data directory,
#'   so this is an internal function, used in generating the package itself!
#' @template savesas
#' @template parse-template
#' @param returnAll logical which, if TRUE, will result in the invisible return
#'   of ahrqComorbidAll result, otherwise, ahrqComorbid is reutrned.
#' @keywords internal
parseAhrqSas <- function(sasPath = system.file("extdata",
                                               "comformat2012-2013.txt",
                                               package = "icd9"),
                         condense = FALSE,
                         save = FALSE,
                         path = "data",
                         returnAll = FALSE) {
  f <- file(sasPath, "r")
  ahrqAll <- sasFormatExtract(readLines(f)) # these seem to be ascii encoded
  close(f)

  ahrqComorbidWork <- ahrqAll[["$RCOMFMT"]]
  # Boom. The remainder of the AHRQ SAS input file consists of DRG definitions
  # (TODO).

  ahrqComorbidAll <- list()

  for (cmd in names(ahrqComorbidWork)) {
    somePairs <- strsplit(x = ahrqComorbidWork[[cmd]], split = "-")
    # non-range values just go on list
    out <- as.list(somePairs[lapply(somePairs, length) == 1])
    thePairs <- somePairs[lapply(somePairs, length) == 2]
    out <- append(out, lapply(thePairs,
                              function(x) icd9ExpandRangeShort(x[1], x[2])))
    # update ahrqComorbid with full range of icd9 codes:
    ahrqComorbidAll[[cmd]] <- unlist(out)
  }

  # drop this superfluous finale which allocates any other ICD-9 code to the
  # "Other" group
  ahrqComorbidAll[[" "]] <- NULL

  ahrqComorbid <- ahrqComorbidAll

  ahrqComorbid$HTNCX <- c(
    ahrqComorbid$HTNCX, # some codes already in this category
    ahrqComorbid$HTNPREG,
    ahrqComorbid$OHTNPREG,
    ahrqComorbid$HTNWOCHF,
    ahrqComorbid$HTNWCHF,
    ahrqComorbid$HRENWORF,
    ahrqComorbid$HRENWRF,
    ahrqComorbid$HHRWOHRF,
    ahrqComorbid$HHRWCHF,
    ahrqComorbid$HHRWRF,
    ahrqComorbid$HHRWHRF)

  ahrqComorbid$CHF <- c(
    ahrqComorbid$CHF, # some codes already in this category
    ahrqComorbid$HTNWCHF,
    ahrqComorbid$HHRWCHF,
    ahrqComorbid$HHRWHRF)

  ahrqComorbid$RENLFAIL <- c(
    ahrqComorbid$RENLFAIL, # some codes already in this category
    ahrqComorbid$HRENWRF,
    ahrqComorbid$HHRWRF,
    ahrqComorbid$HHRWHRF)


  ahrqComorbid[c("HTNPREG", "OHTNPREG", "HTNWOCHF",
                 "HTNWCHF","HRENWORF", "HRENWRF", "HHRWOHRF",
                 "HHRWCHF", "HHRWRF", "HHRWHRF")] <- NULL

  # officially, AHRQ HTN with complications means that HTN on its own should be
  # unset. however, this is not feasible here, since we just package up the data
  # into a list, and it can be used however the user wishes. It would not be
  # hard to write an AHRQ specific function to do this if needed, but it makes
  # more sense to me

  # todo: save/return the DRG mappings.

  # either fully expand or fully condense the results
  if (condense) {
    ahrqComorbid <- lapply(ahrqComorbid, icd9CondenseToMajor)
    ahrqComorbidAll <- lapply(ahrqComorbidAll, icd9CondenseToMajor)
  } else {
    ahrqComorbid <- lapply(ahrqComorbid, function(x)
      icd9ChildrenShort(x, invalidAction = "stop"))
    ahrqComorbidAll <- lapply(ahrqComorbidAll, function(x)
      icd9ChildrenShort(x, invalidAction = "stop"))
  }

  names(ahrqComorbid) <- icd9::ahrqComorbidNamesHtnAbbrev

  # save the data in the development tree, so the package user doesn't need to
  # decode it themselves.
  if (save) {
    saveInDataDir("ahrqComorbidAll")
    saveInDataDir("ahrqComorbid")
  }

  if (returnAll) return(invisible(ahrqComorbidAll))

  invisible(ahrqComorbid)
}

#TODO: function to extract these standard ICD-9 groupings, not focussed on
#co-morbidities, but useful for classification ahrq.dx <-
# read.csv(file=system.file(
#   "extdata",
#   "ccs_multi_dx_tool_2013.csv",
#   package="icd9"), quote="'\"")
# ahrq.pr <- read.csv(file=system.file(
#   "extdata",
#   "ccs_multi_pr_tool_2014.csv",
#   package="icd9"), quote="'\"")

#all fields suitable for 'factor' class, except ICD.9.CM.CODE, which has no
#repeated values.
# ahrq.dx[["ICD.9.CM.CODE"]] <- asCharacterNoWarn(ahrq.dx[["ICD.9.CM.CODE"]])

# now work on groupings:
#ag<-aggregate(ICD.9.CM.CODE ~ CCS.LVL.1.LABEL, data=ahrq.dx, FUN=paste)
# TODO to be continued...

#' @title parse original SAS code defining Quan's update of Deyo comorbidities.
#' @description As with \code{parseAhrqSas}, this function reads SAS code, and
#'   in, a very limited way, extracts definitions. In this case the code uses
#'   LET statements, with strings or lists of strings. This saves and invisibly
#'   returns a list with names corresponding to the comorbidities and values as
#'   a vector of 'short' form (i.e. non-decimal) ICD9 codes. Unlike
#'   \code{parseAhrqSas}, there are no ranges defined, so this interpretation is
#'   simpler.
#'
#'   With thanks to Dr. Quan, I have permission to distribute his SAS code.
#'   Previously, the SAS code would be downloaded from the University of
#'   Manitoba at
#'   \url{http://mchp-appserv.cpe.umanitoba.ca/concept/ICD9_E_Charlson.sas.txt}.
#'   There are structural differences between this version and the version
#'   directly from Dr. Quan, however, the parsing results in identical data.
#' @template savesas
#' @template parse-template
#' @keywords internal
parseQuanDeyoSas <- function(sasPath = NULL,
                             condense = FALSE,
                             save = FALSE,
                             path = "data") {
  if (is.null(sasPath)) sasPath <- system.file("extdata",
                                               "ICD9_E_Charlson.sas",
                                               package = "icd9")

  quanSas <- readLines(sasPath, warn = FALSE)
  qlets <- sasExtractLetStrings(quanSas)
  qlabels <- qlets[grepl("LBL[[:digit:]]+", names(qlets))]
  quanDeyoComorbid <- qlets[grepl("DC[[:digit:]]+", names(qlets))]
  names(quanDeyoComorbid) <- unlist(unname(qlabels))

  # use validation: takes time, but these are run-once per package creation (and
  # test) tasks.
  if (condense)
    quanDeyoComorbid <- lapply(
      quanDeyoComorbid,
      icd9CondenseToMajor, invalidAction = "stop")
  else
    quanDeyoComorbid <- lapply(
      quanDeyoComorbid,
      icd9ChildrenShort)

  names(quanDeyoComorbid) <- icd9::charlsonComorbidNamesAbbrev
  if (save) saveInDataDir("quanDeyoComorbid")
  invisible(quanDeyoComorbid)
}

#' @title Generate Quan's revised Elixhauser comorbidities
#' @template parse-template
#' @keywords internal
parseQuanElix <- function(condense = FALSE,
                          save = FALSE,
                          path = "data") {
  quanElixComorbid <- list(
    chf = c("398.91", "402.01", "402.11", "402.91", "404.01", "404.03",
            "404.11", "404.13", "404.91", "404.93", "425.4" %i9d% "425.9",
            "428"),
    arrhythmia = c("426.0", "426.13", "426.7", "426.9", "426.10", "426.12",
                   "427.0" %i9d% "427.4", "427.6" %i9d% "427.9", "785.0",
                   "996.01", "996.04", "V45.0", "V53.3"),
    valve = c("93.2", "394" %i9d% "397", "424", "746.3" %i9d% "746.6", "V42.2",
              "V43.3"),
    pulm.circ = c("415.0", "415.1", "416", "417.0", "417.8", "417.9"),
    pvd = c("093.0", "437.3", "440", "441", "443.1" %i9d% "443.9", "447.1",
            "557.1", "557.9", "V43.4"),
    htn = c("401"),
    htncx = c("402" %i9d% "405"),
    paralysis = c("334.1", "342", "343", "344.0" %i9d% "344.6", "344.9"),
    neuro.other = c("331.9", "332.0", "332.1", "333.4", "333.5", "333.92",
                    "334", "335", "336.2", "340", "341", "345", "348.1",
                    "348.3", "780.3", "784.3"),
    chronic.pulm = c("416.8", "416.9", "490" %i9d% "505", "506.4", "508.1",
                     "508.8"),
    dm.uncomp = c("250.0" %i9d% "250.3"),
    dm.comp = c("250.4" %i9d% "250.9"),
    hypothyroid = c("240.9", "243", "244", "246.1", "246.8"),
    renal = c("403.01", "403.11", "403.91", "404.02", "404.03", "404.12",
              "404.13", "404.92", "404.93", "585", "586", "588", "V42.0",
              "V45.1", "V56"),
    liver = c("70.22", "70.23", "70.32", "70.33", "70.44", "70.54", "70.6",
              "70.9", "456.0" %i9d% "456.2", "570", "571",
              "572.2" %i9d% "572.8", "573.3", "573.4", "573.8", "573.9",
              "V42.7"),
    pud = c("531.7", "531.9", "532.7", "532.9", "533.7", "533.9", "534.7",
            "534.9"),
    hiv = c("42" %i9d% "44"),
    lymphoma = c("200" %i9d% "202", "203.0", "238.6"),
    mets = c("196" %i9d% "199"),
    solid.tumor = c("140" %i9d% "172", "174" %i9d% "195"),
    rheum = c("446", "701.0", "710.0" %i9d% "710.4", "710.8", "710.9", "711.2",
              "714", "719.3", "720", "725", "728.5", "728.89", "729.30"),
    coag = c("286", "287.1", "287.3" %i9d% "287.5"),
    obesity = c("278.0"),
    wt.loss = c("260" %i9d% "263", "783.2", "799.4"),
    lytes = c("253.6", "276"),
    anemia.loss = c("280.0"),
    anemia.def = c("280.1" %i9d% "280.9", "281"),
    etoh = c("265.2", "291.1" %i9d% "291.3", "291.5" %i9d% "291.9", "303.0",
             "303.9", "305.0", "357.5", "425.5", "535.3", "571.0" %i9d% "571.3",
             "980", "V11.3"),
    drugs = c("292", "304", "305.2" %i9d% "305.9", "V65.42"),
    psychoses = c("293.8", "295", "296.04", "296.14", "296.44", "296.54", "297",
                  "298"),
    depression = c("296.2", "296.3", "296.5", "300.4", "309", "311")
  )

  quanElixComorbid <- lapply(
    quanElixComorbid,
    function(x) icd9DecimalToShort(x, invalidAction = "stop"))

  if (condense)
    quanElixComorbid <- lapply(
      quanElixComorbid,
      function(x) icd9CondenseToMajor(x, invalidAction = "stop"))
  else
    quanElixComorbid <- lapply(
      quanElixComorbid,
      icd9ChildrenShort)

  names(quanElixComorbid) <- icd9::quanElixComorbidNamesHtnAbbrev
  if (save) saveInDataDir("quanElixComorbid")
  invisible(quanElixComorbid)
}

#' @title Generate Elixhauser comorbidities
#' @description This function uses the \code{\%i9d\%} operator, so cannot be
#'   done as an R file in the \code{data} directory. The data is documented in
#'   \code{datadocs.R}.
#' @template parse-template
#' @keywords internal
parseElix <- function(condense = FALSE, save = FALSE, path = "data") {
  elixComorbid <- list(
    chf = c("398.91", "402.11", "402.91", "404.11", "404.13", "404.91",
            "404.93", "428.0" %i9d% "428.9"),
    arrhythmia = c("426.1", "426.11", "426.13", "426.2" %i9d% "426.53",
                   "426.6" %i9d% "426.89", "427.0", "427.2", "427.31", "427.60",
                   "427.9", "785", "V45.0", "V53.3"),
    valve = c("93.20" %i9d% "93.24", "394.0" %i9d% "397.1",
              "424.0" %i9d% "424.91", "746.3" %i9d% "746.6", "V42.2", "V43.3"),
    pulm.circ = c("416.0" %i9d% "416.9", " 417.9"),
    pvd = c("440.0" %i9d% "440.9", "441.2", "441.4", "441.7", "441.9",
            "443.1" %i9d% "443.9", "447.1", "557.1", "557.9", "V43.4"),
    htn = c("401.1", "401.9"),
    htncx = c("402.10", "402.90", "404.10", "404.90", "405.11", "405.19",
              "405.91", "405.99"),
    paralysis = c("342.0" %i9d% "342.12", "342.9" %i9d% "344.9"),
    neuro.other = c("331.9", "332.0", "333.4", "333.5", "334.0" %i9d% "335.9",
                    "340", "341.1" %i9d% "341.9", "345.00" %i9d% "345.11",
                    "345.40" %i9d% "345.51", "345.80" %i9d% "345.91", "348.1",
                    "348.3", "780.3", "784.3"),
    chronic.pulm = c("490" %i9d% "492.8", "493.00" %i9d% "493.91", "494",
                     "495.0" %i9d% "505", "506.4"),
    dm.uncomp = c("250.00" %i9d% "250.33"),
    dm.comp = c("250.40" %i9d% "250.73", "250.90" %i9d% "250.93"),
    hypothyroid = c("243" %i9d% "244.2", "244.8", "244.9"),
    renal = c("403.11", "403.91", "404.12", "404.92", "585", "586", "V42.0",
              "V45.1", "V56.0", "V56.8"),
    liver = c("70.32", "70.33", "70.54", "456.0", "456.1", "456.20", "456.21",
              "571.0", "571.2", "571.3", "571.40" %i9d% "571.49", "571.5",
              "571.6", "571.8", "571.9", "572.3", "572.8", "V42.7"),
    pud = c("531.70", "531.90", "532.70", "532.90", "533.70", "533.90",
            "534.70", "534.90", "V12.71"),
    hiv = c("42" %i9d% "44.9"),
    lymphoma = c("200.00" %i9d% "202.38", "202.50" %i9d% "203.01",
                 "203.8" %i9d% "203.81", "238.6", "273.3", "V10.71", "V10.72",
                 "V10.79"),
    mets = c("196.0" %i9d% "199.1"),
    solid.tumor = c("140.0" %i9d% "172.9", "174.0" %i9d% "175.9",
                    "179" %i9d% "195.8", "V10.00" %i9d% "V10.9"),
    rheum = c("701.0", "710.0" %i9d% "710.9", "714.0" %i9d% "714.9",
              "720.0" %i9d% "720.9", "725"),
    coag = c("286.0" %i9d% "286.9", "287.1", "287.3" %i9d% "287.5"),
    obesity = c("278.0"),
    wt.loss = c("260" %i9d% "263.9"),
    lytes = c("276.0" %i9d% "276.9"),
    anemia.loss = c("280.0"),
    anemia.def = c("280.1" %i9d% "281.9", "285.9"),
    etoh = c("291.1", "291.2", "291.5", "291.8", "291.9",
             "303.90" %i9d% "303.93", "305.00" %i9d% "305.03", "V11.3"),
    drugs = c("292.0", "292.82" %i9d% "292.89", "292.9",
              "304.00" %i9d% "304.93", "305.20" %i9d% "305.93"),
    psychoses = c("295.00" %i9d% "298.9", "299.10" %i9d% "299.11"),
    depression = c("300.4", "301.12", "309.0", "309.1", "311")
  )

  elixComorbid <- lapply(
    elixComorbid, function(x)
      icd9DecimalToShort(x, invalidAction = "stop"))

  # convert to short form, for consistency with other mappings.
  if (condense) {
    elixComorbid <- lapply(
      elixComorbid,
      function(x) icd9CondenseToMajor(x, invalidAction = "stop"))
  } else {
    elixComorbid <- lapply(
      elixComorbid,
      icd9ChildrenShort)
  }

  names(elixComorbid) <- icd9::elixComorbidNamesHtnAbbrev
  if (save) saveInDataDir("elixComorbid")
  invisible(elixComorbid)
}

# AHRQ hierarchy.

parseAhrqHierarchy <- function() {
  read.zip.url(
    "http://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2014.zip",
    filename = "ccs_multi_dx_tool_2013.csv",
    FUN = read.csv,
    row.names = NULL
  )
}
# 'single level' CCS AHRQ diagnoses:
# http://www.hcup-us.ahrq.gov/toolssoftware/ccs/Single_Level_CCS_2014.zip

# 'multi level' CCS AHRQ diagnoses
# http://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2014.zip

#' @title read the ICD-9-CM description data as provided by the Center for
#'   Medicaid Services.
#' @description ICD9-CM data unfortunately has no comma separation, so have to
#'   pre-process. Note that this canonical data doesn't specify non-diagnostic
#'   higher-level codes, just the specific diagnostic 'child' codes.
#' @details ideally would get ICD9-CM data zip directly from CMS web page, and
#'   extract, but the built-in unzip only extracts the first file in a zip.
#' @param icd9path path of the source data which is in /extddata in the
#'   installed package, but would be in inst/extdata in development tree.
#' @param save logical whether to attempt to save output in package source tree
#'   data directory
#' @param path Absolute path in which to save parsed data
#' @return invisibly return the result
#' @keywords internal
parseIcd9Descriptions <- function(icd9path =
                                    system.file("extdata",
                                                "CMS32_DESC_LONG_DX.txt",
                                                package = "icd9"),
                                  save = FALSE,
                                  path = "data") {
  f <- file(icd9path, "r")
  r <- readLines(f, encoding = "latin1")
  close(f)
  r <- strsplit(r, " ")
  icd9LongCode <- lapply(r,
                         FUN = function(row)
                           trim(row[1]))
  icd9LongDesc <- lapply(r,
                         FUN = function(row)
                           trim(paste(row[-1], collapse = " ")))

  f <- file(system.file("extdata",
                        "CMS32_DESC_SHORT_DX.txt",
                        package="icd9"),
            "r")
  r <- readLines(f) # this is ascii
  close(f)
  r <- strsplit(r, " ")
  icd9ShortCode <- lapply(r,
                          FUN = function(row)
                            trim(row[1]))
  icd9ShortDesc <- lapply(r,
                          FUN = function(row)
                            trim(paste(row[-1], collapse = " ")))
  icd9CmDesc <- data.frame(
    icd9 = unlist(icd9LongCode),
    descLong = unlist(icd9LongDesc),
    descisShort = unlist(icd9ShortDesc),
    stringsAsFactors = FALSE)

  # attempt to write the date from the source file to RData in the package
  # source tree. disable saving this: use icd9Hierarchy instead. if (save)
  # saveInDataDir("icd9CmDesc")

  message("The following long descriptions contain UTF-8 codes:")
  message(paste(icd9CmDesc[grep(pattern = "UTF",
                                Encoding(icd9CmDesc$descLong)), ],
                sep = ", "))

  invisible(icd9CmDesc)
}

#' @title Read higher-level ICD-9 structure from a reliable web site
#' @description Previous iteration attempted to use the canonical data from
#'   annual RTF files from the CDC, however, this was ridiculously tricky and
#'   error prone, so now using a reliable website and scraping. Will still
#'   confirm the results with tests.
#' @keywords internal
parseIcd9Chapters <- function(year = NULL,
                              save = FALSE) {
  if (is.null(year))
    year <- "2014"
  else
    year <- as.character(year)
  if (format(Sys.time(), "%Y") != year)
    warning(sprintf("Getting ICD-9 data for %s which is not the current year.
              Tests were written to validate extraction of 2014 data.", year))

  icd9Chapters <- icd9WebParseGetList(year)
  icd9ChaptersSub <- list()
  icd9ChaptersMajor <- list()
  for (chap in names(icd9Chapters)) {
    if (chap == "Diseases Of The Blood And Blood-Forming Organs" ||
          chap == "Congenital Anomalies") {
      # these have no subchapter, straight into the three-digit codes
      icd9ChaptersMajor <- c(icd9ChaptersMajor,
                             icd9WebParseGetList(year, icd9Chapters[[chap]]))
    } else {
      # construct URL for next level and get the sub chapters
      subchaps <- icd9WebParseGetList(year, icd9Chapters[[chap]])
      icd9ChaptersSub <- c(icd9ChaptersSub, subchaps)
      # loop through each subchapter to get the majors:
      for (subchap in names(subchaps)) {
        icd9ChaptersMajor <- c(icd9ChaptersMajor,
                               icd9WebParseGetList(year,
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

  if (save) {
    saveInDataDir("icd9Chapters")
    saveInDataDir("icd9ChaptersSub")
    saveInDataDir("icd9ChaptersMajor")
  }
  invisible(list(icd9Chapters = icd9Chapters,
                 icd9ChaptersSub = icd9ChaptersSub,
                 icd9ChaptersMajor = icd9ChaptersMajor))
}

icd9WebParseStartEndToRange <- function(v) {
  paste(v[["start"]], v[["end"]], sep = "-")
}

# internal only
icd9WebParseGetList <- function(year, chapter = NULL, subchap = NULL) {
  #print(paste(year, chapter, subchap))
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
  li <-  memReadHtmlList(doc = icd9url, which = 1)
  # swap so descriptions (second on web page) become the vector names
  v <- jwutil::strPairMatch("^([VvEe0-9-]*)[[:space:]]*(.*)$", li, swap = TRUE)
  lapply(v,
         FUN = function(x) {
           y <- unlist(strMultiMatch(pattern = "^([VvEe0-9]+)-?([VvEe0-9]+)?$",
                                     text = x))
           names(y) <- c("start", "end")
           if (y[["end"]] == "") {
             y <- y[-2]
             names(y) <- "major"
           }
           y
         }
  )
}

# don't assume XML is loaded - it is a suggested package, and needed only for
# creating packaged data.
if (suppressWarnings(require("XML",
                             character.only = TRUE, quietly = TRUE))) {
  if (suppressWarnings(require("memoise",
                               character.only = TRUE, quietly = TRUE)))
    memReadHtmlList <- memoise::memoise(XML::readHTMLList)
  else
    memReadHtmlList <- XML::readHTMLList
} else
  memReadHtmlList <- NULL
