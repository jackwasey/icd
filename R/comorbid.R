#' @title match ICD9 codes
#' @aliases "%i9in%"
#' @description This does the hard work of finding whether a given icd9 code
#'   falls under a group of reference ICD9 codes. icd9Reference is expanded to cover
#'   all possible subgroups, then we look for matches where the given ICD9 codes
#'   appear in the icd9Reference.
#'   http://www.acep.org/Clinical---Practice-Management/V-and-E-Codes-FAQ/
#' @seealso comorbidities.
#' @templateVar icd9AnyName "icd9,icd9Reference"
#' @template icd9-any
#' @template short
#' @template validate
#' @param shortReference logical, see argument \code{short}
#' @return logical vector of which icd9 match or are subcategory of
#'   icd9Referenec
#' @keywords internal
icd9InReferenceCode <- function(icd9, icd9Reference, short = TRUE, shortReference = TRUE, validate = FALSE, validateReference = FALSE) {

  if (!class(icd9) %in% c("character", "numeric", "integer"))
    stop("icd9InReferenceCode expects a character or number vector for icd9, but got: ", class(icd9))
  if (!class(icd9Reference) %in% c("character", "numeric", "integer"))
    stop("icd9InReferenceCode expects a character or number vector for the basecodes,
         to avoid ambiguity with trailing zeroes, but got: ", class(icd9Reference))
  stopifnot(class(short) == 'logical', class(shortReference) == "logical")

  if (length(short) >  1 )
    stop("icd9InReferenceCode got vector for short, expected single TRUE or FALSE value")
  if (length(shortReference) >  1 )
    stop("icd9InReferenceCode got vector for shortReference, expected single TRUE or FALSE value")
  if (length(short) == 0 )
    stop("icd9InReferenceCode got empty vector for short, expected single TRUE or FALSE value")
  if (length(shortReference) == 0 )
    stop("icd9InReferenceCode got empty vector for shortReference, expected single TRUE or FALSE value")

  if (length(icd9Reference) == 0) stop("icd9InReferenceCode expects at least one icd9 code to test against")

  if (validate) stopIfInvalidIcd9(icd9, callingFunction = "icd9InReferenceCode-icd9", short = short)
  if (validateReference) stopIfInvalidIcd9(icd9Reference, callingFunction = "icd9InReferenceCode-icd9", short = shortReference)

  kids <- memSpawnRefKids(icd9Reference, shortReference)

  # convert to short form to make comparison
  if (short == FALSE) icd9 <- icd9DecimalToShort(icd9)
  if (shortReference == FALSE) kids <- icd9DecimalToShort(kids)

  icd9 %in% kids
}

#' spawn reference codes into all possible lower-level codes (and memoise)
#'
#' take a regular string of an ICD9 code of format (ABC.zxyz) with or without
#' leading and trailing zeroes. top level ICD9 code and return T/F if the icd9
#' fall within subgroups. This takes several seconds on an unimpressive desktop
#' PC, so would benefit from memoization.
#'
#' @import memoise
#' @keywords internal
spawnReferenceChildren <-
  function(icd9Reference, shortReference) {
    c(
      lapply(
        icd9Reference,
        FUN = function(x) icd9ExpandBaseCode(icd9 = x, short = shortReference)
      ),
      recursive = TRUE
    )
  }

# this runs outside of a function, on package load
library(memoise)
memSpawnRefKids <- memoise::memoise(spawnReferenceChildren)

#' @rdname icd9InReferenceCode
#' @export
#' @examples
#' "1024" %i9in% "102"
#' "1024" %i9in% c("102","1025")
#' c("102", "1024","1025") %i9in% "102"
#' c("102", "1024","1025") %i9in% c("1024", "1025")
#' c("102", "1024","1025") %i9in% c("102", "1024", "1025")
"%i9in%" <- function(icd9, icd9Reference)
  icd9InReferenceCode(icd9 = icd9, icd9Reference = icd9Reference)

#' @title lookup pre-calculated co-morbidities for given list of visit IDs
#' @description merges the data frame \code{dat} with pre-calculated icd9
#'   comorbidities by \code{visitId}
#' @param dat contains the data with at least one field named by \code{visitId},
#'   and likely a field "poa" for the present on arrival flag. Additional fields
#'   are preserved. Merging fields with duplicated visitId will behave according
#'   to default of \code{mergeFun}.
#' @param icd9lk is one of the pre-prepared lookup tables. e.g.
#'   'comorbidAllInpt','comorbidPoaInpt','comorbidNotPoaInpt' If a character
#'   string is given (vector of unit length), then the name is used to lookup
#'   the data in current environment tree. If a data frame is given, this is
#'   used as the data to lookup co-morbidities for the given
#' @template visitid
#' @param mergeFun is the function used to merge the comorbidity data with the
#'   visitId list, using visitId as the key. Can be left as default \code{merge}
#'   but this has limited ability when identical fields appear, and in how field
#'   name clashes can rename; neither does it report on how effective the merge
#'   was.
#' @param ... additional arguments passed to \code{mergeFun}
#' @return data.frame with input visit IDs merged with comorbidities
#' @keywords internal
lookupComorbidities <- function(dat,
                                icd9lk,
                                visitId = "visitId",
                                mergeFun = merge,
                                ...) {
  if (is.character(icd9lk)) {
    if (!exists(x = icd9lk, inherits = TRUE))
      stop("the icd9 comorbidities pre-generated lookup table '", icd9lk, "' doesn't exist in current environments")
    icd9lk <- get("icd9lk", inherits = TRUE)
  }
  stopifnot(visitId %in% names(icd9lk), visitId %in% names(dat))
  stopifnot(exists(mergeFun))

  mp <- do.call(
    mergeFun,
    list(x = dat, by.x = visitId, y = get(icd9lk), by.y = visitId, leftOuterJoin = TRUE, ...))

  # update just the new logical rows replacing NA with FALSE. This happens when
  # a patient has no comorbidities.
  comorbidityNames <- names(get(icd9lk))
  # select only the logical fields, not the patcom field
  comorbidityNames <- comorbidityNames[comorbidityNames != visitId]
  mp[, comorbidityNames] <- mp[, comorbidityNames] & !is.na(mp[, comorbidityNames])
  mp
}

#' @title merge comorbidities with icd9 codes per visitId (or other identity)
#' @description default comorbidity mapping is with AHRQ data. This is slow with
#'   long lists of patients, so intended to be used as intermediate step to save
#'   files like comorbidPoaInpt
#' @param icd9df data.frame with fields specified by visitId and icd9Code.
#'   icd9Code is assumed to be a non-decimal 'short' form ICD9 code. There is a
#'   many to many ratio of icd9:visitId. This table contains multiple visitId
#'   rows, with one row per ICD-9 code. Therefore, every ICD-9 code listed is
#'   associated with at least one visit ID.
#' @template visitid
#' @template icd9field
#' @param icd9Mapping list (or name of a list if character vector of length one
#'   is given as argument) of the comorbidities with each top-level list item
#'   containing a vector of decimal ICD9 codes. This is in the form of a list,
#'   with the names of the items corresponding to the comorbidities (e.g. "HTN",
#'   or "diabetes") and the contents of each list item being a character vector
#'   of short-form (no decimal place but ideally zero left-padded) ICD-9 codes.
#' @param validateMapping logical, whether to validate all the ICD-9 codes in
#'   the mapping list. Default is not to check. If validation fails, stop with
#'   an error. This is probably worth doing at least once for each mapping used,
#'   since there should never be an error in mapping. There is overhead to check
#'   the mapping each time, so not done by default. Could consider using
#'   \code{memoise} to cache the result of the check. (TODO)
#' @param shortMapping logical, whether the mapping is defined with short ICD-9
#'   codes (TRUE, the default), or decimal if set to FALSE.
#' @export
icd9Comorbidities <- function(icd9df,
                              visitId = "visitId",
                              icd9Field = "icd9",
                              icd9Mapping = ahrqComorbid,
                              validateMapping = F,
                              shortMapping = T) {

  stopifnot(visitId %in% names(icd9df), icd9Field %in% names(icd9df))

  if (is.character(icd9Mapping)) {
    stopifnot(exists(icd9Mapping))
    icd9Mapping <- get(icd9Mapping)
  }

  if (validateMapping) {
    if (shortMapping) {
      stopifnot(all(unlist(lapply(icd9Mapping, FUN = icd9ValidShort), use.names=F)))
    } else {
      stopifnot(all(unlist(lapply(icd9Mapping, FUN = icd9ValidDecimal), use.names=F)))
    }
  }

  # loop through names of icd9 mapping, and put the results together so each
  # column is one comorbidity in a data frame. This is much faster with vapply,
  # and it keeps the logicals instead of making them characters
  i <- cbind(
    icd9df[visitId],
    vapply(
      X = names(icd9Mapping),
      FUN.VALUE = rep(FALSE, length(icd9df[[icd9Field]])),
      FUN = function(comorbidity) {
        icd9InReferenceCode(
          # drop factor down to character codes #TODO: is this necessary or desirable?
          asCharacterNoWarn(icd9df[[icd9Field]]),
          # provide vector of base ICD9 codes for this comorbidity group
          icd9Mapping[[comorbidity]]
        )
      }
    )
  )
  ag <- aggregate(
    x = i[, -which(names(i) == visitId)], # all cols except visit ID will be aggregated
    by = list(visitId = i[[visitId]]), # group by the visitId
    FUN = any,
    simplify = TRUE
  )

  ag
}

#' @rdname icd9Comorbidities
#' @title gets those comorbidities where the "Present on Arrival" (POA) flag is
#'   not set, or set to "N"
#' @description this is not a simple binary, since many codes are exempt,
#'   unspecified, or unknown. Therefore, two options are given: get all the
#'   comorbidities where the POA flag was definitely -ve, coded as "N" or
#'   definitely +ve and coded as "Y". Negating one set won't give the other set
#'   unless all codes were either Y or N. #describeIn icd9Comorbidities
#' @param poaField The name of column in the data frame which contains the
#'   Present On Arrival flag. The flag itself is a single character, typically
#'   one of "Y", "N", "E", "X", "U" or empty. The poaField is a character vector
#'   of length one.
#' @export
icd9ComorbiditiesNotPoa <- function(icd9df, icd9Mapping, visitId = "visitId",
                                    icd9Field = "icd9Code", poaField = "poa") {
  stopifnot(poaField %in% names(icd9df))
  icd9Comorbidities(icd9df[ is.na(icd9df[[poaField]]) | icd9df[[poaField]] != "N",],
                    visitId=visitId, icd9Field=icd9Field, icd9Mapping=icd9Mapping)
}

#' @rdname icd9Comorbidities
#' @title gets those comorbidities where the "Present on Arrival" (POA) flag is
#'   set to "Y"
#' @export
icd9ComorbiditiesPoa <- function(icd9df, icd9Mapping, visitId = "visitId",
                                 icd9Field = "icd9Code", poaField = "poa") {
  stopifnot(poaField %in% names(icd9df))
  icd9Comorbidities(icd9df[!is.na(icd9df[[poaField]]) & icd9df[[poaField]] == "Y",],
                    visitId = visitId, icd9Field = icd9Field, icd9Mapping = icd9Mapping)
}


#' @title parse AHRQ data
#' @description Takes the raw data taken directly from the AHRQ web site and
#'   parses into RData. It is then saved in the development tree data directory,
#'   so this is an internal function, used in generating the package itself!
#' @template savesas
#' @param returnAll logical which, if TRUE, will result in the invisible return of ahrqComorbidAll result, otherwise, ahrqComorbid is reutrned.
#' @return list of lists, name value pairs, and where a single name was
#'   associated with multiple further name-value pairs, this is presented as a
#'   sub-list. This is primarily required because of the obtuse SAS FORMAT data
#'   structure: the AHRQ codes are hidden in a sublist of the first item.
#' @keywords internal
parseAhrqSas <- function(sasPath = system.file("extdata", "comformat2012-2013.txt", package="icd9"),
                         save = FALSE,
                         saveDir = "~/icd9/data",
                         returnAll = FALSE) {
  f <- file(sasPath, "r")
  ahrqAll <- sasFormatExtract(readLines(f)) # these seem to be ascii encoded
  close(f)

  ahrqComorbidWork <- ahrqAll[["$RCOMFMT"]]
  # Boom. The remainder of the AHRQ SAS input file consists of DRG definitions (TODO).

  ahrqComorbidAll <- list()

  for (cmd in names(ahrqComorbidWork)) {
    somePairs <- strsplit(x = ahrqComorbidWork[[cmd]], split = "-")
    # non-range values just go on list
    out <- as.list(somePairs[lapply(somePairs, length) == 1])
    thePairs <- somePairs[lapply(somePairs, length) == 2]
    out <- append(out, lapply(thePairs, function(x) icd9ExpandRangeShort(x[1], x[2])))
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

  # officially, AHRQ HTN with complications means that HTN on its own should be unset.
  # however, this is not feasible here, since we just package up the data into a list, and it can be used however the user wishes. It would not be hard to write an AHRQ specific function to do this if needed, but it makes more sense to me


  # todo: save/return the DRG mappings.

  # save the data in the development tree, so the package user doesn't need to
  # decode it themselves.
  if (save) saveSourceTreeData("ahrqComorbidAll", path = saveDir)
  if (save) saveSourceTreeData("ahrqComorbid", path = saveDir)

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
#' @template savesas
#' @keywords internal datasets
parseQuanSas <- function(sasPath = "http://mchp-appserv.cpe.umanitoba.ca/concept/ICD9_E_Charlson.sas.txt",
                         save = FALSE, saveDir = "~/icd9/data") {
  quanSas <- readLines(sasPath, warn = FALSE)
  qlets <- sasExtractLetStrings(quanSas)
  qlabels <- qlets[grepl("LBL[[:digit:]]+", names(qlets))]
  quanCharlsonComorbid <- qlets[grepl("DC[[:digit:]]+", names(qlets))]
  names(quanCharlsonComorbid) <- unlist(unname(qlabels))

  if (save) saveSourceTreeData("quanCharlsonComorbid", path = saveDir)

  invisible(quanCharlsonComorbid)
}

#' @title Generate Elixhauser comorbidities
#' @description This function uses the \code{\%i9d\%} operator, so cannot be done
#'   as an R file in the \code{data} directory. The data is documented in
#'   \code{datadocs.R}.
#' @param saveDir path to directory to save the data. This is typically the data
#'   folder in the devleopment source tree.
#' @keywords internal datasets
parseElixhauser <- function(save = FALSE, saveDir = "~/icd9/data") {
  elixhauserComorbid <- list(
    chf = c("398.91", "402.11", "402.91", "404.11", "404.13", "404.91", "404.93", "428.0" %i9d% "428.9"),
    arrhythmia = c("426.1", "426.11", "426.13", "426.2" %i9d% "426.53", "426.6" %i9d% "426.89", "427.0", "427.2", "427.31", "427.60", "427.9", "785", "V45.0", "V53.3"),
    valve = c("93.20" %i9d% "93.24", "394.0" %i9d% "397.1", "424.0" %i9d% "424.91", "746.3" %i9d% "746.6", "V42.2", "V43.3"),
    pulm.circ = c("416.0" %i9d% "416.9", " 417.9"),
    pvd = c("440.0" %i9d% "440.9", "441.2", "441.4", "441.7", "441.9", "443.1" %i9d% "443.9", "447.1", "557.1", "557.9", "V43.4"),
    htn = c("401.1", "401.9"),
    htncx = c("402.10", "402.90", "404.10", "404.90", "405.11", "405.19", "405.91", "405.99"),
    paralysis = c("342.0" %i9d% "342.12", "342.9" %i9d% "344.9"),
    neuro.other = c("331.9", "332.0", "333.4", "333.5", "334.0" %i9d% "335.9", "340", "341.1" %i9d% "341.9", "345.00" %i9d% "345.11", "345.40" %i9d% "345.51", "345.80" %i9d% "345.91", "348.1", "348.3", "780.3", "784.3"),
    chronic.pulm = c("490" %i9d% "492.8", "493.00" %i9d% "493.91", "494", "495.0" %i9d% "505", "506.4"),
    dm.uncomp = c("250.00" %i9d% "250.33"),
    dm.comp = c("250.40" %i9d% "250.73", "250.90" %i9d% "250.93"),
    hypothyroid = c("243" %i9d% "244.2", "244.8", "244.9"),
    renal = c("403.11", "403.91", "404.12", "404.92", "585", "586", "V42.0", "V45.1", "V56.0", "V56.8"),
    liver = c("70.32", "70.33", "70.54", "456.0", "456.1", "456.20", "456.21", "571.0", "571.2", "571.3", "571.40" %i9d% "571.49", "571.5", "571.6", "571.8", "571.9", "572.3", "572.8", "V42.7"),
    pud = c("531.70", "531.90", "532.70", "532.90", "533.70", "533.90", "534.70", "534.90", "V12.71"),
    hiv = c("42" %i9d% "44.9"),
    lymphoma = c("200.00" %i9d% "202.38", "202.50" %i9d% "203.01", "203.8" %i9d% "203.81", "238.6", "273.3", "V10.71", "V10.72", "V10.79"),
    mets = c("196.0" %i9d% "199.1"),
    solid.tumor = c("140.0" %i9d% "172.9", "174.0" %i9d% "175.9", "179" %i9d% "195.8", "V10.00" %i9d% "V10.9"),
    rheum = c("701.0", "710.0" %i9d% "710.9", "714.0" %i9d% "714.9", "720.0" %i9d% "720.9", "725"),
    coag = c("286.0" %i9d% "286.9", "287.1", "287.3" %i9d% "287.5"),
    obesity = c("278.0"),
    wt.loss = c("260" %i9d% "263.9"),
    lytes = c("276.0" %i9d% "276.9"),
    anemia.loss = c("280.0"),
    anemia.def = c("280.1" %i9d% "281.9", "285.9"),
    etoh = c("291.1", "291.2", "291.5", "291.8", "291.9", "303.90" %i9d% "303.93", "305.00" %i9d% "305.03", "V11.3"),
    drugs = c("292.0", "292.82" %i9d% "292.89", "292.9", "304.00" %i9d% "304.93", "305.20" %i9d% "305.93"),
    psychoses = c("295.00" %i9d% "298.9", "299.10" %i9d% "299.11"),
    depression = c("300.4", "301.12", "309.0", "309.1", "311")
  )

  # convert to short form, for consistency with other mappings.
  elixhauserComorbid <- lapply(elixhauserComorbid, icd9DecimalToShort)

  if (save) saveSourceTreeData("elixhauserComorbid", path = saveDir)

  invisible(elixhauserComorbid)
}

