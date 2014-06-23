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
#' 
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
    if (!exists(x = icd9lk, inherits=T)) stop("the icd9 comorbidities pre-generated lookup table '", icd9lk, "' doesn't exist in current environments")
    icd9lk <- get("icd9lk", inherits=T)
  } 
  stopifnot(visitId %in% names(icd9lk), visitId %in% names(dat))
  stopifnot(exists(mergeFun))
  
  mp <- do.call(mergeFun, list(x=dat, by.x=visitId, y=get(icd9lk), by.y=visitId, leftOuterJoin=T, ...))
  
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
#' @param sasPath single character string containing path to SAS FORMAT
#'   definition code.
#' @param save logical, whether to try to save the output data in the source 
#'   tree.
#' @param path character vector of unit length containing path to the source 
#'   package data directory. Default is ~/icd9/data
#' @return list of lists, name value pairs, and where a single name was 
#'   associated with multiple further name-value pairs, this is presented as a 
#'   sub-list. This is primarily required because of the obtuse SAS FORMAT data 
#'   structure: the AHRQ codes are hidden in a sublist of the first item.
#' @keywords internal
parseAhrqSas <- function(sasPath = system.file("extdata", "comformat2012-2013.txt", package = "icd9"),
                         save = FALSE, path="~/icd9/data") {
  f <- file(sasPath, "r")
  ahrqAll <- sasFormatExtract(readLines(f)) # these seem to be ascii encoded
  close(f)
  
  ahrqComorbidWork <- ahrqAll[["$RCOMFMT"]]
  # Boom. The remainder of the AHRQ SAS input file consists of DRG definitions (TODO).
  
  ahrqComorbid <- list()
  
  for (cmd in names(ahrqComorbidWork)) {
    somePairs <- strsplit(x = ahrqComorbidWork[[cmd]], split = "-")
    # non-range values just go on list
    out <- as.list(somePairs[lapply(somePairs, length) == 1])
    thePairs <- somePairs[lapply(somePairs, length) == 2]
    out <- append(out, lapply(thePairs, function(x) icd9ExpandRangeShort(x[1], x[2])))
    # update ahrqComorbid with full range of icd9 codes:
    ahrqComorbid[[cmd]] <- unlist(out)
  }
  
  # drop this superfluous finale which allocates any other ICD-9 code to the
  # "Other" group
  ahrqComorbid[[" "]] <- NULL
  
  # todo: save/return the DRG mappings.
  
  # save the data in the development tree, so the package user doesn't need to
  # decode it themselves.
  if (save) saveSourceTreeData("ahrqComorbid", path = path)
  
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

