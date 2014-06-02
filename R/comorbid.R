#' @rdname icd9ToComorbidities
#' @title match ICD9 codes
#' @description This does the hard work of finding whether a given icd9 code
#'   falls under a group of reference ICD9 codes. baseCodes is expanded to cover
#'   all possible subgroups, then we look for matches where the given ICD9 codes
#'   appear in the baseCodes. 
#'   http://www.acep.org/Clinical---Practice-Management/V-and-E-Codes-FAQ/
#' @seealso comorbidities.
#' @param icd9codes vector, either decimal (string or floating point) or short
#'   form (must be character)
#' @param baseCodes vector, decimal (but may be string or floating point)
#' @param icd9codeShort true or false, default to accept short codes
#' @param baseCodeShort true or false, default to accept long codes
#' @return logical vector of which icd9codes match or are subcategory of
#'   baseCodes
#' @keywords internal
icd9ToComorbid <- function(icd9codes, baseCodes, icd9codeShort=TRUE, baseCodeShort=FALSE) {
  
  if (!class(icd9codes) %in% c("character","numeric","integer")) stop("icd9ToComorbid expects a character or number vector for the icd9codes to examine, but got: ", class(icd9codes))
  if (!class(baseCodes) %in% c("character","numeric","integer")) stop("icd9ToComorbid expects a character or number vector for the basecodes,to avoid ambiguity with trailing zeroes, but got: ", class(baseCodes))
  if (class(icd9codeShort)!='logical') stop("icd9ToComorbid expects logical value for icd9codeShort")
  if (class(baseCodeShort)!='logical') stop("icd9ToComorbid expects logical value for baseCodeShort")
  
  if (length(icd9codeShort)>1 )  stop("icd9ToComorbid got vector for icd9CodeShort, expected single TRUE or FALSE value")
  if (length(baseCodeShort)>1 )  stop("icd9ToComorbid got vector for baseCodeShort, expected single TRUE or FALSE value")
  if (length(icd9codeShort)==0 )  stop("icd9ToComorbid got empty vector for icd9CodeShort, expected single TRUE or FALSE value")
  if (length(baseCodeShort)==0 )  stop("icd9ToComorbid got empty vector for baseCodeShort, expected single TRUE or FALSE value")
  
  if (length(baseCodes)==0) stop("icd9ToComorbid expects at least one icd9 code to test against")
  
  warnIfInvalidICD9(icd9codes, callingFunction="icd9ToComorbid-icd9codes", short=icd9codeShort)
  #maybe do once, not every loop: stopIfInvalidICD9(baseCodes, callingFunction="icd9ToComorbid-baseCodes", short=baseCodeShort)
  
  # take a regular string of an ICD9 code of format (ABC.zxyz) with or without leading and trailing zeroes.
  #top level ICD9 code and return T/F if the icd9codes fall within subgroups
  icd9codes %in% icd9DecimalToShort(
    c(
      lapply(baseCodes, FUN=function(x) icd9ExpandBaseCode(icd9=x, short=baseCodeShort)), 
      recursive=TRUE)
  )
}


#' @rdname icd9ToComorbidities
#' @title get co-morbidities for given list of visitId codes of in-patients
#' @description merges the visitId list against the pre-calculated icd9 
#'   comorbidities get co-morbidities flagged as Present On Arrival (POA) for 
#'   given list of PATCOMs get co-morbidities actively marked not Present On 
#'   Arrival (POA) for given list of PATCOMs
#' @param dat contains the data with at least one field named by \code{visitId},
#'   and likely a field "poa" for the present on arrival flag.
#' @param visitId defaults to 'visitId'
#' @param icd9lk is one of the pre-prepared lookup tables. 
#'   'comorbidAllInpt','comorbidPoaInpt','comorbidNotPoaInpt' (TODO: this needs
#'   isolating)
#' @param mergeFun is the function used to merge the comorbidity data with the
#'   visitId list, using visitId as the key. Can be left as default \code{merge}
#'   but this has limited ability when identical fields appear, and in how field
#'   name clashes can rename; neither does it report on how effective the merge
#'   was.
#' @return data.frame with input visit IDs merged with comorbidities
#' @keywords internal
lookupComorbiditiesAll <- function(dat, 
                                   visitId = "visitId", 
                                   icd9lk = 'comorbidAllInpt',
                                   mergeFun = merge) {
  
  if (icd9lk %nin% c('comorbidAllInpt','comorbidPoaInpt','comorbidNotPoaInpt')) 
    stop("the icd9 comorbidities must be one of: 'comorbidAllInpt','comorbidPoaInpt','comorbidNotPoaInpt' but I received ", icd9lk)
  
  # comorbidAllInpt <- icd9codesToComorbidities(icd9diagInpt, visitId="patcom", icd9Field="i9diag")
  
  mp <- do.call(mergeFun, list(x=dat, by.x=visitId, y=get(icd9lk), by.y=visitId, leftOuterJoin=T))
  
  # update just the new logical rows replacing NA with FALSE. This happens when a patient has no comorbidities.
  comorbidityNames <- names(get(icd9lk))
  comorbidityNames <- comorbidityNames[comorbidityNames != visitId] # select only the logical fields, not the patcom field
  mp[, comorbidityNames] <- mp[, comorbidityNames] & !is.na(mp[, comorbidityNames])
  mp
}

#' @rdname icd9ToComorbidities
#' @export
lookupComorbiditiesAll <- function(dat, visitId)
  lookupComorbidities(dat, visitId, 'comorbidAllInpt')

#' @rdname icd9ToComorbidities
#' @export
lookupComorbiditiesPoa <- function(dat, visitId)
  lookupComorbidities(dat, visitId, 'comorbidPoaInpt')

#' @rdname icd9ToComorbidities
#' @export
lookupComorbiditiesPoa <- function(dat, visitId)
  lookupComorbidities(dat, visitId, 'comorbidNotPoaInpt')
#' @rdname icd9ToComorbidities
#' @title merge comorbidities with icd9 codes per visitId (or other identity)
#' @description default response is Jack's mapping of comorbidities which was 
#'   curated by hand based on several published lists of ICD9 codes used to
#'   ennumerate comorbidities. Other options are \code{deyo} and \code{ahrq}. 
#'   This is slow with long lists of patients, so intended to be used as
#'   intermediate step to save files like comorbidPoaInpt
#' @param icd9df data.frame with fields specified by visitId and icd9Code.
#'   icd9code is assumed to be a non-decimal 'short' form ICD9 code.
#' @param icd9Mapping list of the comorbidities with each top-level list item
#'   containing a vector of decimal ICD9 codes
#' @export
icd9Comorbidities <- function(icd9df, visitId="visitId", icd9Field="icd9Code", 
                              icd9Mapping) {
  
  # validate mapping here, or not at all. Will stop checking down the chain of function calls.
  
  i <- cbind(
    icd9df[visitId],
    vapply(
      X = names(icd9Mapping), # loop through names of icd9 mapping, i.e. one comorbidity at a time
      FUN.VALUE = rep(FALSE, length(icd9df[[icd9Field]])), # way faster with vapply, and it keeps the logicals instead of making them character
      FUN = function(comorbidity) {
        icd9ToComorbid( 
          asCharacterNoWarn(icd9df[[icd9Field]]), # drop factor down to character codes #TODO: is this necessary?
          icd9Mapping[[comorbidity]] # provide vector of base ICD9 codes for this comorbidity group
        )
      }
    )
  )
  ag <- aggregate( x=i[,-which(names(i)==visitId)], by=list(i[[visitId]]), FUN = any, simplify=F)
  names(ag)[1] <- visitId
  ag
}

#' @rdname icd9ToComorbidities
#' @title gets those comorbidities where the "Present on Arrival" (POA) flag is set or not set.
#' @description this is not a simple binary, since many codes are exempt, unspecified, or unknown. Therefore, two options are given: get all the comorbidities where the POA flag was definitely -ve, coded as "N" or definitely +ve and coded as "Y". Negating one set won't give the other set unless all codes were either Y or N.
#' #describeIn icd9Comorbidities
#' @export
icd9comorbiditiesNotPoa <- function(icd9df, icd9Mapping, visitId="visitId",
                                    icd9Field="icd9Code", poaField="poa")
  icd9comorbidities(icd9df[ is.na(icd9df[[poaField]]) | icd9df[[poaField]] != "N",],
                    visitId=visitId, icd9Field=icd9Field, icd9Mapping=icd9Mapping)

#' @rdname icd9ToComorbidities
#' @export
icd9comorbiditiesPoa <- function(icd9df, icd9Mapping, visitId="visitId", 
                                 icd9Field="icd9Code", poaField="poa")
  icd9comorbidities(icd9df[!is.na(icd9df[[poaField]]) & icd9df[[poaField]] == "Y",],
                    visitId=visitId, icd9Field=icd9Field, icd9Mapping=icd9Mapping)


#' @title parse AHRQ and ICD9-CM data
#' @description Takes the raw data taken directly from the AHRQ web site and 
#'   parses into RData. It is then saved in the development tree data directory,
#'   so this is an internal function, used in generating the package itself!
#'   @param save logical, whether to try to save the output data in the source tree.
#' @return list of lists, name value pairs, and where a single name was 
#'   associated with multiple further name-value pairs, this is presented as a 
#'   sub-list. This is primarily required because of the obtuse SAS FORMAT data
#'   structure: the AHRQ codes are hidden in a sublist of the first item.
parseAhrqSas <- function(save=F, path="~/icd9/data") {
  #ahrq.dx <- read.csv(file=system.file("extdata", "ccs_multi_dx_tool_2013.csv", package="icd9"), quote="'\"")
  #ahrq.pr <- read.csv(file=system.file("extdata", "ccs_multi_pr_tool_2014.csv", package="icd9"), quote="'\"")
  
  # all fields suitable for 'factor' class, except ICD.9.CM.CODE, which has no repeated values.
  #ahrq.dx[["ICD.9.CM.CODE"]] <- asCharacterNoWarn(ahrq.dx[["ICD.9.CM.CODE"]])
  
  # now work on groupings:
  #ag<-aggregate(ICD.9.CM.CODE ~ CCS.LVL.1.LABEL, data=ahrq.dx, FUN=paste)
  # TODO to be continued...
  
  f <- file(system.file("extdata", "comformat2012-2013.txt", package="icd9"), "r")
  ahrqAll <- sasFormatExtract(readLines(f)) # no special encoding?
  
  ahrqComorbidWork <- ahrqAll[["$RCOMFMT"]]
  # Boom. The remainder is DRG stuff.
  
  ahrqComorbid <- list()
  
  for (cmd in names(ahrqComorbidWork)) {
    #flog.info("working on %s", cmd)
    somePairs <- strsplit(x=ahrqComorbidWork[[cmd]], split="-")
    #flog.debug("got these values and ranges:", somePairs, capture=T)
    out <- as.list(somePairs[lapply(somePairs, length) == 1]) # non-range values just go on list
    thePairs <- somePairs[lapply(somePairs, length) == 2]
    #flog.debug("got these ranges:", thePairs, capture=T)
    out <- append(out, as.list(lapply(thePairs, function(x) icd9ExpandRangeShort(x[1], x[2]))))
    # update ahrqComorbid with full range of icd9 codes:
    ahrqComorbid[[cmd]] <- unlist(out)
  }
  
  # save the data in the development tree, so the package user doesn't need to decode it themselves.
  if (save) saveSourceTreeData("ahrqComorbid", path = path)
  
}
