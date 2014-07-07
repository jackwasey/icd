#' @title explain ICD9 codes
#' @description convert full format (123.45 style) ICD9 codes into the name and
#'   description for human review there are official ICD9-CM data tables, not
#'   with conversion to decimal notation, but to the textual format.
#' @note TODO: it will be useful to have S3 ICD-9 short and long classes for
#'   situations like this where we could easily dispatch on short or long type,
#'   or even use a print.icd9decimal or print.icd9Short S3 method to display
#'   ICD-9 codes.
#' @template icd9-any
#' @template icd9-short
#' @template icd9-decimal
#' @template isShort
#' @param doCondense single logical value which indicates whether to condense the given set of ICD-9 codes by replacing subsets of codes with 'parent' codes which exactly encompass certain subsets. E.g. If all cholera diagnoses are provided, only '001 - Cholera' needs to be displayed, not all subtypes. This is currently partially implemented. See issue #3 in github.
#' @examples
#' icd9ExplainShort(ahrqComorbid[[1]][1:3])
#' @return data frame, or list of data frames, with fields for ICD9 code, name
#'   and description, derived from datamart lookup table
#' @seealso package comorbidities
#' @references \url{http://www.stata.com/help.cgi?icd9}
#' @export
icd9Explain <- function(icd9, isShort, doCondense = TRUE) UseMethod("icd9Explain")

#' @rdname icd9Explain
#' @export
icd9ExplainShort <- function(icd9Short, doCondense = TRUE) icd9Explain(icd9Short, isShort = TRUE, doCondense = doCondense)

#' @rdname icd9Explain
#' @export
icd9ExplainDecimal <- function(icd9Decimal, doCondense = TRUE) icd9Explain(icd9Decimal, isShort = FALSE, doCondense = doCondense)

#' @describeIn icd9Explain explain alll ICD-9 codes in a list of vectors
#' @export
icd9Explain.list <- function(icd9, isShort, doCondense = TRUE) lapply(icd9, icd9Explain, isShort = isShort, doCondense = doCondense)

#' @describeIn icd9Explain explain character vector of ICD-9 codes
#' @export
icd9Explain.character <- function(icd9, isShort, doCondense = TRUE) {

  if (!isShort) {
    # make sure there are preceding zeroes, in order to match the icd9CmDesc data.
    icd9 <- icd9AddLeadingZeroesDecimal(icd9)
    icd9 <- icd9DecimalToShort(icd9)
  }
  orphans <- c()
  if (doCondense) {
    # find common parent ICD-9 codes, but only if they have a description
    icd9 <- icd9CondenseToExplainShort(icd9, invalidAction = "warn")
    # find those codes without explanations:
    unexplainedParents <- icd9[!(icd9 %in% icd9CmDesc[["icd9"]])]
    # get all their children, so we can work backwards to the highest-level explanations
    orphans <- icd9CondenseToExplainShort(icd9ChildrenShort(unexplainedParents), invalidAction = "warn")
  }
  out <- icd9CmDesc[ icd9CmDesc[["icd9"]] %in% c(icd9, orphans), ]
  row.names(out) <- NULL
  names(out) <- c("ICD-9", "Diagnosis", "Description")
  if (!isShort) out[["ICD-9"]] <- icd9ShortToDecimal(out[["ICD-9"]])
  out
}

#' @describeIn icd9Explain explain numeric vector of ICD-9 codes, with warning
#' @export
icd9Explain.numeric <- function(icd9, isShort, doCondense = TRUE) {
  warning("Numeric ICD-9 codes are unable to accurately represent actual ICD-9 codes.
          Converting to character, but beware of inevitable errors.")
  icd9Explain.character(as.character(icd9), isShort = isShort)
}

#' @title describe ICD-9 codes, guess whether short or long
#' @description partly implemented. Goal is to guess whether codes are short or
#'   decimal form, then to call icd9Explain with the condense argument.
#'   Currently condense works, but not with the icd9 lookup table currently in
#'   use. Not exporting this function until it works as intended.
#' @keywords internal
icd9Tell <- function(icd9) {
  icd9 <- as.character(icd9)
  if (is.list(icd9)) {
    testCodes <- icd9[[1]]
  } else {
    testCodes <- icd9
  }
  if (mean(icd9ValidDecimal(testCodes) > 0.9)) return(icd9ExplainDecimal(icd9, doCondense = TRUE))
  icd9ExplainShort(icd9, doCondense = TRUE)
}

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
#' @export
parseIcd9Cm <- function(icd9path = system.file("extdata","CMS32_DESC_LONG_DX.txt",
                                               package = 'icd9'),
                        save = FALSE,
                        path = "~/icd9/data") {
  f <- file(icd9path, "r")
  r <- readLines(f, encoding = "latin1")
  close(f)
  r <- strsplit(r, " ")
  icd9LongCode <- lapply(r, FUN = function(row) row[1])
  icd9LongDesc <- lapply(r, FUN = function(row) paste(row[-c(1,2)], collapse = " "))

  f <- file(system.file("extdata", "CMS32_DESC_SHORT_DX.txt", package='icd9'), "r")
  r <- readLines(f) # this is ascii
  close(f)
  r <- strsplit(r, " ")
  icd9ShortCode <- lapply(r, FUN = function(row) row[1])
  icd9ShortDesc <- lapply(r, FUN = function(row) paste(row[-c(1,2)], collapse = " "))
  icd9CmDesc <- data.frame(
    icd9 = unlist(icd9LongCode),
    descLong = unlist(icd9LongDesc),
    descisShort = unlist(icd9ShortDesc),
    stringsAsFactors = FALSE)

  # attempt to write the date from the source file to RData in the package source tree.
  if (save) saveSourceTreeData("icd9CmDesc", path = path)

  message("The following long descriptions contain UTF-8 codes:")
  message(paste(icd9CmDesc[grep(pattern = "UTF", Encoding(icd9CmDesc$descLong)), ], sep = ", "))

  invisible(icd9CmDesc)
}

#' @title get ICD-9 Chapters from vector of ICD-9 codes
#' @description work-in-progress
#' @param icd9-any
#' @param isShort
#' @param invalid
#' @keywords internal
icd9ChapterShort <- function(icd9Short, isShort, invalidAction = icd9InvalidActions) {
  majors <- icd9ShortToMajor(icd9Short, match.arg(invalidAction))
  chapterExpandedRanges <- lapply(icd9CmChapters, function(x) x[[1]] %i9mj% x[[2]])
#TODO complete this with 'spread' type function, as done when generating co-morbidities. Maybe use dplyr
}
