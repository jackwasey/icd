# old R versions of code, now reimplemented in C++. Kept here for performance
# comparison and debugging

icd9Children_R <- function(icd9, onlyReal = FALSE, isShort) {
  if (isShort) return(icd9ChildrenShort(icd9, onlyReal = onlyReal))
  icd9ChildrenDecimal(icd9, onlyReal = onlyReal)
}

icd9ChildrenDecimal_R <- function(icd9Decimal, onlyReal = FALSE) {

  parts <- icd9DecimalToParts(icd9Decimal, minorEmpty = "")
  out <- c()
  for (r in rownames(parts)) {
    out <- append(out,
                  paste(
                    parts[[r, "major"]],
                    icd9ExpandMinor(parts[[r, "minor"]],
                                    isE = icd9IsE(parts[[r, "major"]])),
                    sep = "."
                  )
    )
  }
  out <- unique(out)
  out <- icd9AddLeadingZeroesDecimal(out)
  if (onlyReal) return(icd9GetRealDecimal(out))
  out

}

icd9ChildrenShort_R <- function(icd9Short, onlyReal = FALSE) {
  #if (length(icd9Short) == 0) return()
  parts <- icd9ShortToParts(icd9Short, minorEmpty = "")
  out <- c()
  for (r in 1:nrow(parts)) {
    out <- c(out,
             icd9MajMinToShort_R(
               major = parts[[r, "major"]],
               minor = icd9ExpandMinor_R(parts[[r, "minor"]],
                                         isE = icd9IsE(parts[[r, "major"]]))))
  }
  out <- unique(out)
  if (onlyReal) return(icd9GetRealShort(out))
  out
}

icd9IsVE_R <- function(icd9)
  grepl(pattern = "[EeVv]", icd9)

icd9IsV_R <- function(icd9)
  grepl(pattern = "[Vv]", icd9)

icd9IsE_R <- function(icd9)
  grepl(pattern = "[Ee]", icd9)


icd9ExpandMinor_R <- function(minor = "", isE) {

  # this is an error, not just invalidity. Could easily allow multiple values,
  # but I would then have to return a list and post-process that, so I think
  # this keeps things simpler, but maybe slower.
  # TODO: why not allow multiple?
  if (length(minor) > 1) stop("received more than one code to expand")

  if (isE) return(icd9ExpandMinorE(minor = minor))
  icd9ExpandMinorNV_R(minor = minor)
}

icd9ExpandMinorNV_R <- function(minor = "") {

  stopifnot(is.character(minor))

  # minor should be 0-2 character, digits only
  if (nchar(minor) > 2)
    stop("icd9ExpandMinor: starting length already too long! minor is: ", minor)

  # iterate through minors to generate all possible child codes.
  while (max(nchar(minor)) < 2) {
    newStrings <- appendZeroToNine(minor)
    # and add the new ones of any length
    minor <- unique(append(minor, newStrings))
  }
  minor
}

icd9ExpandMinorE <- function(minor = "") {
  if (nchar(minor) == 0) return(c("", as.character(seq(0,9))))
  if (nchar(minor) == 1) return(minor)
  if (nchar(minor) > 1) stop("starting length too long! minor is: ", minor)
  stop("other invalid E minor condition for minor = ", minor)
}

appendZeroToNine <- function(str) {
  stopifnot(allIsNumeric(str))
  apply(expand.grid(str, as.character(0:9),""), 1, paste, collapse="")
}

icd9DecimalToShort_R <- function(icd9Decimal) {

  if (length(icd9Decimal) == 0) return(character())

  # should return everything zero-padded by default. Good default behaviour.
  x <- icd9DecimalToParts(icd9Decimal)

  x[is.na(x$minor), "minor"] <- "" # NA to ""
  y <- paste(
    icd9AddLeadingZeroesMajor(x$major),
    x$minor,sep  = "")
  y[is.na(x$major)] <- NA # to avoid "NA" strings appearing...
  y
}

icd9DecimalToParts_R <- function(icd9Decimal, minorEmpty = "") {

  stopifnot(length(minorEmpty) == 1)
  if (is.na(minorEmpty)) minorEmpty <- NA_character_

  if (length(icd9Decimal) == 0) return(data.frame(major = character(),
                                                  minor = character()))
  icd9Decimal <- icd9ValidNaWarnStopDecimal(icd9Decimal)
  icd9Decimal <- trim(icd9Decimal)
  icd9Decimal[icd9Decimal == ""] <- "." # don't ask
  a <- strsplit(icd9Decimal, ".", fixed = TRUE)
  x <- as.data.frame(
    do.call(rbind, lapply(a, "[", 1:2)),
    stringsAsFactors = FALSE
  )  # this may be slow! (need to flip axes from list to data frame)
  names(x) <- c("major", "minor")
  # if major is NA, then I think the minor must be NA, regardless of minorEmpty.
  x[is.na(x$minor) & !is.na(x$major), "minor"] <- minorEmpty
  x
}
icd9GetMajor_R <- function(icd9, isShort) {
  if (isShort) {
    i <- icd9ShortToParts(icd9Short = icd9)
  } else {
    i <- icd9DecimalToParts(icd9Decimal = icd9)
  }
  i$major
}


icd9ShortToDecimal_R <- function(icd9Short) {
  if (!is.character(icd9Short))
    stop("icd9Short must be a character: number values could be ambiguous if converted blindly to character")

  if (length(icd9Short) == 0) return(character())

  parts <- icd9ShortToParts(icd9Short)
  out <- paste( parts$major, ".", parts$minor, sep = "")
  if (any(parts$minor == "") || is.na(parts$minor)) {

    out[parts$minor == "" | is.na(parts$minor)] <-
      parts[parts$minor == "" | is.na(parts$minor), "major"]
  }

  # paste inevitably makes <NA> into a string "NA", so if major was NA, then
  # restore that:
  out[is.na(parts$major)] <- NA_character_
  out
}


icd9ShortToParts_R <- function(icd9Short, minorEmpty = "") {
  # assume bytes not unicode, for speed.
  eCodes <- icd9IsE(icd9Short)
  icd9Short <- strip(icd9Short)
  x <- icd9MajMinToParts_R(
    major = substr(icd9Short, 0, 3),
    minor = substr(icd9Short, 4, 5)
  )
  # now fix the E codes:
  if (any(eCodes)) {
    x[eCodes, "major"] <- substr(icd9Short[eCodes], 0, 4)
    x[eCodes, "minor"] <- substr(icd9Short[eCodes], 5, 5)
  }
  x$minor[is.na(x$minor)] <- minorEmpty
  if (minorEmpty != "")
    x[!is.na(x$minor) & x$minor == "", "minor"] <- minorEmpty
  x
}

icd9ShortToPartsNV <- function(icd9Short) {
  vapply(X = icd9Short,
         FUN.VALUE = c("", ""),
         FUN = function(x) c(substr(x, 0, 3), substr(x, 4, 5))
  )
}

icd9ShortToPartsE <- function(icd9Short) {
  vapply(X = icd9Short,
         FUN.VALUE = c("", ""),
         FUN = function(x) c(substr(x, 0, 4), substr(x, 5, 5))
  )
}

icd9PartsToShort_R <- function(parts)
  icd9PartsRecompose(parts = parts, isShort = TRUE)

icd9PartsToDecimal_R <- function(parts)
  icd9PartsRecompose(parts = parts, isShort = FALSE)

icd9MajMinToParts_R <- function(major, minor)
  data.frame(major = major, minor = minor, stringsAsFactors = FALSE)

icd9MajMinToShort_R <- function(major, minor)
  icd9PartsToShort(icd9MajMinToParts_R(major, minor))

icd9MajMinToDecimal_R <- function(major, minor)
  icd9PartsToDecimal(parts = icd9MajMinToParts_R(major, minor))

icd9PartsRecompose <- function(parts, isShort) {

  if (is.factor(parts$major))
    parts$major <- asCharacterNoWarn(parts$major)

  if (is.factor(parts$minor))
    parts$minor <- asCharacterNoWarn(parts$minor)

  ## TODO: don't allow Vx single digit V codes to be appended when making a short.

  # only allow pass through of non-zero-padded majors in short if no minor.
  # Otherwise, major is passed through unchanged.

  parts$minor[is.na(parts$minor)] <- ""

  if (isShort) {
    parts$major <- icd9AddLeadingZeroesMajor(parts$major)
    out <- sprintf("%s%s", parts$major, parts$minor)
  }
  else
    out <- sprintf("%s.%s", parts$major, parts$minor)
  out[is.na(parts$major)] <- NA_character_
  out
}

icd9AddLeadingZeroes_R <- function(icd9, isShort) {
  if (isShort) return(icd9AddLeadingZeroesShort(icd9Short = icd9))
  icd9AddLeadingZeroesDecimal(icd9Decimal = icd9)
}

icd9AddLeadingZeroesDecimal_R <- function(icd9Decimal) {
  parts <- icd9DecimalToParts(icd9Decimal)
  parts[["major"]] <- icd9AddLeadingZeroesMajor(parts[["major"]])
  icd9PartsToDecimal(parts = parts)
}

icd9AddLeadingZeroesShort_R <- function(icd9Short) {
  parts <- icd9ShortToParts(icd9Short)
  parts[["major"]] <- icd9AddLeadingZeroesMajor(parts[["major"]])
  icd9PartsToShort(parts = parts)
}

icd9AddLeadingZeroesMajor_R <- function(major)
  sprintf("%03d", asIntegerNoWarn(major))

test_that("appendZeroToNine", {
  expect_error(appendZeroToNine(list(a = c(1, 2)))) # error on silly input
  expect_identical(appendZeroToNine("1"), as.character(10:19))
  expect_identical(appendZeroToNine(1), as.character(10:19))
  expect_identical(appendZeroToNine(""), as.character(0:9))
  expect_identical(sort(appendZeroToNine(c("1", "2"))),
                   as.character(c(10:19, 20:29)))
  expect_identical(sort(appendZeroToNine(c("", "9"))),
                   as.character(c(0:9, 90:99)))
})
