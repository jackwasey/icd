# EXCLUDE COVERAGE START


asCharacterNoWarn <- function(x) {
  old <- options(warn = - 1)
  on.exit(options(old))
  if (is.factor(x)) x <- levels(x)[x]
  as.character(x)
}

"%nin%" <- function(x, table)
  match(x, table, nomatch = 0) == 0

trim <- function(x)
  gsub("^\\s+|\\s+$", "", x)

strip <- function (x, pattern = " ", useBytes = TRUE)
  gsub(pattern = pattern, replacement = "", x = x,
       fixed = TRUE, useBytes = useBytes)

#' @title encode TRUE as 1, and FALSE as 0 (integers)
#' @description when saving data as text files for distribution, printing large
#'   amounts of text containing TRUE and FALSE is inefficient. Convert to binary
#'   takes more R memory, but allows more compact output TODO: test
#' @details Taken from my
#'   \href{http://cran.r-project.org/web/packages/jwutil/index.html}{\code{jwutil}
#'   package}.#' @param x dataframe which may contain logical fields
#' @return data frame without logical fields
#' @keywords internal manip
logicalToBinary <- function(x) {
  stopifnot(is.data.frame(x))
  if (any(dim(x) == 0))
    stop("got zero in at least one dimension in data frame. %d, %d",
         dim(x)[1], dim(x)[2])

  # can condense this code into a one-liner, but this is clearer:
  logical_fields <- names(x)[sapply(x, is.logical)]
  if (is.na(logical_fields) || length(logical_fields) == 0) return(x)

  #update just the logical fields with integers
  x[, logical_fields] <-
    vapply(
      X         = x[, logical_fields],
      FUN       = function(y) ifelse(y, 1L, 0L),
      FUN.VALUE = integer(length = dim(x)[1])
    )
  x
}


#' @title return the actual matches from a bracketed regex
#' @description Be careful: this may throw funny results for exotic regex, but
#'   so far, it seems okay. it also drops the first result which always seems to
#'   be a duplicate or whole-string match.
#' @details Taken from my
#'   \href{http://cran.r-project.org/web/packages/jwutil/index.html}{\code{jwutil}
#'   package}.
#' @param pattern regular expression: if it has bracketed sections, these
#'   submatches are returned
#' @param text is the string to match against. This vector should be the same
#'   length as the pattern vector, or the patern vector should be length one.
#' @param ... are additional parameters passed to regexec and regmatches. I
#'   haven't tried this: it may need two separate variables containing lists of
#'   params, since this will send everything to both functions.
#' @param dropEmpty logical whether to drop rows with no matches
#' @return list of character vectors, list length being the length of the inptu
#'   text vector.
#' @keywords internal
strMultiMatch <- function(pattern, text, dropEmpty = FALSE, ...) {
  # unlist puts the name in the first position, which I don't think I ever want.
  result <- lapply(
    text, function(x) unlist(
      regmatches(
        x = x,
        m = regexec(
          pattern = pattern,
          text=x, ...),
        ...)
    )[ - 1]
  )
  if (!dropEmpty) return(result)
  result[sapply(result, function(x) length(x) != 0)]
}

#' @rdname strMultiMatch
#' @description \code{strPaitMatch} differs in that there should only be two
#'   pairs of parenthesis, then the first (by default) becomes the name, and the
#'   second the value.
#' @param swap logical scalar, whether to swap the names and values. Default is
#'   not to swap, so the first match becomes the name.
#' @keywords internal
strPairMatch <- function(pattern, text, swap = FALSE, dropEmpty = FALSE, ...) {
  stopifnot(length(pattern) == 1)
  stopifnot(length(text) > 0)
  stopifnot(length(swap) == 1)
  stopifnot(length(dropEmpty) == 1)
  stopifnot(is.character(pattern))
  stopifnot(is.character(text))
  stopifnot(is.logical(swap))
  stopifnot(is.logical(dropEmpty))

  res <- strMultiMatch(pattern = pattern, text = text,
                       dropEmpty = dropEmpty, ...)
  stopifnot(all(sapply(res, function(x) length(x) == 2)))

  outNames <- vapply(X = res,
                     FUN = "[",
                     FUN.VALUE = character(1),
                     ifelse(swap, 2, 1))
  stopifnot(all(!is.na(outNames)))

  out <- vapply(X = res,
                FUN = "[",
                FUN.VALUE = character(1),
                ifelse(swap, 1, 2))
  stopifnot(all(!is.na(out)))

  names(out) <- outNames
  out
}

# EXCLUDE COVERAGE END
