"%nin%" <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}

#' Match pairs of strings to get named vector
#'
#' Match a character vector against a regular expression with at least two
#' parenthetic groupings, returning named vector.
#' @param string vector of strings
#' @param pattern vector of regular expression which should match exactly two
#'   strings for each element in \code{stringr}. If \code{pos} is specified,
#'   this rule is relaxed.
#' @param pos optional pair of integers with positions of the first and second
#'   desired matches, when multiple matches are made by the regular expression
#' @param swap logical scalar, whether to swap the names and values. Default is
#'   not to swap, so the first match becomes the name.
#' @keywords internal manip
#' @noRd
.str_pair_match <- function(string,
                            pattern,
                            pos,
                            swap = FALSE,
                            ...) {
  stopifnot(is.character(string))
  stopifnot(is.character(pattern))
  stopifnot(is.logical(swap))
  pos_missing <- missing(pos)
  if (pos_missing) {
    pos <- c(1L, 2L)
  } else {
    stopifnot(is.numeric(pos), length(pos) == 2, !any(is.na(pos)))
  }
  res <- lapply(
    string,
    function(x) unlist(
        regmatches(x, m = regexec(pattern = pattern, text = x, ...))
      )[-1]
  )
  res <- res[vapply(res, function(x) length(x) != 0, logical(1))]
  res <- do.call(rbind, res)
  stopifnot(!is.null(res))
  if (pos_missing && ncol(res) > max(pos)) {
    stop("the pair matching has three or more ress but needed two.
          Use (?: to have a non-grouping regular expression parenthesis")
  }
  out_names <- res[, ifelse(swap, 2L, 1L)]
  if (any(is.na(out_names))) {
    stop("didn't match some rows:", string[is.na(out_names)],
      call. = FALSE
    )
  }
  out <- res[, ifelse(swap, 1L, 2L)]
  stopifnot(all(!is.na(out)))
  names(out) <- out_names
  out
}

#' @describeIn dot-str_pair_match Return all matches for regular expression
#' @noRd
.str_match_all <- function(string, pattern, ...) {
  string <- as.character(string)
  regmatches(x = string, m = regexec(pattern = pattern, text = string, ...))
}

#' Strip character(s) from character vector
#'
#' After benchmarking, \code{gsub} is probably quicker than
#' \code{stringr}/\code{stringi}. For comorbidity processing.
#' @param x character vector
#' @param pattern passed to \code{gsub} default is \sQuote{ }
#' @param use_bytes single logical passed to \code{base::gsub}, default is the
#'   slightly quicker \code{TRUE}
#' @return character vector of same length as input
#' @keywords internal
#' @noRd
strip <- function(x, pattern = " ", use_bytes = TRUE) {
  gsub(
    pattern = pattern, replacement = "", x = x,
    fixed = TRUE, useBytes = use_bytes
  )
}

#' Mimic the \code{R CMD check} test for non-ASCII characters
#'
#' \code{R CMD check} is quick to tell you where \code{UTF-8} characters are not
#' encoded, but gives no way of finding out which or where
#' @examples
#' \dontrun{
#' sapply(icd9cm_hierarchy, get_non_ascii)
#' get_encodings(icd9cm_hierarchy)
#' sapply(icd9cm_leaf_v32, get_non_ascii)
#' sapply(icd9cm_leaf_v32, get_encodings)
#' }
#' @noRd
#' @keywords internal
get_non_ascii <- function(x) {
  x[is_non_ascii(as_char_no_warn(x))]
}

#' @rdname get_non_ascii
#' @noRd
#' @keywords internal
is_non_ascii <- function(x) {
  is.na(iconv(as_char_no_warn(x), from = "latin1", to = "ASCII"))
}

#' @rdname get_non_ascii
#' @noRd
#' @keywords internal
.get_encodings <- function(x) {
  vapply(x,
    FUN = function(y) unique(Encoding(as_char_no_warn(y))),
    FUN.VALUE = character(1)
  )
}
