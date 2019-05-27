#' Swap names and values of a vector
#'
#' Swap names and values of a vector. Non-character values are implicitly
#' converted to names.
#' @param x named vector
#' @return vector, with values being the names of the input vector, and names
#' being the previous values.
#' @noRd
#' @keywords internal
.swap_names_vals <- function(x) {
  stopifnot(is.atomic(x))
  stopifnot(!is.null(names(x)))
  new_names <- unname(x)
  x <- names(x)
  names(x) <- new_names
  x
}

#' Parse a (sub)chapter text description with parenthesised range
#'
#' @param x vector of descriptions followed by ICD code ranges
#' @return list of two-element character vectors, the elements being named
#'   'start' and 'end'.
#' @noRd
#' @keywords internal manip
.chapter_to_desc_range <- function(x, re_major) {
  stopifnot(is.character(x), is.character(re_major))
  re_code_range <- paste0(
    "(.*)[[:space:]]?\\((",
    re_major, ")-(",
    re_major, ")\\)"
  )
  re_code_single <- paste0("(.*)[[:space:]]?\\((", re_major, ")\\)")
  mr <- .str_match_all(x, re_code_range)
  ms <- .str_match_all(x, re_code_single)
  okr <- vapply(mr, length, integer(1)) == 4L
  oks <- vapply(ms, length, integer(1)) == 3L
  if (!all(okr || oks)) {
    stop("Problem matching\n", x[!(okr || oks)], call. = FALSE)
  }
  m <- ifelse(okr, mr, ms)
  out <- lapply(m, function(y) c(start = y[[3]], end = y[[length(y)]]))
  names(out) <- vapply(m, function(y) trimws(.to_title_case(y[[2]])),
    FUN.VALUE = character(1)
  )
  out
}

.chapter_to_desc_range.icd9 <- function(x) { # nolint
  .chapter_to_desc_range(x, re_major = re_icd9_major_bare)
}

.chapter_to_desc_range.icd10 <- function(x) { # nolint
  .chapter_to_desc_range(x, re_major = re_icd10_major_bare)
}

.get_chapter_ranges_from_flat <- function(flat_hier = icd10cm2019,
                                          field = "chapter") {
  u <- if (is.factor(flat_hier[[field]])) {
    levels(flat_hier[[field]])
  } else {
    as.character(unique(flat_hier[[field]]))
  }
  three_digits <- as.character(flat_hier[["three_digit"]])
  setNames <- function(x) {
    y <- x
    names(y) <- x
    y
  }
  lapply(
    setNames(u),
    function(chap) {
      td <- sort(
        unique(three_digits[flat_hier[[field]] == chap])
      )
      c(
        start = td[1],
        end = td[length(td)]
      )
    }
  )
}

.to_title_case <- function(x) {
  for (split_char in c(" ", "-", "[")) {
    s <- strsplit(x, split_char, fixed = TRUE)[[1]]
    x <- paste(toupper(substring(s, 1L, 1L)), substring(s, 2L),
      sep = "", collapse = split_char
    )
  }
  x
}

#' Internal use only: get data from the icd.data package, without relying on it
#' being attached
#'
#' Some data is hidden in active bindings, so it may be downloaded on demand,
#' and some is lazy-loaded. This will work for regular package members, active
#' bindings, and lazy data, whether or not the package is attached or loaded.
#'
#' This is really just needed for the transition from icd 3.3 to icd 4.0, and
#' icd.data > 1.0
#' @param alt If the data cannot be found, this value is returned. Default is
#'   \code{NULL}.
#' @keywords internal
#' @noRd
get_icd_data <- function(data_name, alt = NULL) {
  if (!is.character(data_name)) {
    data_name <- deparse(substitute(data_name))
  }
  ns <- asNamespace("icd")
  if (exists(data_name, ns)) {
    return(get(data_name, ns))
  }
  if (.exists_in_cache(data_name)) {
    return(.get_from_cache(data_name))
  }
  alt
}

.exists_in_ns <- function(name) {
  all(vapply(name, .exists_in_ns_single, logical(1), USE.NAMES = FALSE))
}

.exists_in_ns_single <- function(name) {
  stopifnot(length(name) == 1L)
  pkg_ns <- asNamespace("icd")
  lazy_env <- pkg_ns[[".__NAMESPACE__."]][["lazydata"]]
  exists(name, lazy_env) || exists(name, pkg_ns)
}

.ls_lazy <- function(all.names = TRUE, ...) {
  pkg_ns <- asNamespace("icd")
  ls(pkg_ns[[".__NAMESPACE__."]][["lazydata"]],
    all.names = all.names
  )
}

.ls_in_ns <- function(all.names = TRUE, ...) {
  pkg_ns <- asNamespace("icd")
  ls(pkg_ns, all.names = all.names)
}
