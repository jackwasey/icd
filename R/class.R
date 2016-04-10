# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

# get and set class types this package uses The master list is: icd9 icd9cm
# icd10 icd10cm icd_long icd_wide icd_decimal icd_short
# icd_comorbidity_map
#
# I'm not sure of the best order, so I think I'll avoid assuming any order,
# except for more specific ICD types coming first.

# list currently implemented classes for validation.
icd9_sub_classes <- c("icd9cm")
icd9_classes <- c(icd9_sub_classes, "icd9")
icd10_sub_classes <- c("icd10cm")
icd10_classes <- c(icd10_sub_classes, "icd10")
icd_version_classes <- c(icd9_classes, icd10_classes)
icd_data_classes <- c("icd_long_data", "icd_wide_data")
icd_other_classes <- c("icd_comorbidity_map")
icd_all_classes <- c(icd_version_classes, icd_data_classes, icd_other_classes)
icd_system_classes <- c("data.frame", "list", "numeric", "character", "factor")

icd_conflicts_with_icd9 <- function(x) inherits(x, icd10_classes)
icd_conflicts_with_icd10 <- function(x) inherits(x, icd9_classes)
icd_conflicts_with_icd9cm <- icd_conflicts_with_icd9

icd_check_conflict_with_icd10 <- function(x)
  if (icd_conflicts_with_icd10(x))
    stop("Trying to set ICD-10 class on an object which already has an ICD-9 class")

icd_check_conflict_with_icd9 <- function(x)
  if (icd_conflicts_with_icd9(x))
    stop("Trying to set ICD-10 class on an object which already has an ICD-9 class")

# for now, but could be refined:
icd_check_conflict_with_icd9cm <- icd_check_conflict_with_icd9
icd_check_conflict_with_icd10cm <- icd_check_conflict_with_icd10

#' Check whether there are any ICD class conflicts
#'
#' Some classes cannot be simultaneously true for one vector of ICD codes, e.g.
#' ICD-9-CM and ICD-9 from the WHO. This function returns \code{TRUE} if there
#' are any such combinations of incompatible classes. If the constructor
#' functions such as \code{icd9cm()} and \code{icd10()} are used, this should
#' never happen.
#' @param x input object to test for class conflicts
#' @examples
#' bad_codes <- c("100", "A01", "V100", "E999.0")
#' class(bad_codes) <- c("icd9", "icd10cm")
#' stopifnot(icd:::icd_classes_conflict(bad_codes))
#' \dontrun{
#' # benchmark attributes vs attr for getting and setting
#' rp <- "a"
#' new_attr <- list(k = "b")
#' times <- 5e6
#' microbenchmark::microbenchmark(attr(rp, "k") <- "b", attributes(rp) <- new_attr, times = times)
#' microbenchmark::microbenchmark(attr(rp, "k"), attributes(rp)[["k"]], times = times)
#' microbenchmark::microbenchmark(attr(rp, "k"), attributes(rp), times = times)
#' microbenchmark::microbenchmark(attr(rp, "k", exact = TRUE), attributes(rp), times = times)
#' }
#' @keywords internal
icd_classes_conflict <- function(x) {
  is.icd9(x) && is.icd10(x)
}

#' prefer an order of classes
#'
#' The order of classes can matter because, for some functions, we'd prefer to
#' decide what to do based on a higher level structure, e.g. whether the
#' structure is a comorbidity map before caring if it is ICD-9 or ICD-10. I
#' can't see how it matters whether we prioritize long/wide and short/decimal
#' yet, so won't test.
#' @param x any object which may or may not have classes from this package

#' @keywords internal
icd_classes_ordered <- function(x) {

  m <- match(class(x), c(icd_other_classes,
                         icd_version_classes,
                         icd_data_classes,
                         icd_system_classes))
  all(diff(m) >= 0, na.rm = TRUE)
}

#' Construct ICD-9 data types
#'
#' Takes an R structure and sets class to an ICD type. In the case of ICD-9 and
#' ICD-10 codes, if a particular sub-type is set, e.g. ICD-9-CM (\code{icd9cm}),
#' then an ICD-9 class (\code{icd9}) is also set.
#'
#' The \code{as.} function e.g. \code{as.icd9}, do checking and try to put
#' multiple classes in a nice order. Calling the bare constructor, e.g.
#' \code{icd9} just prepends the new class and returns without any checks. The
#' latter is much faster, but for most uses, \code{as.icd9} and siblings would
#' be better.
#'
#' Some features make more sense as attributes. E.g. setting code type to
#' \code{short} or \code{decimal}.
#'
#' @param x object to set class \code{icd9}
#' @param warn single logical value, if \code{TRUE} will gives warning when
#'   converting between types. ICD-9 to ICD-10 will cause an error regardless.
#' @name set_icd_class
#' @examples
#' x = as.icd10("A1009")
#' attr(x, "icd_short_diag") <- TRUE
#' x
#' attributes(x) <- list(icd_short_diag = NULL)
#' x
#'
#' # despite exhortation to implement [<- etc, the following seems to work:
#' j <- as.icd_short_diag(as.icd10(c("A11", "B2222")))
#' j[2] <- "C33"
#' stopifnot(is.icd_short_diag(j))
#' stopifnot(is.icd10(j), is.icd10(j[1]), is.icd10(j[[1]]))
#' j[[1]] <- "D44001"
#' stopifnot(is.icd_short_diag(j))
#' stopifnot(is.icd10(j), is.icd10(j[2]), is.icd10(j[[2]]))
#' @keywords internal
icd9 <- function(x) {
  # SOMEDAY: From Wickham: "When implementing a vector class, you should
  # implement these methods: length, [, [<-, [[, [[<-, c. (If [ is implemented
  # rev, head, and tail should all work)." But see examples, as this may not be
  # needed.
  cl <- class(x)
  if ("icd9" %in% cl) return(x)
  class(x) <- c("icd9", cl)
  x
}

#' @rdname set_icd_class
#' @export
as.icd9 <- function(x) {
  if (missing(x)) x <- character()
  icd_check_conflict_with_icd9(x)
  if (is.icd9(x)) return(x)
  after <- match("icd9cm", class(x), nomatch = 0L)
  class(x) <- append(class(x), "icd9", after = after)
  x
}

#' @rdname set_icd_class
#' @keywords internal
icd9cm <- function(x) {
  cl <- class(x)
  if ("icd9cm" %in% cl) return(x)
  class(x) <- c("icd9cm", cl)
  x
}

#' @rdname set_icd_class
#' @export
as.icd9cm <- function(x) {
  if (missing(x)) x <- character()
  icd_check_conflict_with_icd9cm(x)
  if (inherits(x, "icd9") && inherits(x, "icd9cm")) return(x)
  icd9_pos <- match("icd9", class(x))
  if (!is.na(icd9_pos))
    class(x) <- append(class(x), "icd9cm", after = icd9_pos - 1)
  else
    # put the more specific type at beginning
    class(x) <- append(class(x), c("icd9cm", "icd9"), after = 0)
  x
}

#' @rdname set_icd_class
#' @export
as.icd10 <- function(x) {
  if (missing(x)) x <- character()
  icd_check_conflict_with_icd10(x)
  if (inherits(x, "icd10")) return(x)
  icd10cm_pos <- match("icd10cm", class(x), nomatch = 0L)
  class(x) <- append(class(x), "icd10", after = icd10cm_pos)
  x
}

#' @rdname set_icd_class
#' @keywords internal
icd10 <- function(x) {
  cl <- class(x)
  if ("icd10" %in% cl) return(x)
  class(x) <- c("icd10", cl)
  x
}

#' @rdname set_icd_class
#' @export
as.icd10cm <- function(x, short_code = NULL) {
  if (missing(x)) x <- character()
  icd_check_conflict_with_icd10cm(x)
  if (inherits(x, "icd10cm")) return(x)
  icd10_pos <- match("icd10", class(x))
  if (!is.na(icd10_pos))
    class(x) <- append(class(x), "icd10cm", after = icd10_pos - 1)
  else
    class(x) <- append(class(x), c("icd10cm", "icd10"), after = 0)
  if (!is.null(short_code))
    attr(x, "icd_short_diag") <- short_code
  x
}

#' @rdname set_icd_class
#' @keywords internal
icd10cm <- function(x) {
  cl <- class(x)
  if ("icd10cm" %in% cl)
    return(x)
  if ("icd10" %in% cl)
    class(x) <- c("icd10cm", cl)
  else
    class(x) <- c("icd10cm", "icd10", cl)
  x
}

#' @rdname set_icd_class
#' @details long or wide format data is always a data frame. It does not
#'   carry any other ICD classes, even if it only contains one type of code.
#' @export
as.icd_long_data <- function(x) {
  # Also from Wickham: "When implementing a matrix/array class, you should
  # implement these methods: dim (gets you nrow and ncol), t, dimnames (gets you
  # rownames and colnames), dimnames<- (gets you colnames<-, rownames<-), cbind,
  # rbind."
  assert_data_frame(x)
  if (is.icd_wide_data(x))
    warning("Setting 'icd_long_data' on a data.frame or matrix which already has 'icd_wide_data' class")
  if (is.icd_long_data(x))
    return(x)
  class(x) <- c("icd_long_data", class(x))
  x
}

#' @rdname set_icd_class
#' @export
as.icd_wide_data <- function(x) {
  assert_data_frame(x)
  if (is.icd_long_data(x))
    warning("Setting 'icd_wide_data' on a data.frame or matrix which already has 'icd_long_data' class")
  if (is.icd_wide_data(x))
    return(x)
  class(x) <- c("icd_wide_data", class(x))
  x
}

#' @rdname set_icd_class
#' @param ... arguments passed on to create a \code{data.frame}
#' @description \code{icd_long_data} and \code{icd_short_data} create
#'   \code{data.frame}s using all the arguments, and sets the class, whereas
#'   \code{as.icd_long_data} and \code{as.icd_wide_data} just set the class of
#'   an existing \code{data.frame}.
#' @export
icd_long_data <- function(...) {
  as.icd_long_data(data.frame(...))
}

#' @rdname set_icd_class
#' @export
icd_wide_data <- function(...) {
  as.icd_wide_data(data.frame(...))
}

#' @rdname set_icd_class
#' @details Using \code{attributes} instead of \code{class} is a better fit for
#'   the data. It simplifies S3 dispatch, and appears to be very fast to get or
#'   set using the built-in R functions.
#' @rdname set_icd_class
#' @keywords internal
icd_comorbidity_map <- function(x) {
  assert_list(x, any.missing = FALSE, min.len = 1, names = "unique")
  cl <- class(x)
  if ("icd_comorbidity_map" %in% cl) return(x)
  class(x) <- c("icd_comorbidity_map", cl)
  x
}

#' @rdname set_icd_class
#' @export
as.icd_comorbidity_map <- function(x) {
  assert_list(x, any.missing = FALSE, min.len = 1, names = "unique")
  # avoid copying the data if class is already correct.
  if (inherits(x, "icd_comorbidity_map"))
    return(x)
  class(x) <- c("icd_comorbidity_map", class(x))
  x
}

#' Extract vector of codes from an ICD comorbidity map
#'
#' Equivalent to a list, but preserves class of extracted vector.
#' @param x comorbidity map, which is a named list
#' @param index integer
#' @template dotdotdot
#' @examples
#' # show that attributes are preserved when subsetting
#' stopifnot(is.icd_short_diag(icd10_map_ahrq[[1]]))
#' @export
`[[.icd_comorbidity_map` <- function(x, index, ...) {
  out <- NextMethod()
  # no need to reset attributes?
  out
}

#' combine ICD codes
#'
#' These function implement combination of lists or vectors of codes, while
#' preserving ICD classes. Base R \code{c} just drops all user defined classes
#' and casts down to lowest common denominator, e.g. if mixing numbers and
#' characters. No attempt here to catch all possible combinations of feeding in
#' mixed ICD types and other types. Let R do what it normally does, but just try
#' to keep classes of the first item in the list.
#' @param ... elements to combine
#' @param warn single logical value, if TRUE, will give warnings when
#'   incompatible types are combined using \code{c}
#' @examples
#' \dontrun{
#' # throw an error? or assign type according to first argument?
#' c(as.icd9("E998"), as.icd10("A10"))
#'
#' # benchmark subsetting to justify using .subset2 (5% faster)
#' library(microbenchmark)
#' j <- list(as.icd9cm("E990"), as.icd9cm("10010"))
#' k <- list(rep(as.icd9cm("E990"), times = 500))
#' microbenchmark(j[[1]], .subset2(j, 1),
#'                k[[1]], .subset2(k, 1),
#'                times = 1e6)
#'
#' # logical list to vector
#' a <- list(T,T)
#' microbenchmark(as.logical(a), c(a, recursive = TRUE), times = 1e6)
#'
#' # c(..., recursive = TRUE) vs unlist
#' l = list(c("100", "440", "999"), c("123", "234"))
#' microbenchmark::microbenchmark(c(l, recursive = TRUE),
#'                                c(unlist(l)),
#'                                times = 1e6)
#' stopifnot(identical(c(l, recursive = TRUE), c(unlist(l))))
#'
#' }
#' @name combine
#' @export
c.icd9 <- function(..., warn = FALSE) {
  dots <- list(...)

  if (warn && any(vapply(dots, icd_conflicts_with_icd9, FUN.VALUE = logical(1))))
    stop("Do you really want to combine ICD-9 codes (first argument) with ICD-10 codes (other arguments)?
         If so, unset the class of the arguments")
  structure(c(lapply(dots, unclass), recursive = TRUE), class = class(.subset2(dots, 1)))
  # SOMEDAY: would be nice to set the attribute, but by default, R's 'c' drops attributes.
}

#' @rdname combine
#' @export
c.icd10 <- function(..., warn = FALSE) {
  dots <- list(...)
  base_class <- class(.subset2(dots, 1))
  if (warn && any(vapply(dots, icd_conflicts_with_icd10, FUN.VALUE = logical(1))))
    stop("Do you really want to combine ICD-10 codes (first argument) with ICD-9 codes (subsequent arguments)?
         If so, use 'unclass' on some or all the arguments")
  out <- structure(c(unlist(dots)), class = base_class)
  # only set this attribute if all the consituent terms have the same attribute
  # present. One NULL or one conflict will mean the attribute is not set
  attribs <- lapply(dots, attr, which = "icd_short_diag")
  if (!any(vapply(attribs, is.null, logical(1)))) {
    n <- sum(as.logical(attribs))
    if (n == 0)
      attr(out, "icd_short_diag") <- FALSE
    else if (n == length(out))
      attr(out, "icd_short_diag") <- TRUE
  }
  out
}

#' extract subset from ICD data
#'
#' exactly the same as using x[n] or x[[n]] but preserves the ICD classes in
#' result
#' @param x input data with list, vector, factor, and class set to an ICD type.
#' @template dotdotdot
#' @examples
#' x <- as.icd9(list(my_codes = c("V10.1", "441.1")))
#' x[1]
#' x[[1]]
#' x[[1]][2]
#' # subsetting a list should give the underlying data structure type,
#' # preserving the ICD class
#' stopifnot(!inherits(x[[1]], "list"))
#' stopifnot(!inherits(x[[1]][2], "list"))
#'
#' y <- as.icd10(c("A01", "B0234"))
#' y[2]
#' y[[2]]
#' stopifnot(inherits(y[2], "icd10"))
#' stopifnot(inherits(y[[2]], "icd10"))
#' @name subset_icd
#' @export
`[.icd9` <- function(x, ...) {
  y <- NextMethod()
  attr(y, "icd_short_diag") <- attr(x, "icd_short_diag")
  class(y) <- class(x)
  y
}

#' @rdname subset_icd
#' @export
`[[.icd9` <- function(x, ...) {
  y <- NextMethod()
  attr(y, "icd_short_diag") <- attr(x, "icd_short_diag")
  if (mode(x) != "list")
    class(y) <- class(x)
  y
}

#' @rdname subset_icd
#' @export
`[.icd10` <- function(x, ...) {
  y <- NextMethod()
  attr(y, "icd_short_diag") <- attr(x, "icd_short_diag")
  class(y) <- class(x)
  y
}

#' @rdname subset_icd
#' @export
`[[.icd10` <- function(x, ...) {
  y <- NextMethod()
  attr(y, "icd_short_diag") <- attr(x, "icd_short_diag")
  if (mode(x) != "list")
    class(y) <- class(x)
  y
}

#' test ICD-related classes
#'
#' currently no checks on correctness of the classes for these functions
#'
#' @param x Any object which may have ICD-related classes set
#' @export
is.icd9 <- function(x) inherits(x, c("icd9", "icd9cm"))

#' @rdname is.icd9
#' @export
is.icd10 <- function(x) inherits(x, c("icd10", "icd10cm"))

#' @rdname is.icd9
#' @export
is.icd9cm <- function(x) inherits(x, "icd9cm")

#' @rdname is.icd9
#' @export
is.icd10cm <- function(x) inherits(x, "icd10cm")

#' @rdname is.icd9
#' @export
is.icd_long_data <- function(x) inherits(x, "icd_long_data")

#' @rdname is.icd9
#' @export
is.icd_wide_data <- function(x) inherits(x, "icd_wide_data")

#' @rdname is.icd9
#' @export
is.icd_comorbidity_map <- function(x) inherits(x, "icd_comorbidity_map")
