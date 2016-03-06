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
# icd10 icd10cm icd10who icd_long icd_wide icd_decimal icd_short
# icd_comorbidity_map
#
# I'm not sure of the best order, so I think I'll avoid assuming any order,
# except for more specific ICD types coming first.

# list currently implemented classes for validation
icd9_sub_classes <- c("icd9cm")
icd9_classes <- c(icd9_sub_classes, "icd9")
icd10_sub_classes <- c("icd10who", "icd10cm")
icd10_classes <- c(icd10_sub_classes, "icd10")
icd_version_classes <- c(icd9_classes, icd10_classes)
icd_data_classes <- c("icd_long_data", "icd_wide_data")
icd_code_classes <- c("icd_short_code", "icd_decimal_code")
icd_other_classes <- c("icd_comorbidity_map")
icd_all_classes <- c(icd_version_classes, icd_data_classes,
                     icd_code_classes, icd_other_classes)
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
icd_check_conflict_with_icd10who <- icd_check_conflict_with_icd10

#' check whether there are any ICD class conflicts
#'
#' E.g. both icd10who and icd10cm
#' @param x input object to test for class conflicts
#' @keywords internal
icd_classes_conflict <- function(x) {

  (is.icd9(x) && is.icd10(x)) ||
    sum(icd9_sub_classes %in% class(x)) > 1 ||
    sum(icd10_sub_classes %in% class(x)) > 1 ||
    sum(icd_data_classes %in% class(x)) > 1 ||
    sum(icd_code_classes %in% class(x)) > 1
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
                         icd9_classes,
                         icd10_classes,
                         icd_system_classes))
  out <- all(diff(m) >= 0, na.rm = TRUE)

  # can do some more specific tests with subsets:
  # short vs decimal should be after the ICD version, if it exists
  m <- match(class(x), c(icd9_classes,
                         icd10_classes,
                         icd_code_classes,
                         icd_system_classes))

  out && all(diff(m) >= 0, na.rm = TRUE)
}

#' @describeIn icd_classes_ordered stop if classes not well ordered
#' @keywords internal
icd_stop_classes_disorder <- function(x) {
  if (!icd_classes_ordered(x))
    stop("Classes in object are: ", paste(class(x), collapse = ", "), ",
         which is out of order.")
}

#' construct ICD-9 data types
#'
#' Takes an R structure and sets class to an ICD type. In the case
#'   of ICD-9 and ICD-10 codes, if a particular sub-type is set, e.g. ICD-9-CM
#'   (/code{icd9cm}), then an ICD-9 class (/code{icd9}) is also set.
#'
#' @param x object to set class \code{icd9}
#' @param warn single logical value, if \code{TRUE} will gives warning when
#'   converting between types. ICD-9 to ICD-10 will cause an error regardless.
#' @name set_icd_class
#' @export
icd9 <- function(x) {
  if (missing(x)) x <- character()
  icd_check_conflict_with_icd9(x)
  if (is.icd9(x)) return(x)
  after <- match("icd9cm", class(x), nomatch = 0)
  class(x) <- append(class(x), "icd9", after = after)
  x
}

#' @rdname set_icd_class
#' @export
icd9cm <- function(x) {
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
icd10 <- function(x) {
  if (missing(x)) x <- character()
  icd_check_conflict_with_icd10(x)
  if (inherits(x, "icd10")) return(x)
  icd10cm_pos <- match("icd10cm", class(x), nomatch = 0)
  icd10who_pos <- match("icd10who", class(x), nomatch = 0)
  after <- max(icd10cm_pos, icd10who_pos)
  class(x) <- append(class(x), "icd10", after = after)
  x
}

#' @rdname set_icd_class
#' @export
icd10cm <- function(x) {
  if (missing(x)) x <- character()
  icd_check_conflict_with_icd10cm(x)
  if (inherits(x, "icd10cm")) return(x)
  icd10_pos <- match("icd10", class(x))
  if (!is.na(icd10_pos))
    class(x) <- append(class(x), "icd10cm", after = icd10_pos - 1)
  else
    class(x) <- append(class(x), c("icd10cm", "icd10"), after = 0)
  x
}

#' @rdname set_icd_class
#' @export
icd10who <- function(x) {
  if (missing(x)) x <- character()
  icd_check_conflict_with_icd10who(x)
  if (inherits(x, "icd10who")) return(x)
  icd10_pos <- match("icd10", class(x))
  if (!is.na(icd10_pos))
    class(x) <- append(class(x), "icd10who", after = icd10_pos - 1)
  else
    class(x) <- append(class(x), c("icd10who", "icd10"), after = 0)
  x
}

#' Update the ICD version class of data.frame if there is an unambiguous version
#' column therein
#'
#' If only one ICD code type is defined in columns in a matrix or data.frame,
#' then update the class of the data frame to reflect that type.
#' @param x \code{data.frame} of patient data
#' @template icd_name
#' @param must_work single logical, if \code{TRUE} (the default) will stop if a
#'   single ICD version class cannot be applied
#' @keywords internal
update_data_frame_class <- function(x, icd_name = get_icd_name(x), must_work = TRUE) {

  assert_data_frame(x, min.cols = 2, col.names = "unique")
  assert_string(icd_name)
  assert_flag(must_work)

  i9 <- any(vapply(x, inherits, "icd9", FUN.VALUE = logical(1)))
  i9cm <- any(vapply(x, inherits, "icd9cm", FUN.VALUE = logical(1)))
  i10 <- any(vapply(x, inherits, "icd10", FUN.VALUE = logical(1)))
  i10cm <- any(vapply(x, inherits, "icd10cm", FUN.VALUE = logical(1)))
  i10who <- any(vapply(x, inherits, "icd10who", FUN.VALUE = logical(1)))

  # now if none of the columns inherit from an ICD version class, then we should guess ourselves:
  if (!i9 && !i9cm && !i10 && !i10cm && !i10who) {

    icd_ver <- icd_guess_version(x[[icd_name]])
    if (icd_ver == "icd9")
      return(icd9(x))
    else if (icd_ver == "icd10")
      return(icd10(x))
    else
      if (must_work)
        stop("no columns with ICD version class found in input data frame, and",
             " unable to guess ICD version from the data in column '", icd_name,
             "' of the input data frame", call. = FALSE)
  }

  if (sum(i9 || i9cm, i10 || i10cm, i10 || i10who) == 1) {
    if (i9 && !is.icd9(x))
      return(icd9(x))
    if (i9cm && !is.icd9cm(x))
      return(icd9cm(x))
    if (i10 && !is.icd10(x))
      return(icd10(x))
    if (i10cm && !is.icd10cm(x))
      return(icd10cm(x))
    if (i10who && !is.icd10who(x))
      return(icd10who(x))
  }

  if (must_work)
    stop("found columns of differing ICD version classes, so unable to ",
         " assign a single class to the whole data frame.")
  x
}

#' @rdname set_icd_class
#' @export
icd_long_data <- function(x) {
  if (!is.data.frame(x) && !is.matrix(x))
    stop("Long or Wide structure only makes sense in a matrix or data frame")
  if (inherits(x, "icd_long_data"))
    return(x)
  class(x) <- append(class(x), "icd_long_data", 0)
  x
}

#' @rdname set_icd_class
#' @export
icd_wide_data <- function(x) {
  if (!is.data.frame(x) && !is.matrix(x))
    stop("Long or Wide structure only makes sense in a matrix or data frame")
  if (inherits(x, "icd_wide_data"))
    return(x)
  class(x) <- append(class(x), "icd_wide_data", 0)
  x
}

#' @rdname set_icd_class
#' @export
icd_short_code <- function(x, warn = TRUE) {
  assert_flag(warn)
  if (missing(x)) x <- character()
  # TODO consider warning if there are decimals!
  if (inherits(x, "icd_short_code")) return(x)
  if (inherits(x, "icd_decimal_code")) {
    if (warn) warning("setting class to describe short format ICD codes, but decimal is currently set")
    class(x) <- class(x)[class(x) %nin% "icd_decimal_code"]
  }

  class(x) <- append(class(x), "icd_short_code",
                     after = get_pos_short_decimal_class(x))
  x
}

#' get position to set short or decimal class
#'
#' prefer immediately after \code{icd9cm}, etc., if not, place before system
#' classes at end, or at the very end if no system classes
#' @keywords internal
get_pos_short_decimal_class <- function(x) {
  pos_last_icd_type <- which(class(x) %in% c(icd9_classes, icd10_classes))
  pos_system_type <- which(class(x) %in% icd_system_classes)
  if (length(pos_last_icd_type) > 0)
    max(pos_last_icd_type)
  else if (length(pos_system_type) > 0)
    max(pos_system_type) - 1
  else
    length(class(x))
}

#' @rdname set_icd_class
#' @export
icd_decimal_code <- function(x, warn = TRUE) {
  assert_flag(warn)
  if (missing(x)) x <- character()
  # TODO consider warning if there are decimals!
  if (inherits(x, "icd_decimal_code"))
    return(x)
  if (inherits(x, "icd_short_code")) {
    if (warn) warning("setting class to describe decimal format ICD codes, but short is currently set")
    class(x) <- class(x)[class(x) %nin% "icd_short_code"]
  }
  class(x) <- append(class(x), "icd_decimal_code",
                     after = get_pos_short_decimal_class(x))
  x
}

#' @rdname set_icd_class
#' @details This, I think, should take priority over ICD-9 vs ICD-10 when doing
#'   S3 dispatching
#' @export
icd_comorbidity_map <- function(x) {
  assert_list(x, any.missing = FALSE, min.len = 1, names = "unique")
  if (inherits(x, "icd_comorbidity_map"))
    return(x)
  class(x) <- append(class(x), "icd_comorbidity_map", after = 0)
  x
}

#' extract vector of codes from an ICD comorbidity map
#'
#' Equivalent to a list, but preserves class of extracted vector
#' @param x comorbidity map to be sub-setted. This is a named list.
#' @param index integer
#' @template dotdotdot
#' @export
`[[.icd_comorbidity_map` <- function(x, index, ...) {
  x_cl <- class(x)

  enforce_child_classes <- x_cl[x_cl %in% c(icd_code_classes,
                                            icd_version_classes,
                                            icd_data_classes)]

  out <- NextMethod()
  missing_classes <- setdiff(enforce_child_classes, class(out))
  class(out) <- append(class(out), missing_classes, 0)
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
#' @examples
#' \dontrun{
#' # throw an error
#' c(icd9("E998"), icd10("A10"))
#' }
#' @name combine
#' @export
c.icd9 <- function(...) {
  args <- list(...)
  base_class <- class(args[[1]])
  if (any(vapply(args, icd_conflicts_with_icd9, FUN.VALUE = logical(1))))
    stop("Do you really want to combine ICD-9 codes (first argument) with ICD-10 codes (other arguments)?
         If so, unset the class of the arguments")
  structure(c(unlist(lapply(list(...), unclass))), class = base_class)
}

#' @rdname combine
#' @export
c.icd9cm <- function(...) {
  args <- list(...)
  base_class <- class(args[[1]])
  if (any(vapply(args, icd_conflicts_with_icd9cm, FUN.VALUE = logical(1))))
    stop("Do you really want to combine ICD-9 codes (first argument) with ICD-10 codes (other arguments)?
         If so, unset the class of the arguments")
  structure(c(unlist(lapply(list(...), unclass))), class = base_class)
}

#' @rdname combine
#' @export
c.icd10 <- function(...) {
  args <- list(...)
  base_class <- class(args[[1]])
  if (any(vapply(args, icd_conflicts_with_icd10, FUN.VALUE = logical(1))))
    stop("Do you really want to combine ICD-10 codes (first argument) with ICD-9 codes (subsequent arguments)?
         If so, use unclass on some or all the arguments")
  structure(c(unlist(lapply(args, unclass))), class = base_class)
}

#' @rdname combine
#' @export
c.icd10cm <- function(...) {
  args <- list(...)
  base_class <- class(args[[1]])
  if (any(vapply(args, icd_conflicts_with_icd10, FUN.VALUE = logical(1))))
    stop("Do you really want to combine ICD-10 codes (first argument) with ICD-9 codes (subsequent arguments)?
         If so, use unclass on some or all the arguments")
  structure(c(unlist(lapply(args, unclass))), class = base_class)
}

#' @rdname combine
#' @export
c.icd10who <- function(...) {
  args <- list(...)
  base_class <- class(args[[1]])
  if (any(vapply(args, icd_conflicts_with_icd10, FUN.VALUE = logical(1))))
    stop("Do you really want to combine ICD-10 codes (first argument) with ICD-9 codes (subsequent arguments)?
         If so, use unclass on some or all the arguments")
  structure(c(unlist(lapply(args, unclass))), class = base_class)
}

#' @rdname combine
#' @export
#' @keywords internal
c.icd_short_code <- function(...) {
  args <- list(...)
  base_class <- class(args[[1]])
  if (any(vapply(args, is.icd_decimal_code, FUN.VALUE = logical(1))))
    stop("Do you really want to combine short and decimal format ICD codes?
         If so, use unclass on some or all the arguments")
  if (!all(vapply(args, is.icd_short_code, FUN.VALUE = logical(1))))
    warning("The first argument is in short format, whereas subsequent arguments
            include codes with no short or decimal designation.")
  structure(c(unlist(lapply(args, unclass))), class = base_class)
}

#' @rdname combine
#' @export
#' @keywords internal
c.icd_decimal_code <- function(...) {
  args <- list(...)
  base_class <- class(args[[1]])
  if (any(vapply(args, is.icd_short_code, FUN.VALUE = logical(1))))
    stop("Do you really want to combine short and decimal format ICD codes?
         If so, use unclass on some or all the arguments")
  if (!all(vapply(args, is.icd_decimal_code, FUN.VALUE = logical(1))))
    warning("The first argument is in short format, whereas subsequent arguments
            include codes with no short or decimal designation.")
  structure(c(unlist(lapply(args, unclass))), class = base_class)
}

#' extract subset from ICD data
#'
#' exactly the same as using x[n] or x[[n]] but preserves the ICD classes in
#' result
#' @param x input data with list, vector, factor, and class set to an ICD type.
#' @template dotdotdot
#' @examples
#' x <- icd9(list(my_codes = c("V10.1", "441.1")))
#' x[1]
#' x[[1]]
#' x[[1]][2]
#' # subsetting a list should give the underlying data structure type,
#' # preserving the ICD class
#' stopifnot(!inherits(x[[1]], "list"))
#' stopifnot(!inherits(x[[1]][2], "list"))
#'
#' y <- icd10(c("A01", "B0234"))
#' y[2]
#' y[[2]]
#' stopifnot(inherits(y[2], "icd10"))
#' stopifnot(inherits(y[[2]], "icd10"))
#' @name subset_icd
#' @export
`[.icd9` <- function(x, ...) {
  y <- NextMethod()
  if (!is.data.frame(x))
    class(y) <- class(x)
  y
}

#' @rdname subset_icd
#' @export
`[[.icd9` <- function(x, ...) {
  y <- NextMethod()
  if (mode(x) != "list")
    class(y) <- class(x)
  y
}

#' @rdname subset_icd
#' @export
`[.icd10` <- function(x, ...) {
  y <- NextMethod()
  if (!is.data.frame(x))
    class(y) <- class(x)
  y
}

#' @rdname subset_icd
#' @export
`[[.icd10` <- function(x, ...) {
  y <- NextMethod()
  if (mode(x) != "list")
    class(y) <- class(x)
  y
}

#' test ICD-related classes
#'
#' currently no checks on correctness of the classes for these functions
#'
#' @details TODO: could warn or fix if something is \code{icd10cm} or
#'   \code{icd10who} but not \code{icd10}
#' @param x Any object which may have ICD-related classes set
#' @param strict logical value, if \code{TRUE}, will only match the type
#'   exactly; if \code{FALSE}
#' @export
is.icd9 <- function(x, strict = FALSE)
  if (strict) {
    inherits(x, "icd9")
  } else {
    inherits(x, c("icd9", "icd9cm"))
  }

#' @rdname is.icd9
#' @details TODO: could warn or fix if something is \code{icd10cm} or
#'   \code{icd10who} but not \code{icd10}
#' @export
is.icd10 <- function(x, strict = FALSE)
  if (strict) {
    inherits(x, "icd10")
  } else {
    inherits(x, c("icd10", "icd10cm", "icd10who"))
  }

#' @rdname is.icd9
#' @export
is.icd9cm <- function(x) inherits(x, "icd9cm")

#' @rdname is.icd9
#' @export
is.icd10cm <- function(x) inherits(x, "icd10cm")

#' @rdname is.icd9
#' @export
is.icd10who <- function(x) inherits(x, "icd10who")

#' @rdname is.icd9
#' @export
is.icd_long_data <- function(x) inherits(x, "icd_long_data")

#' @rdname is.icd9
#' @export
is.icd_wide_data <- function(x) inherits(x, "icd_wide_data")

#' @rdname is.icd9
#' @export
is.icd_short_code <- function(x) inherits(x, "icd_short_code")

#' @rdname is.icd9
#' @export
is.icd_decimal_code <- function(x) inherits(x, "icd_decimal_code")

#' @rdname is.icd9
#' @export
is.icd_comorbidity_map <- function(x) inherits(x, "icd_comorbidity_map")

# TODO, print S3 methods so we can choose to show ICD version, and/or human
# readable descriptions of the codes
