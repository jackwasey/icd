# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

######################################################################
#
# get and set class types this package uses The master list is: icd9 icd9cm icd10
# icd10cm icd10who icd_long icd_wide icd_decimal icd_short icd_map
#
# I'm not sure of the best order, so I think I'll avoid assuming any order,
# except for more specific ICD types coming first.
######################################################################

# list currently implemented classes for validation
icd9_sub_classes <- c("icd9cm")
icd9_classes <- c(icd9_sub_classes, "icd9")
icd10_sub_classes <- c("icd10who", "icd10cm")
icd10_classes <- c(icd10_sub_classes, "icd10")
icd_data_classes <- c("icd_long_data", "icd_wide_data")
icd_code_classes <- c("icd_short_code", "icd_decimal_code")
icd_other_classes <- c("map") # maybe calls this comorbidity_map?

icd_check_conflict_with_icd9 <- function(x)
  if (inherits(x, icd9_classes))
    stop("Trying to set ICD-10 class on an object which already has an ICD-9 class")

icd_check_conflict_with_icd10 <- function(x)
  if (inherits(x, icd10_classes))
    stop("Trying to set ICD-10 class on an object which already has an ICD-9 class")

#' @title prefer an order of classes
#' @description The order of classes can matter because, for some functions,
#'   we'd prefer to decide what to do based on whether the structure is a
#'   comorbidity map before caring if it is icd-9 or 10. I can't see how it
#'   matters whether we prioritize long/wide and short/decimal yet, so won't
#'   test.
#' @param x any object which may or may not have classes from this package
#' @keywords internal
icd_check_class_order <- function(x) {

  system_classes <- c("data.frame", "list", "numeric", "character", "factor")

  m <- match(class(x), c(icd_other_classes, icd9_classes, icd10_classes, system_classes))
  if (any(m != cumsum(m)))
    stop("preferred class order not met.")
}

icd_check_class_conflict <- function(x) {
  if (sum(icd9_sub_classes %in% class(x)) > 1)
    stop("Conflicting ICD-9 sub-classes")
  if (sum(icd10_sub_classes %in% class(x)) > 1)
    stop("Conflicting ICD-10 sub-classes")
  if (sum(icd_data_classes %in% class(x)) > 1)
    stop("Conflicting long/wide data classes exist")
  if (sum(icd_code_classes %in% class(x)) > 1)
    stop("Conflicting short/decimal classes exist")
}

#' @rdname set_icd_class
#' @title construct ICD-9 data types
#' @description Takes an R structure and sets class to an ICD type. In the case of ICD-9 and
#' ICD-10 codes, if a particular sub-type is set, e.g. ICD-9-CM (/code{icd9cm}), then an ICD-9
#' class (/code{icd9}) is also set.
#'
#' @export
icd9 <- function(x) {
  icd_check_conflict_with_icd10(x)
  if (inherits(x, "icd9")) return(x)
  after = match("icd9cm", class(x), nomatch = 0)
  class(x) <- append(class(x), "icd9", after = after)
  x
}

#' @rdname set_icd_class
#' @export
icd9cm <- function(x) {
  icd_check_conflict_with_icd10(x)
  if (inherits(x, "icd9") && inherits(x, "icd9cm")) return(x)
  icd9_pos = match("icd9", class(x))
  if (!is.na(icd9_pos))
    class(x) <- append(class(x), "icd9cm", after = icd9_pos - 1)
  else
    # put the more specific type at beginning
    class(x) <- append(class(x), "icd9cm", after = 0)
  x
}

#' @rdname set_icd_class
#' @export
icd10 <- function(x) {
  icd_check_conflict_with_icd9(x)
  if (inherits(x, "icd10")) return(x)
  icd10cm_pos = match("icd10cm", class(x), nomatch = 0)
  icd10who_pos = match("icd10who", class(x), nomatch = 0)
  after = max(icd10cm_pos, icd10who_pos)
  class(x) <- append(class(x), "icd10", after = after)
  x
}

#' @rdname set_icd_class
#' @export
icd10cm <- function(x) {
  icd_check_conflict_with_icd9(x)
  if (inherits(x, "icd10cm")) return(x)
  icd10_pos = match("icd10", class(x))
  if (!is.na(icd10_pos))
    class(x) <- append(class(x), "icd10cm", after = icd10_pos - 1)
  else
    class(x) <- append(class(x), "icd10cm", after = 0)
  x
}

#' @rdname set_icd_class
#' @export
icd10who <- function(x) {
  icd_check_conflict_with_icd9(x)
  if (inherits(x, "icd10who")) return(x)
  icd10_pos = match("icd10", class(x))
  if (!is.na(icd10_pos))
    class(x) <- append(class(x), "icd10who", after = icd10_pos - 1)
  else
    class(x) <- append(class(x), "icd10who", after = 0)
  x
}

#' If only one ICD code type is defined in columns in a matrix or data.frame,
#' then update the class of the data frame to relfect that type.
#' @keywords internal
update_class_from_columns <- function(x) {
  i9 <- any(vapply(x, inherits, "icd9", FUN.VALUE = logical(1)))
  i9cm <- any(vapply(x, inherits, "icd9cm", FUN.VALUE = logical(1)))
  i10 <- any(vapply(x, inherits, "icd10", FUN.VALUE = logical(1)))
  i10cm <- any(vapply(x, inherits, "icd10cm", FUN.VALUE = logical(1)))
  i10who <- any(vapply(x, inherits, "icd10who", FUN.VALUE = logical(1)))
  if (sum(i9 || i9cm, i10 || i10cm, i10 || i10who) == 1) {
    if (i9 && "icd9" %nin% class(x))
      class(x) <- append(class(x), "icd9", 0)
    if (i9cm && "icdcm" %nin% class(x))
      class(x) <- append(class(x), c("icd9cm", "icd9"), 0)
    if (i10 && "icd10" %nin% class(x))
      class(x) <- append(class(x), "icd10", 0)
    if (i10cm && "icd10cm" %nin% class(x))
      class(x) <- append(class(x), c("icd10cm", "icd10"), 0)
    if (i10who && "icd10who" %nin% class(x))
      class(x) <- append(class(x), c("icd10who", "icd10"), 0)
  }
  x
}

#' @rdname set_icd_class
#' @export
icd_long_data <- function(x) {
  if (!is.data.frame(x) && !is.matrix(x))
    stop("Long or Wide structure only makes sense in a matrix or data frame")
  # if columns are exclusively either ICD-9 or ICD-10 then this should be type
  # of the returned structure, too.
  x <- update_class_from_columns(x)

  if (inherits(x, "icd_long_data")) return(x)
  class(x) <- append(class(x), "icd_long_data", 0)
  x
}

#' @rdname set_icd_class
#' @export
icd_wide_data <- function(x) {
  if (!is.data.frame(x) && !is.matrix(x))
    stop("Long or Wide structure only makes sense in a matrix or data frame")
  x <- update_class_from_columns(x)
  if (inherits(x, "icd_wide_data"))
    return(x)
  class(x) <- append(class(x), "icd_wide_data", 0)
  x
}

#' @rdname set_icd_class
#' @export
icd_short_code <- function(x) {
  # TODO consider warning if there are decimals!
  if (inherits(x, "icd_short_code")) return(x)
  if (inherits(x, "icd_decimal_code")) {
    warning("setting class to describe short format ICD codes, but decimal is currently set")
    class(x) <- class(x)[class(x) %nin% "icd_decimal_code"]
  }
  class(x) <- append(class(x), "icd_short_code")
  x
}

#' @rdname set_icd_class
#' @export
icd_decimal_code <- function(x) {
  # TODO consider warning if there are decimals!
  if (inherits(x, "icd_decimal_code")) return(x)
  if (inherits(x, "icd_short_code")) {
    warning("setting class to describe decimal format ICD codes, but short is currently set")
    class(x) <- class(x)[class(x) %nin% "icd_short_code"]
  }
  class(x) <- append(class(x), "icd_decimal_code")
  x
}

#' @rdname set_icd_class
#' @details This, I think, should take priority over ICD-9 vs ICD-10 when doing S3 dispatching
#' Maybe should call this class icd_comorbidity_map instead
#' @export
icd_map <- function(x) {
  checkmate::assertList(x, any.missing = FALSE, min.len = 1, names = "unique")
  if (inherits(x, "icd_map")) return(x)
  class(x) <- append(class(x), "icd_map", after = 0)
  x
}

# for comorbidity maps, if we subset, we want the result to have icd classes:


#' extract elements of an ICD comorbidity map
#'
#' Equivalent to a list, but preserves class of extracted elements
#' @export
`[.icd_map` <- function(x, index, ...) {
  new_classes <- class(x)
  new_classes <- new_classes[new_classes != "icd_map"]
  y <- unclass(x)
  out <- y[index, ...]
  class(out) <- new_classes
  out
}

#' extract vector of codes from an ICD comorbidity map
#'
#' Equivalent to a list, but preserves class of extracted vector
#' @export
`[[.icd_map` <- function(x, index, ...) {
  new_classes <- class(x)
  new_classes <- new_classes[new_classes != "icd_map"]
  y <- unclass(x)
  out <- y[[index, ...]]
  class(out) <- new_classes
  out
}

#' @title combine ICD codes
#' @name combine
#' @description These function implement combination of lists or vectors of
#'   codes, while preserving ICD classes.
#' @export
c.icd9 <- function(...) {
  args <- list(...)
  base_class <- class(args[[1]])
  if (any(is.icd10(unlist(args))))
    stop("Do you really want to combine ICD-9 codes (first argument) with ICD-10 codes (other arguments)? If so, unset the class of the arguments")
  #if (!all(is.icd9(unlist(args)))) # this breaks icd9_generate_map
  # warning("Combine ICD-9 codes with codes of unknown type")
  structure(c(unlist(lapply(list(...), unclass))), class = base_class)
}

#' @rdname combine
#' @export
c.icd9cm <- function(...) {
  args <- unlist(list(...))
  if (any(!inherits(args, "icd9cm")))
    warning("The first codes given are ICD-9-CM class, but subsequent ones are not.")
}

#' @rdname combine
#' @export
c.icd10 <- function(...) {
  args <- list(...)
  base_class <- class(args[[1]])
  if (any(is.icd9(unlist(args))))
    stop("Do you really want to combine ICD-10 codes (first argument) with ICD-9 codes (subsequent arguments)? If so, use unclass on some or all the arguments")
  if (!all(is.icd10(unlist(args))))
    warning("Combining ICD-9 codes with codes of unknown type")
  structure(c(unlist(lapply(args, unclass))), class = base_class)
}

#' @rdname combine
#' @export
c.icd10cm <- function(...) {
  args <- unlist(list(...))
  if (any(is.icd10who(args)))
    warning("The first argument is ICD-10-CM, whereas subsequent arguments include ICD-10 WHO codes.")
  NextMethod()
}

#' @rdname combine
#' @export
c.icd10who <- function(...) {
  args <- unlist(list(...))
  if (any(is.icd10cm(args)))
    warning("The first argument is ICD-10 WHO, whereas subsequent arguments include ICD-10-CM codes.")
  NextMethod()
}

#' @name subset_icd
#' @title extract subset from ICD data
#' @description exactly the same as using x[n] or x[[n]] but preserves the ICD
#'   classes in result
#' @details TODO:Potential here to use attributes, since we can (as base R does
#'   in \code{datediff}, \code{POSIXct}, etc.) recreate the attributes after a
#'   subsetting operation. This would simplify the class system.
#' @export
`[.icd9` <- function(x, ...) {

  # unfortunately, I need to switch on type here, which somewhat defeats the
  # purpose of S3, but I can't get NextMethod to do what I want without infinite
  # recursion in indexing data.frames.
   if (is.data.frame(x)) {
    y <- `[.data.frame`(x, ...)
    class(y) <- append(class(y), "icd9", 0)
    return(y)
   }

  cl <- class(x)
  class(x) <- cl[cl != "icd9"]
  x <- NextMethod()
  if (inherits(x, "data.frame"))
    class(x) <- cl
  else
    class(x) <- cl[cl %nin% c("data.frame", icd_data_classes)]
  x
}

#' @rdname subset_icd
#' @export
`[[.icd9` <- function(x, ...) {

  if (is.data.frame(x)) {
    y <- `[[.data.frame`(x, ...)
    class(y) <- append(class(y), "icd9", 0)
    return(y)
  }

  cl <- class(x)
  class(x) <- cl[cl != "icd9"]
  x <- NextMethod()  #NextMethod("[")
  if (inherits(x, "data.frame"))
    class(x) <- cl
  else
    class(x) <- cl[cl %nin% c("data.frame", icd_data_classes)]
  x
}

#' @rdname subset_icd
#' @export
`[.icd10` <- function(x, ..., drop = TRUE) {
  cl <- class(x)
  class(x) <- cl[cl != "icd9"]
  out <- NextMethod("[")
  class(out) <- cl
  out
}

#' @rdname subset_icd
#' @export
`[[.icd10` <- function(x, ..., exact = TRUE) {
  cl <- class(x)
  class(x) <- cl[cl != "icd9"]
  if (inherits(x, "data.frame")) {
    # [[.data.frame never returns a data frame itself, and seems to preserve the
    # underlying class
    y <- `[[.data.frame`(x, ...)
    return(y)
  }
  #out <- NextMethod("[[")
  x <- NextMethod(object = x)
  class(x) <- cl
  x
}

#' test ICD-related classes
#'
#' currently no checks on correctness of the classes for these functions
#' @details TODO: could warn or fix if something is icd10cm or icd10who but not icd10
#' @param x Any object which may have ICD-related classes set
#' @param strict logical value, if TRUE, will only match the type exactly; if \code{FALSE}
#' @export
is.icd9 <- function(x, strict = FALSE)
  if (strict) {
    inherits(x, "icd9")
  } else {
    inherits(x, c("icd9", "icd9cm"))
  }

#' @rdname is.icd9
#' @details TODO: could warn or fix if something is icd10cm or icd10who but not icd10
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
is.icd_map <- function(x) inherits(x, "icd_map")

# TODO, print S3 methods so we can choose to show ICD version, and/or human
# readable descriptions of the codes
