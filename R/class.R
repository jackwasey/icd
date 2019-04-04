# ICD-9
icd9_dx_sub_classes <- c(
  "icd9cm",
  "icd9who"
)
icd9_pc_sub_classes <- "icd9cm_pc"
icd9_sub_classes <- c(
  icd9_dx_sub_classes,
  icd9_pc_sub_classes
)
icd9_classes <- c(icd9_sub_classes, "icd9")
# ICD-10
icd10_dx_sub_classes <- c(
  "icd10cm",
  "icd10who"
)
icd10_pc_sub_classes <- "icd10cm_pc"
icd10_sub_classes <- c(
  icd10_dx_sub_classes,
  icd10_pc_sub_classes
)
icd10_classes <- c(icd10_sub_classes, "icd10")
# ICD-9 and ICD-10
icd_dx_not_generic <- c(
  icd9_dx_sub_classes,
  icd10_dx_sub_classes
)
icd_pc_not_generic <- c(
  icd9_pc_sub_classes,
  icd10_pc_sub_classes
)
icd_version_classes <- c(
  icd9_classes,
  icd10_classes
)
# Other data types
icd_data_classes <- c(
  "icd_long_data",
  "icd_wide_data"
)
icd_other_classes <- c("comorbidity_map")
icd_all_classes <- c(
  icd_version_classes,
  icd_data_classes,
  icd_other_classes
)
icd_system_classes <- c(
  "data.frame",
  "list",
  "numeric",
  "character",
  "factor"
)

#' Check for class conflict
#' @param x Data to test
#' @param do_stop logical, if \code{TRUE}, execution will stop with an error
#' @keywords internal
#' @noRd
icd_conflicts <- function(x, do_stop = FALSE)
  UseMethod("icd_conflicts")

stop_conflict <- function(x)
  stop("Cannot set requested class as the current data already has the ",
    "following incompatible classes: ", paste(class(x), sep = ", "),
    ". If you really wish to do this, use unclass, then set the desired ",
    "icd class",
    call. = FALSE
  )

#' @export
icd_conflicts.icd9 <- function(x, do_stop = FALSE) {
  res <- inherits(x, icd10_classes)
  if (res && do_stop) stop_conflict(x)
  res
}

#' @export
icd_conflicts.icd9cm <- function(x, do_stop = FALSE) {
  res <- icd_conflicts.icd9(x, do_stop) ||
    inherits(x, "icd9who") ||
    inherits(x, "icd9cm_pc")
  if (res && do_stop) stop_conflict(x)
  res
}

#' @export
icd_conflicts.icd9cm_pc <- function(x, do_stop = FALSE) {
  res <- icd_conflicts.icd9(x, do_stop) ||
    inherits(x, "icd9who") ||
    inherits(x, "icd9cm") ||
    inherits(x, "icd9")
  if (res && do_stop) stop_conflict(x)
  res
}

#' @export
icd_conflicts.icd9who <- function(x, do_stop = FALSE) {
  res <- icd_conflicts.icd10(x) ||
    inherits(x, "icd9cm") ||
    inherits(x, "icd9cm_pc")
  if (res && do_stop) stop_conflict(x)
  res
}

#' @export
icd_conflicts.icd10 <- function(x, do_stop = FALSE) {
  res <- inherits(x, icd9_classes)
  if (res && do_stop) stop_conflict(x)
  res
}

#' @export
icd_conflicts.icd10cm <- function(x, do_stop = FALSE) {
  res <- icd_conflicts.icd10(x, do_stop) ||
    inherits(x, "icd10who") ||
    inherits(x, "icd10cm_pc")
  if (res && do_stop) stop_conflict(x)
  res
}

#' @export
icd_conflicts.icd10cm_pc <- function(x, do_stop = FALSE) {
  res <- icd_conflicts.icd10(x, do_stop) ||
    inherits(x, "icd10who") ||
    inherits(x, "icd10cm") ||
    inherits(x, "icd10")
  if (res && do_stop) stop_conflict(x)
  res
}

#' @export
icd_conflicts.icd10who <- function(x, do_stop = FALSE) {
  res <- icd_conflicts.icd10(x) ||
    inherits(x, "icd10cm") ||
    inherits(x, "icd10cm_pc") ||
    inherits(x, "icd10fr")
  if (res && do_stop) stop_conflict(x)
  res
}

#' @export
icd_conflicts.icd10fr <- function(x, do_stop = FALSE) {
  res <- icd_conflicts.icd10(x) ||
    inherits(x, "icd10cm") ||
    inherits(x, "icd10cm_pc") ||
    inherits(x, "icd10who")
  if (res && do_stop) stop_conflict(x)
  res
}

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
#' @keywords internal
#' @noRd
icd_classes_conflict <- function(x) {
  is.icd9(x) && is.icd10(x) ||
    is.icd9cm(x) && is.icd9cm_pc(x) ||
    is.icd10cm(x) && is.icd10cm_pc(x) ||
    is.icd_long_data(x) && is.icd_wide_data(x)
}

#' Prefer an order of classes
#'
#' The order of classes can matter because, for some functions, we would prefer
#' to decide what to do based on a higher level structure, e.g., whether the
#' structure is a comorbidity map before caring if it is ICD-9 or ICD-10.
#' @param x any object which may or may not have classes from this package
#' @keywords internal
#' @noRd
classes_ordered <- function(x) {
  m <- match(class(x), c(
    icd_other_classes,
    icd_version_classes,
    icd_data_classes,
    icd_system_classes
  ))
  all(diff(m) >= 0, na.rm = TRUE)
}

#' Construct ICD-9 and ICD-10 data types
#'
#' Takes an R structure and sets class to an ICD type. In the case of ICD-9 and
#' ICD-10 codes, if a particular sub-type is set, e.g. ICD-9-CM (\code{icd9cm}),
#' then an ICD-9 class (\code{icd9}) is also set.
#'
#' The \code{as.*} functions e.g. \code{as.icd9}, do checking and try to put
#' multiple classes in a nice order. Calling the internal bare constructor, e.g.
#' \code{icd:::icd9} just prepends the new class and returns without any checks,
#' which is much faster.
#'
#' Some features make more sense as attributes. E.g. setting code type to
#' \code{short} or \code{decimal}.
#'
#' @param x object to set class \code{icd9}
#' @template short_code
#' @name set_icd_class
#' @family ICD data types
#' @examples
#' x <- as.icd10("A1009")
#' attr(x, "icd_short_diag") <- TRUE
#' x
#' attributes(x) <- list(icd_short_diag = NULL)
#' x
#' 
#' y <- as.decimal_diag(as.icd10("A10.09"))
#' y
#' is.short_diag(y)
#' 
#' j <- as.short_diag(as.icd10(c("A11", "B2222")))
#' j[2] <- "C33"
#' stopifnot(is.short_diag(j))
#' stopifnot(is.icd10(j), is.icd10(j[1]), is.icd10(j[[1]]))
#' j[[1]] <- "D44001"
#' stopifnot(is.short_diag(j))
#' stopifnot(is.icd10(j), is.icd10(j[2]), is.icd10(j[[2]]))
NULL

#' @noRd
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

#' @describeIn set_icd_class Use generic ICD-9 class for this data. Ideally, use
#'   the more specific \code{icd9cm} or other sub-classes (when available).
#' @export
as.icd9 <- function(x) {
  stopifnot(is.atomic(x))
  if (is.icd9(x)) return(x)
  after <- match("icd9cm", class(x), nomatch = 0L)
  class(x) <- append(class(x), "icd9", after = after)
  # set the class then allow dispatch to confirm okay
  icd_conflicts(x, do_stop = TRUE)
  x
}

#' @noRd
#' @keywords internal
icd9cm <- function(x) {
  cl <- class(x)
  if ("icd9cm" %in% cl) return(x)
  class(x) <- c("icd9cm", cl)
  x
}

#' @describeIn set_icd_class Use ICD-9-CM
#' @export
as.icd9cm <- function(x) {
  stopifnot(is.atomic(x))
  if (inherits(x, "icd9") && inherits(x, "icd9cm")) return(x)
  icd9_pos <- match("icd9", class(x))
  if (!is.na(icd9_pos)) {
    class(x) <- append(class(x), "icd9cm", after = icd9_pos - 1)
  } else {
    # put the more specific type at beginning
    class(x) <- append(class(x), c("icd9cm", "icd9"), after = 0)
  }
  icd_conflicts(x, do_stop = TRUE)
  x
}

#' @noRd
#' @keywords internal
icd9cm_pc <- function(x) {
  cl <- class(x)
  if ("icd9cm_pc" %in% cl) return(x)
  class(x) <- c("icd9cm_pc", cl)
  x
}

#' @describeIn set_icd_class Indicate the data are ICD-9-CM procedure codes.
#' @export
as.icd9cm_pc <- function(x) {
  stopifnot(is.atomic(x))
  if ("icd10" %in% class(x)) {
    stop("An ICD-10 diagnostic class is already set on this variable")
  }
  if ("icd10cm_pc" %in% class(x)) {
    stop("An ICD-10 procedure class is already set on this variable")
  }
  if (any(c("icd9", "icd9cm", "icd9who") %in% class(x))) {
    stop("An ICD-9 diagnostic code class is already set")
  }
  class(x) <- append(class(x), "icd9cm_pc", after = 0)
  icd_conflicts(x, do_stop = TRUE)
  x
}

#' @noRd
#' @keywords internal
icd10 <- function(x) {
  cl <- class(x)
  if ("icd10" %in% cl) return(x)
  class(x) <- c("icd10", cl)
  x
}

#' @describeIn set_icd_class Use generic ICD-10 class for this data. If
#'   possible, use the more specific \code{icd10who} or \code{icd10cm}.
#' @export
as.icd10 <- function(x) {
  stopifnot(is.atomic(x))
  if (inherits(x, "icd10")) return(x)
  icd10cm_pos <- match("icd10cm", class(x), nomatch = 0L)
  class(x) <- append(class(x), "icd10", after = icd10cm_pos)
  icd_conflicts(x, do_stop = TRUE)
  x
}

#' @noRd
#' @keywords internal
icd10cm <- function(x) {
  cl <- class(x)
  if ("icd10cm" %in% cl) return(x)
  if ("icd10" %in% cl) {
    class(x) <- c("icd10cm", cl)
  } else {
    class(x) <- c("icd10cm", "icd10", cl)
  }
  x
}

#' @describeIn set_icd_class Use ICD-10-CM (USA) class for the given data
#' @export
as.icd10cm <- function(x, short_code = NULL) {
  stopifnot(is.atomic(x))
  if (inherits(x, "icd10cm")) return(x)
  icd10_pos <- match("icd10", class(x))
  if (!is.na(icd10_pos)) {
    class(x) <- append(class(x), "icd10cm", after = icd10_pos - 1)
  } else {
    class(x) <- append(class(x), c("icd10cm", "icd10"), after = 0)
  }
  if (!is.null(short_code)) {
    attr(x, "icd_short_diag") <- short_code
  }
  icd_conflicts(x, do_stop = TRUE)
  x
}

#' @noRd
#' @keywords internal
icd10cm_pc <- function(x) {
  cl <- class(x)
  if ("icd10cm_pc" %in% cl) return(x)
  class(x) <- c("icd10cm_pc", cl)
  x
}

#' @describeIn set_icd_class Indicate the data are ICD-10-CM procedure codes.
#' @export
as.icd10cm_pc <- function(x) {
  stopifnot(is.atomic(x))
  if ("icd9" %in% class(x)) {
    stop("ICD-9 diagnostic class already set on this variable")
  }
  if ("icd10cm_pc" %in% class(x)) {
    stop("ICD-10-CM procedure class already set on this variable")
  }
  if (any(c("icd10cm", "icd10who") %in% class(x))) {
    stop("and ICD-10 diagnostic code class is already set")
  }
  class(x) <- append(class(x), "icd10cm_pc", after = 0)
  icd_conflicts(x, do_stop = TRUE)
  x
}

#' @noRd
#' @keywords internal
icd10who <- function(x) {
  cl <- class(x)
  if ("icd10who" %in% cl) return(x)
  class(x) <- c("icd10who", cl)
  x
}

#' @describeIn set_icd_class Use WHO ICD-10 class for the given data
#' @export
as.icd10who <- function(x, short_code = NULL) {
  stopifnot(is.atomic(x))
  if (inherits(x, "icd10who")) return(x)
  icd10_pos <- match("icd10", class(x))
  if (!is.na(icd10_pos)) {
    class(x) <- append(class(x), "icd10who", after = icd10_pos - 1)
  } else {
    class(x) <- append(class(x), c("icd10who", "icd10"), after = 0)
  }
  if (!is.null(short_code)) {
    attr(x, "icd_short_diag") <- short_code
  }
  icd_conflicts(x, do_stop = TRUE)
  x
}

#' @noRd
#' @keywords internal
icd10who <- function(x) {
  cl <- class(x)
  if ("icd10who" %in% cl) return(x)
  if ("icd10" %in% cl) {
    class(x) <- c("icd10who", cl)
  } else {
    class(x) <- c("icd10who", "icd10", cl)
  }
  x
}

#' @describeIn set_icd_class Use ICD-10-FR (France) class for the given data
#' @export
as.icd10fr <- function(x, short_code = NULL) {
  stopifnot(is.atomic(x))
  if (inherits(x, "icd10fr")) return(x)
  icd10_pos <- match("icd10", class(x))
  if (!is.na(icd10_pos)) {
    class(x) <- append(class(x), "icd10fr", after = icd10_pos - 1)
  } else {
    class(x) <- append(class(x), c("icd10fr", "icd10"), after = 0)
  }
  if (!is.null(short_code)) {
    attr(x, "icd_short_diag") <- short_code
  }
  icd_conflicts(x, do_stop = TRUE)
  x
}

#' @noRd
#' @keywords internal
icd10fr <- function(x) {
  cl <- class(x)
  if ("icd10fr" %in% cl) return(x)
  if ("icd10" %in% cl) {
    class(x) <- c("icd10fr", cl)
  } else {
    class(x) <- c("icd10fr", "icd10", cl)
  }
  x
}

#' @describeIn set_icd_class Use ICD-10-BE (Belgium) class for the given data
#' @export
as.icd10be <- function(x, short_code = NULL) {
  stopifnot(is.atomic(x))
  if (inherits(x, "icd10be")) return(x)
  icd10_pos <- match("icd10", class(x))
  if (!is.na(icd10_pos)) {
    class(x) <- append(class(x), "icd10be", after = icd10_pos - 1)
  } else {
    class(x) <- append(class(x), c("icd10be", "icd10"), after = 0)
  }
  if (!is.null(short_code)) {
    attr(x, "icd_short_diag") <- short_code
  }
  icd_conflicts(x, do_stop = TRUE)
  x
}

#' @noRd
#' @keywords internal
icd10be <- function(x) {
  cl <- class(x)
  if ("icd10be" %in% cl) return(x)
  if ("icd10" %in% cl) {
    class(x) <- c("icd10be", cl)
  } else {
    class(x) <- c("icd10be", "icd10", cl)
  }
  x
}

#' Set the ICD data structure class of a \code{matrix} or \code{data.frame}.
#'
#' These functions take your patient data, and allow you to describe whether it
#' is wide or long. \code{icd} never requires you do this, but it may help avoid
#' errors, especially if you have atypical data that might confuse \code{icd}'s
#' heuristics.
#' @name wide_vs_long
#' @template widevlong
#' @section Conversion: To convert between long and wide data, use
#'   \code{\link{long_to_wide}} or \code{\link{wide_to_long}}. Conversion
#'   functions in other packages, such as \code{ddplyr} will work, too, but will
#'   need some work to account for the typical structure of patient data and
#'   diagnostic codes. This is not done with \code{ddplyr}, \code{data.table}
#'   etc because it would add a big dependency burden. This package aims to be
#'   agnostic and use base R as much as possible.
#' @param x Input data is a \code{matrix}, \code{data.frame}, or a class that
#'   inherits one of these base structures, such as a \code{tibble}.
#' @param warn Single logical, if \code{TRUE}, the default, a warning will be
#'   shown if changing class between long and wide types.
#' @param ... Data used to construct data frame before setting the appropriate
#'   class.
#' @family ICD data types
#' @examples
#' (w <- icd_wide_data(
#'   id = c(1, 2, 3),
#'   dx01 = c("100", "441", "V20"),
#'   dx02 = c("E9981", "V10", "44004")
#' ))
#' wide_to_long(w)
#' class(uranium_pathology)
#' class(vermont_dx)
#' @seealso \code{\link{long_to_wide}} and \code{\link{wide_to_long}}
NULL

#' @describeIn wide_vs_long Set class on a matrix or data.frame to
#'   \code{icd_long_data}. To convert wide to long data, use
#'   \code{\link{wide_to_long}}.
#' @family ICD data types
#' @export
as.icd_long_data <- function(x, warn = TRUE) {
  # Also from Wickham: "When implementing a matrix/array class, you should
  # implement these methods: dim (gets you nrow and ncol), t, dimnames (gets you
  # rownames and colnames), dimnames<- (gets you colnames<-, rownames<-), cbind,
  # rbind."
  stopifnot(is.data.frame(x) || is.matrix(x))
  assert_data_frame(x)
  if (warn && is.icd_wide_data(x)) {
    warning(
      "Setting 'icd_long_data' on a data.frame or matrix which already",
      " has 'icd_wide_data' class"
    )
  }
  if (is.icd_long_data(x)) {
    return(x)
  }
  class(x) <- c("icd_long_data", class(x))
  x
}

#' @describeIn wide_vs_long Construct a \code{data.frame}, adding the
#'   \code{icd_long_data} class.
#' @export
as.icd_wide_data <- function(x, warn = TRUE) {
  stopifnot(is.data.frame(x) || is.matrix(x))
  if (warn && is.icd_long_data(x)) {
    warning(
      "Setting 'icd_wide_data' on a data.frame or matrix which already",
      " has 'icd_long_data' class"
    )
  }
  if (is.icd_wide_data(x)) {
    return(x)
  }
  class(x) <- c("icd_wide_data", class(x))
  x
}

#' @describeIn wide_vs_long Construct a \code{data.frame}, adding the
#'   \code{icd_long_data} class.
#' @family ICD data types
#' @export
icd_long_data <- function(..., warn = TRUE)
  as.icd_long_data(data.frame(...), warn = warn)

#' @describeIn wide_vs_long Construct a \code{data.frame}, adding the
#'   \code{icd_wide_data} class.
#' @export
icd_wide_data <- function(x, ..., warn = TRUE)
  as.icd_wide_data(data.frame(...), warn = warn)

#' @details Using \code{attributes} instead of \code{class} is a better fit for
#'   the data. It simplifies S3 dispatch, and appears to be very fast to get or
#'   set using the built-in R functions.
#' @keywords internal
#' @noRd
comorbidity_map <- function(x) {
  assert_list(x, min.len = 1, names = "unique")
  cl <- class(x)
  if ("comorbidity_map" %in% cl) return(x)
  class(x) <- c("comorbidity_map", cl)
  x
}

#' Set the class of a named list to show it is a comorbidity map.
#' @param x A list of depth one, with unique names, and elements that are
#'   character vectors.
#' @family ICD data types
#' @export
as.comorbidity_map <- function(x) {
  stopifnot(
    is.list(x),
    !is.null(names(x)),
    !any(names(x) == ""),
    !anyDuplicated(names(x))
  )
  if (inherits(x, "comorbidity_map")) {
    return(x)
  }
  class(x) <- c("comorbidity_map", class(x))
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
#' stopifnot(is.short_diag(icd10_map_ahrq[[1]]))
#' @keywords internal
#' @export
`[[.comorbidity_map` <- function(x, index, ...) {
  out <- NextMethod()
  # no need to reset attributes?
  out
}

#' Combine ICD codes
#'
#' These function implement ICD specific methods for \code{c}, i.e., combinations of lists or vectors of codes, while
#' preserving ICD classes. Base R \code{c} just drops all user defined classes
#' and casts down to lowest common denominator, e.g. if mixing numbers and
#' characters. No attempt here to catch all possible combinations of feeding in
#' mixed ICD types and other types. Let R do what it normally does, but just try
#' to keep classes of the first item in the list.
#' @param ... elements to combine
#' @param warn single logical value, if TRUE, will give warnings when
#'   incompatible types are combined using \code{c}
#' @examples
#' # Care with the following:
#' c(as.icd9("E998"), as.icd10("A10"))
#' # which results in both codes sharing the 'icd9' class.
#' @seealso \link[=set_icd_class]{ICD data types}
#' @name combine
#' @export
c.icd9 <- function(..., warn = FALSE) {
  dots <- list(...)
  if (warn &&
    any(vapply(dots, icd_conflicts.icd9, FUN.VALUE = logical(1)))) {
    stop(
      "Do you really want to combine ICD-9 codes (first argument) ",
      " with ICD-9 codes (subsequent arguments)? If so, use 'unclass'",
      " on some or all the arguments"
    )
  }
  structure(c(lapply(dots, unclass), recursive = TRUE),
    class = class(.subset2(dots, 1))
  )
  # SOMEDAY: would be nice to set the attribute, but by default, R's 'c' drops
  # attributes.
}

#' @rdname combine
#' @examples
#' # ICD-10 codes
#' (a <- as.icd10("A100SSX"))
#' (b <- as.icd10("Z999A"))
#' c(a, b)
#' c(as.icd_short_diag(a), as.icd_short_diag(b))
#' (d <- as.icd10("A10.0SSX"))
#' (e <- as.icd10("Z99.9A"))
#' c(d, e)
#' c(as.icd_decimal_diag(d), as.icd_decimal_diag(e))
#' # warn when mixing attribute types
#' suppressWarnings(
#'   c(as.icd_short_diag(a), as.icd_decimal_diag(e))
#' )
#' @export
c.icd10 <- function(..., warn = FALSE) {
  dots <- list(...)
  base_class <- class(.subset2(dots, 1))
  if (warn &&
    any(vapply(dots, icd_conflicts.icd10, FUN.VALUE = logical(1)))) {
    stop(
      "Do you really want to combine ICD-10 codes (first argument) ",
      " with ICD-9 codes (subsequent arguments)? If so, use 'unclass'",
      " on some or all the arguments."
    )
  }
  out <- structure(c(unlist(dots)), class = base_class)
  # only set this attribute if all the consituent terms have the same attribute
  # present. One NULL or one conflict will mean the attribute is not set
  attribs <- lapply(dots, attr, which = "icd_short_diag")
  nulls <- vapply(attribs, is.null, logical(1))
  if (all(nulls)) return(out)
  short_attribs <- unlist(attribs[!nulls])
  if (all(short_attribs)) {
    attr(out, "icd_short_diag") <- TRUE
  } else if (!any(short_attribs)) {
    attr(out, "icd_short_diag") <- FALSE
  } else {
    warning(
      "Combining codes with different short and decimal attributes.",
      " 'icd_short_diag' attribute will be removed from result. Did",
      " you mean to do this?"
    )
  }
  out
}

#' extract subset(s) from ICD data
#'
#' exactly the same as using \code{x[n]} or \code{x[[n]]} but preserves the ICD
#' classes in result
#' @param x input data with list, vector, factor, and class set to an ICD type.
#' @template dotdotdot
#' @examples
#' x <- list(my_codes = as.icd9(c("V10.1", "441.1")))
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
#' @name subset
NULL

#' @describeIn subset Extract ICD-9 codes
#' @export
`[.icd9` <- function(x, ...) {
  y <- NextMethod()
  attr(y, "icd_short_diag") <- attr(x, "icd_short_diag")
  class(y) <- class(x)
  y
}

#' @describeIn subset Extract ICD-9 codes
#' @export
`[[.icd9` <- function(x, ...) {
  y <- NextMethod()
  attr(y, "icd_short_diag") <- attr(x, "icd_short_diag")
  if (mode(x) != "list") {
    class(y) <- class(x)
  }
  y
}

#' @describeIn subset Extract ICD-10 codes
#' @export
`[.icd10` <- function(x, ...) {
  y <- NextMethod()
  attr(y, "icd_short_diag") <- attr(x, "icd_short_diag")
  class(y) <- class(x)
  y
}

#' @describeIn subset Extract ICD-10 codes
#' @export
`[[.icd10` <- function(x, ...) {
  y <- NextMethod()
  attr(y, "icd_short_diag") <- attr(x, "icd_short_diag")
  if (mode(x) != "list") {
    class(y) <- class(x)
  }
  y
}

#' Test presence of ICD classes
#'
#' This merely checks whether the given object is a certain type of ICD code, it
#' does no validation of any kind. For validation, see \code{\link{is_valid}}.
#' @param x Any object which may have ICD-related classes set
#' @examples
#' # A character string is not itself an ICD code
#' is.icd9("100.1")
#' is_valid("100.1")
#' is.icd9(as.icd9cm("100.1"))
#' @export
is.icd9 <- function(x) {
  inherits(x, icd9_classes)
}

#' @rdname is.icd9
#' @export
is.icd9cm <- function(x) inherits(x, "icd9cm")

#' @rdname is.icd9
#' @export
is.icd9cm_pc <- function(x) inherits(x, "icd9cm_pc")

#' @rdname is.icd9
#' @export
is.icd9who <- function(x) inherits(x, "icd9who")

#' @rdname is.icd9
#' @export
is.icd10 <- function(x) inherits(x, icd10_classes)

#' @rdname is.icd9
#' @export
is.icd10cm <- function(x) inherits(x, "icd10cm")

#' @rdname is.icd9
#' @export
is.icd10cm_pc <- function(x) inherits(x, "icd10cm_pc")

#' @rdname is.icd9
#' @export
is.icd10who <- function(x) inherits(x, "icd10who")

#' @rdname is.icd9
#' @export
is.icd10fr <- function(x) inherits(x, "icd10fr")

#' @rdname is.icd9
#' @export
is.icd10be <- function(x) inherits(x, "icd10be")

#' Test for class describing patient data
#'
#' This function does not examine the data itself; it just checks whether one of
#' the classes \code{icd_long_data} or \code{icd_wide_data} class is set.
#' @param x Typically a \code{data.frame}
#' @seealso \code{\link{icd_long_data}}
#' @export
is.icd_long_data <- function(x) inherits(x, "icd_long_data")

#' @rdname is.icd_long_data
#' @export
is.icd_wide_data <- function(x) inherits(x, "icd_wide_data")

#' @rdname is.icd9
#' @export
is.comorbidity_map <- function(x) inherits(x, "comorbidity_map")

#' Print ICD codes and comorbidity maps cleanly
#' @examples
#' x <- structure(
#'   c("40201", "2258", "7208", "25001", "34400", "4011", "4011", NA),
#'   class = c("icd9cm", "icd9", "character"),
#'   icd_short_diag = TRUE
#' )
#' \dontrun{
#' print(x)
#' print(x, verbose = TRUE)
#' # as.factor drops any 'icd' classes
#' print(as.factor(x), verbose = TRUE)
#' }
#' @param x ICD codes to be printed
#' @param verbose Annotate based on code attributes, e.g., decimal versus short
#'   codes.
#' @template dotdotdot
#' @export
print.icd9 <- function(x, verbose = FALSE, ...)
  print_codes(x,
    ifelse(is.icd9cm(x), "ICD-9-CM", "ICD-9"),
    verbose = verbose, ...
  )

#' @rdname print.icd9
#' @examples
#' \dontrun{
#' u <- uranium_pathology[1:10, "icd10"]
#' print(u)
#' print(u, verbose = TRUE)
#' # as.character will unclass the 'icd' classes
#' print(as.character(u), verbose = TRUE)
#' a <- structure(c("R21", "Z21"),
#'   class = c("icd10cm", "icd10", "character")
#' )
#' print(a, verbose = TRUE)
#' }
#' @export
print.icd10 <- function(x, verbose = FALSE, ...) {
  icd10cl <- grep("icd10.+", class(x), value = TRUE)
  if (length(icd10cl) == 0) {
    icd10cl <- "default"
  }
  sub_class <- switch(icd10cl,
    icd10cm = "ICD-10-CM Diagnostic Codes",
    icd10cm_pc = "ICD-10-CM Procedure Codes",
    icd10who = "WHO ICD-10 (Diagnostic Codes)",
    icd10fr = "ICD-10-FR (French Diagnostic Codes)",
    icd10be = "ICD-10-BE (Belgian Diagnostic Codes)",
    icd10be_pc = "ICD-10-BE (Belgian Procedure Codes)",
    "ICD-10 Codes (Subtype not set)"
  )
  print_codes(x, sub_class, verbose = verbose, ...)
}

print_codes <- function(x, code_str, verbose = FALSE, ...) {
  if (verbose) {
    if (is.icd_short_diag(x)) {
      cat("Short-form", code_str, "codes:\n")
    } else if (is.icd_decimal_diag(x)) {
      cat("Decimal-form", code_str, "codes:\n")
    } else {
      cat(code_str, "codes (short/decimal attribute not set):\n")
    }
  }
  x <- icd_attr_clean(x)
  icd_cl <- class(x) %in% c(icd9_classes, icd10_classes)
  class(x) <- class(x)[!icd_cl]
  print(x, ...)
}
