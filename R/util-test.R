#nocov start

expect_no_warn <- function(object, expected, ...)
  testthat::expect_warning(object, regexp = NA, ...)

expect_no_error <- function(object, expected, ...)
  testthat::expect_error(object, regexp = NA, ...)

#' expect equal, ignoring any ICD classes
#'
#' Strips any \code{icd} classes (but not others) before making comparison
#' @noRd
#' @keywords internal debugging
expect_equal_no_icd <- function(object, expected, ...) {
  class(object) <- class(object)[class(object) %nin% icd_all_classes]
  class(expected) <- class(expected)[class(expected) %nin% icd_all_classes]
  testthat::expect_equivalent(object, expected, ...)
}

#' @noRd
#' @keywords internal debugging
expect_equal_no_class_order <- function(object, expected, ...) {
  eval(bquote(testthat::expect_true(all(class(.(object)) %in% class(.(expected))), ...)))
  eval(bquote(testthat::expect_equivalent(unclass(.(object)), unclass(.(expected)), ...)))
}

#' @describeIn classes_ordered \code{testthat} \code{expect} function
#'   for ICD classes to be in correct order.
#' @keywords internal debugging
#' @noRd
expect_classes_ordered <- function(x)
  eval(bquote(testthat::expect_true(classes_ordered(.(x)))))

#' generate random ICD-9 codes
#'
#' @keywords internal debugging datagen
generate_random_short_icd9 <- function(n = 50000)
  as.character(floor(stats::runif(min = 1, max = 99999, n = n)))

#' generate random ICD-9 codes
#'
#' Uses billable ICD-10-CM codes from current master list
#' @param n number to select, passed to \code{sample}
#' @template short_code
#' @keywords internal debugging datagen
generate_random_short_icd10cm_bill <- function(n = 10, short_code = TRUE) {
  i <- get_from_icd_data("icd10cm_latest", icd.data::icd10cm2016)
  x <- sample(
    unlist(
      i[i$billable == 1, "code"]),
    replace = TRUE, size = n)
  if (short_code) x else short_to_decimal(x)
}

generate_random_decimal_icd9 <- function(n = 50000)
  paste(
    round(stats::runif(min = 1, max = 999, n = n)),
    sample(expand_minor.icd9(""), replace = TRUE, size = n),
    sep = "."
  )

generate_random_pts <- function(...)
  generate_random_ordered_pts(...)

generate_random_ordered_pts <- function(...) {
  x <- generate_random_unordered_pts(...)
  x[order(x$visit_id), ]
}

generate_random_unordered_pts <- function(num_patients = 50000,
                                          dz_per_patient = 20,
                                          n = num_patients,
                                          np = dz_per_patient,
                                          fun = generate_random_short_icd9) {
  set.seed(1441)
  pts <- round(n / np)
  data.frame(
    visit_id = as_char_no_warn(sample(seq(1, pts), replace = TRUE, size = n)),
    code = fun(n),
    poa = as.factor(
      sample(x = c("Y", "N", "n", "n", "y", "X", "E", "", NA),
             replace = TRUE, size = n)),
    stringsAsFactors = FALSE
  )
}

generate_random_short_ahrq_icd9 <- function(n = 50000)
  sample(unname(unlist(icd::icd9_map_ahrq)), size = n, replace = TRUE)

#' generate random strings
#'
#' Mixed upper and lower case, with replacement
#' @keywords internal debugging datagen
random_string <- function(n, max_chars = 4) {
  rand_ch <- function()
    sample(c(LETTERS, letters, 0:9, rep("", times = 50)), replace = TRUE, size = n)

  v <- vapply(1:max_chars,
              FUN = function(x) rand_ch(),
              FUN.VALUE = character(n)
  )
  apply(v, 1, paste0, collapse = "")
}

#' allow \pkg{microbenchmark} to compare multiple results
#' @param x list of values to compare for identity, e.g. results from evaluated
#'   expression in \code{microbenchmark::microbenchmark}
#' @keywords internal
all_identical <- function(x)
  all(vapply(x[-1], function(y) identical(x[[1]], y), FUN.VALUE = logical(1)))

get_one_of_each <- function()
  c("002.3", "140.25", "245", "285", "290.01", "389.00",
    "390.00", "518", "525", "581", "631", "700", "720", "759.99",
    "765", "780.95", "800", "V02.34", "E900.4")

#' Set up a test environment which also has the internal functions
#' @keywords internal debugging data
test_env <- function() {
  ns <- getNamespace("icd")
  list2env(as.list(ns, all.names = TRUE), parent = parent.env(ns))
}

#' Generate simulated NEDS data for PCCC and wide-data testing
#' @param n Integer number of rows of data to generate
#' @param ncol Integer number of diagnostic code columns, default of 20 matches
#'   NEDS
#' @param icd10 Logical, default `TRUE` to sample ICD-10-CM codes. `FALSE` gives
#'   `ICD-9`
#' @template verbose
#' @examples
#' summary(icd::comorbid_pccc_dx(icd:::generate_neds_pts()))
#' neds <- icd:::generate_neds_pts(n = 100, ncol = 10L, icd10 = FALSE)
#' stopifnot(dim(neds) == c(100L, 11L))
#' summary(icd::comorbid_pccc_dx(neds))
#' \dontrun{
#' # original size data for PCCC benchmarking:
#' set.seed(1441)
#' neds <- icd:::generate_neds_pts(28584301L)
#' neds_comorbid <- icd::comorbid_pccc_dx(neds)
#' }
#' @keywords internal
generate_neds_pts <- function(
  n = 1000L,
  ncol = 20L,
  icd10 = TRUE,
  verbose = FALSE
) {
  codes <- if (icd10) {
    i <- get_from_icd_data("icd10cm_latest", icd.data::icd10cm2016)
    unclass(as_char_no_warn(i$code))
  } else {
    unclass(as_char_no_warn(icd.data::icd9cm_hierarchy$code))
  }
  dat <- data.frame(id = as.character(n + seq(n)),
                    icd_code = sample(codes, n, replace = TRUE),
                    stringsAsFactors = TRUE)
  pts_per_code_pos <- as.integer(n / (seq(ncol)) ^ 4)
  dat_wide_factors <- data.frame(id = dat$id,
                                 dx01 = dat$icd_code,
                                 stringsAsFactors = TRUE)
  for (dx in seq(2L, ncol)) {
    dx_str <- sprintf("%02i", dx)
    if (verbose) message("building column:", dx_str)
    len <- pts_per_code_pos[dx]
    l <-  unique(c(NA, sample(codes, len, replace = TRUE)))
    f <- as.integer(sample(c(seq_along(l), rep(1L, n - length(l)))))
    attr(f, "levels") <- l
    attr(f, "class") <- "factor"
    dat_wide_factors[[paste0("dx", dx_str)]] <- f
  }
  dat_wide_str <- dat_wide_factors[1]
  for (i in seq_along(dat_wide_factors)) {
    if (i == 1) next
    dat_wide_str[names(dat_wide_factors)[i]] <-
      as.character(dat_wide_factors[[i]])
  }
  dat_wide_str
}

.catch <- function() {
  .Call("run_testthat_tests", PACKAGE = "icd")
}
#nocov end

assert_flag <- function(x) {
  stopifnot(is.logical(x), length(x) == 1L)
}

assert_string <- function(x) {
  stopifnot(is.character(x), length(x) == 1L)
}

assert_character <- function(x, ...) {
  stopifnot(is.character(x))
}

assert_integer <- function(x, len = NULL) {
  stopifnot(is.integer(x))
  if (!is.null(len)) stopifnot(length(x) == len)
}

assert_int <- function(x, len = NULL, ...) {
  stopifnot(is.integer(x) || floor(x) == x)
  if (!is.null(len)) stopifnot(length(x) == 1L)
}

assert_list <- function(x, ...) {
  stopifnot(is.list(x))
}

assert_scalar <- function(x) {
  stopifnot(length(x) == 1L)
}

assert_fac_or_char <- function(x) {
  stopifnot(is.factor(x) || is.character(x))
}

assert_data_frame <- function(x, ...) {
  stopifnot(is.data.frame(x))
}

assert_matrix <- function(x, ...) {
  stopifnot(is.matrix(x))
}

expect_logical <- function(x, ...) {
  testthat::expect_true(is.logical(x))
}

expect_character <- function(x, ...) {
  testthat::expect_true(is.character(x))
}

# workaround so icd.data 1.0 will not cause CRAN or user errors
skip_missing_icd10who <- function(ver = "2016", lang = "en") {
  if (exists("skip_missing_icd10who", envir = asNamespace("icd.data"))) {
    f <- get("skip_missing_icd10who", envir = asNamespace("icd.data"))
    f(ver = ver, lang = lang)
  } else {
    testthat::skip("No skip_missing_icd10who function, so no data")
  }
}
