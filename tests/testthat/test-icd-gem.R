context("ICD-9 / ICD-10 GEM routines")


# create target data -----------------------------------------------------------

# input data
icd_data <- data.frame(
  source = c("0010", "24951", "24951", "24951", "24951", "24951", "24951", "24951", "24951", "24951", "36570"),
  target = c("A000", "E08311", "E08319", "E0836", "E0839", "E0865", "E09311", "E09319", "E0936", "E0939", "NoDx"),
  flags  = c("00000", "10111", "10111", "10111", "10000", "10112", "10111", "10111", "10111", "10000", "11000"),
  stringsAsFactors = FALSE
)

# the target per-scenario code lists
target_codes <- list(
  list("0" = "A000"),
  list("0" = c("E0839", "E0939")),
  list("1" = c("E08311", "E08319", "E0836", "E09311", "E09319", "E0936"), "2" = "E0865"),
  list(),
  list()
)

# the target output for when the input codes are in short form
target_short_df <- data.frame(
  source   = as.icd9cm(as.short_diag(c("0010", "24951", "24951", "36570", "99999"))),
  scenario = c("0", "0", "1", "0", "0"),
  type     = c("simple", "simple", "combination", "no mapping", "not in GEM"),
  approx   = c(FALSE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
target_short_df$codes <- target_codes

# the target output for when the input codes are in decimal form
target_decimal_df <- cbind(
  data.frame(
    source    = as.icd9cm(short_to_decimal(target_short_df$source)),
    converted = target_short_df$source,
    stringsAsFactors = FALSE
  ),
  target_short_df[, c("scenario", "type", "approx", "codes")]
)

# the target output for when the input codes are in short form and `condense =
# TRUE`
target_short_condensed_list <- lapply(
  X   = split(target_short_df, target_short_df$source),
  FUN = function(x) {
    out <- data.frame(
      source           = x$source[1L],
      stringsAsFactors = FALSE)
    out$mapping <- list(x[, setdiff(names(x), "source")])
    row.names(out$mapping[[1L]]) <- seq_len(nrow(out$mapping[[1L]]))
    out
  }
)
target_short_condensed_df <- do.call(rbind, target_short_condensed_list)
row.names(target_short_condensed_df) <- seq_len(nrow(target_short_condensed_df))




# icd_gem ----------------------------------------------------------------------

test_that("create_icd_map expected input short form", {

  # create object under test
  icd_data <- as.icd9cm(as.short_diag(c("0010", "24951", "36570", "99999")))
  actual <- icd_gem(icd_data)

  # general structure
  expect_true(is.data.frame(actual))
  expect_identical(names(actual), c("source", "scenario", "type", "approx", "codes"))

  # column values
  expect_identical(actual$source,   target_short_df$source)
  expect_identical(actual$scenario, target_short_df$scenario)
  expect_identical(actual$type,     target_short_df$type)
  expect_identical(actual$approx,   target_short_df$approx)
  expect_identical(actual$codes,    target_short_df$codes)

  # overall
  expect_identical(actual, target_short_df)
})

test_that("create_icd_map expected input decimal form", {

  # create object under test
  icd_data <- as.icd9cm(as.decimal_diag(c("001.0", "249.51", "365.70", "999.99")))
  actual <- icd_gem(icd_data)

  # general structure
  expect_true(is.data.frame(actual))
  expect_identical(names(actual), c("source", "converted", "scenario", "type", "approx", "codes"))

  # column values
  expect_identical(actual$source,    target_decimal_df$source)
  expect_identical(actual$converted, target_decimal_df$converted)
  expect_identical(actual$scenario,  target_decimal_df$scenario)
  expect_identical(actual$type,      target_decimal_df$type)
  expect_identical(actual$approx,    target_decimal_df$approx)
  expect_identical(actual$codes,     target_decimal_df$codes)

  # overall
  expect_identical(actual, target_decimal_df)
})

test_that("create_icd_map expected input short condensed", {

  check_short_form_nm <- function(x) {
    identical(names(x), c("scenario", "type", "approx", "codes"))
  }

  # create object under test
  icd_data <- as.icd9cm(as.short_diag(c("0010", "24951", "36570", "99999")))
  actual <- icd_gem(icd_data, condense = TRUE)

  # general structure
  expect_true(is.data.frame(actual))
  expect_identical(names(actual), c("source", "mapping"))

  # top-level structure
  expect_identical(actual$source, target_short_condensed_df$source)
  expect_true(is.list(actual$mapping))

  # nested data frame structure
  expect_true(all(vapply(actual$mapping, is.data.frame, logical(1L))))
  expect_true(all(vapply(actual$mapping, check_short_form_nm, logical(1L))))

  # overall
  expect_identical(actual, target_short_condensed_df)
})
