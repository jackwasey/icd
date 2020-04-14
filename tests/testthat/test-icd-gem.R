context("ICD-9 / ICD-10 GEM routines")


# create target data ICD-9 to ICD-10 mapping -----------------------------------

to_icd9s <- function(x) {
  as.icd9cm(as.short_diag(x))
}

to_icd10s <- function(x) {
  as.icd10cm(as.short_diag(x))
}

# GEM for the ICD-9 codes 0010, 24951, and 36570
# ----------------------------------------------
# source target flags
#   0010   A000 00000
#  24951 E08311 10111
#  24951 E08319 10111
#  24951  E0836 10111
#  24951  E0839 10000
#  24951  E0865 10112
#  24951 E09311 10111
#  24951 E09319 10111
#  24951  E0936 10111
#  24951  E0939 10000
#  36570   NoDx 11000

# the target per-scenario code lists.  These are computed by hand, based on the
# values of above GEM entries, plus an added extra empty entry for an invalid
# ICD-9 code of 99999 (to be included as input during testing).
target_codes <- list(
  list("0" = to_icd10s("A000")),
  list("0" = to_icd10s(c("E0839", "E0939"))),
  list("1" = to_icd10s(c("E08311", "E08319", "E0836", "E09311", "E09319", "E0936")), "2" = to_icd10s("E0865")),
  list(), # for code 36570
  list()  # for code 99999
)

# the target output for when the input codes are in short form
target_short_df <- data.frame(
  source   = to_icd9s(c("0010", "24951", "24951", "36570", "99999")),
  scenario = c("0", "0", "1", "0", "0"),
  type     = c("simple", "simple", "combination", "no mapping", "not in GEM"),
  approx   = c(FALSE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
target_short_df$codes <- target_codes

# the target output for when the input codes are in decimal form
target_decimal_df <- target_short_df
target_decimal_df$source <- as.icd9cm(short_to_decimal(target_short_df$source))
target_decimal_df$codes <- lapply(
  target_short_df$codes,
  function(x) lapply(
    x,
    function(y) as.icd10cm(short_to_decimal(y))
  )
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


# create target data ICD-9 PC to ICD-10 PCS mapping ----------------------------

to_icd9s_pc <- function(x) {
  as.icd9cm_pc(as.short_diag(x))
}

to_icd10s_pc <- function(x) {
  as.icd10cm_pc(as.short_diag(x))
}

# GEM for the ICD-9 PC codes 0053 and 0211
# ----------------------------------------
# source  target flags
#   0053 0JH607Z 10000
#   0053 0JH607Z 10111
#   0053 0JH637Z 10000
#   0053 0JH637Z 10111
#   0053 0JH807Z 10000
#   0053 0JH837Z 10000
#   0053 0JPT0PZ 10112
#   0053 0JPT3PZ 10112
#   0211 00Q20ZZ 10000
#   0211 00Q23ZZ 10000
#   0211 00Q24ZZ 10000

# the target per-scenario code lists.  These are computed by hand, based on the
# values of above GEM entries
target_pc_codes <- list(
  list(`0` = to_icd10s_pc(c("0JH607Z", "0JH637Z", "0JH807Z", "0JH837Z"))),
  list(`1` = to_icd10s_pc(c("0JH607Z", "0JH637Z")), `2` = to_icd10s_pc(c("0JPT0PZ", "0JPT3PZ"))),
  list(`0` = to_icd10s_pc(c("00Q20ZZ", "00Q23ZZ", "00Q24ZZ")))
)

# the target output for when the input codes are in short form
target_pc_short_df <- data.frame(
  source   = to_icd9s_pc(c("0053", "0053", "0211")),
  scenario = c("0", "1", "0"),
  type     = c("simple", "combination", "simple"),
  approx   = c(TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
target_pc_short_df$codes <- target_pc_codes




# icd_gem ----------------------------------------------------------------------

test_that("icd_gem ICD-9 to ICD-10 short form", {

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


test_that("icd_gem ICD-9 to ICD-10 decimal form", {

  # create object under test
  icd_data <- as.icd9cm(as.decimal_diag(c("001.0", "249.51", "365.70", "999.99")))
  actual <- icd_gem(icd_data)

  # general structure
  expect_true(is.data.frame(actual))
  expect_identical(names(actual), c("source", "scenario", "type", "approx", "codes"))

  # column values
  expect_identical(actual$source,   target_decimal_df$source)
  expect_identical(actual$scenario, target_decimal_df$scenario)
  expect_identical(actual$type,     target_decimal_df$type)
  expect_identical(actual$approx,   target_decimal_df$approx)
  expect_identical(actual$codes,    target_decimal_df$codes)

  # overall
  expect_identical(actual, target_decimal_df)
})


test_that("icd_gem ICD-9 to ICD-10 short form, condensed output", {

  check_condensed_form_nm <- function(x) {
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
  expect_true(all(vapply(actual$mapping, check_condensed_form_nm, logical(1L))))

  # overall
  expect_identical(actual, target_short_condensed_df)
})


test_that("icd_gem ICD-9 PC to ICD-10 PCS short form", {

  # create object under test
  icd_data <- as.icd9cm_pc(as.short_diag(c("0053", "0211")))
  actual <- icd_gem(icd_data)

  # general structure
  expect_true(is.data.frame(actual))
  expect_identical(names(actual), c("source", "scenario", "type", "approx", "codes"))

  # column values
  expect_identical(actual$source,   target_pc_short_df$source)
  expect_identical(actual$scenario, target_pc_short_df$scenario)
  expect_identical(actual$type,     target_pc_short_df$type)
  expect_identical(actual$approx,   target_pc_short_df$approx)
  expect_identical(actual$codes,    target_pc_short_df$codes)

  # overall
  expect_identical(actual, target_pc_short_df)
})
