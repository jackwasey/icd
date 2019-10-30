#' Create a Data Frame Representing An ICD Code Mapping
#'
#' Takes either an ICD-9 to ICD-10 or ICD-10 to ICD-9 GEM (the GEMs for either
#' mapping direction takes the same structure) and creates a data frame
#' representing the mapping.  The newly created data frame does not take the
#' same structure as the GEMs, but instead takes an equivalent form that is
#' intended to be more amenable for querying.  Please see the object
#' documentation for \code{\link{icd_gem_9_to_10}}
#' full details on the form of the output mapping form.
#'
#' @param icd_source_df A data frame with character columns \code{source},
#'   \code{target}, and \code{flags}, and such that each element of the
#'   \code{flags} column has exactly 5 characters.
#' @return A data frame with one row per source code scenario, and the columns
#'   \code{source}, \code{scenario}, \code{type}, \code{approx}, and
#'   \code{codes}.
#' @noRd
create_icd_map <- function(icd_source_df) {

  # expand the flags field into multiple columns, and split the data frame into
  # a list of one data frame per scenario
  icd_expanded_df <- create_icd_map_expanded_df(icd_source_df)
  scenario_list <- create_icd_map_scenario_list(icd_expanded_df)

  # create a single-row data frame for each scenario, and combine all of the
  # rows into a single data frame
  out <- lapply(scenario_list, create_icd_map_single_scenario)
  do.call(rbind, out)
}


#' Replace the Flag Column With Multiple Columns
#'
#' Create a data frame with the same rows and columns as before, but with the
#' flag column split out into multiple columns.
#'
#' @param icd_source_df A data frame which columns \code{source}, \code{target},
#'   and \code{flags}.
#' @return A data frame with the same \code{source}, and \code{target} columns
#'   as in the input data, and with additional columns appended that are
#'   obtained by a call to \code{create_icd_map_flags_df}.
#' @noRd
create_icd_map_expanded_df <- function(icd_source_df) {
  flags_df <- create_icd_map_flags_df(icd_source_df$flags)
  cbind(
    icd_source_df[, c("source", "target")],
    flags_df
  )
}


#' Split Out the Flag Vector Into Multiple Columns
#'
#' @param flags A character vector such that each element has exactly 5
#'   characters.  Furthermore, it is expected that the first 3 characters are
#'   either \code{"0"} or \code{"1"}.
#' @return A data frame with the columns \code{approx}, \code{no_map},
#'   \code{combin}, \code{scenario}, and \code{choice_list}.
#' @noRd
create_icd_map_flags_df <- function(flags) {

  stopifnot(all(nchar(flags) == 5L))

  # create a matrix `flags_df` with each column taking the value of the
  # corresponding digit in the `Flags` column
  flags_vec <- unlist(strsplit(flags, ""))
  flags_matrix <- matrix(
    data  = flags_vec,
    ncol  = 5L,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c("approx", "no_map", "combin", "scenario", "choice_list")
    )
  )

  # create a data frame based off of the flags matrix but with logical values
  # for the binary entries
  data.frame(
    approx           = (flags_matrix[, "approx"] == "1"),
    no_map           = (flags_matrix[, "no_map"] == "1"),
    combin           = (flags_matrix[, "combin"] == "1"),
    scenario         = flags_matrix[, "scenario"],
    choice_list      = flags_matrix[, "choice_list"],
    stringsAsFactors = FALSE
  )
}


#' Create a List of Per-Scenario Data Frames
#'
#' @param icd_expanded_df A data frame with columns \code{source} and
#'   \code{scenario}.
#' @return A list such that each entry is a data frame with all of the rows from
#'   the input data frame that correspond to a unique pair of \code{source} and
#'   \code{scenario} values.
#' @noRd
create_icd_map_scenario_list <- function(icd_expanded_df) {

  split_by_scenario <- function(df) {
    split(df, df$scenario)
  }

  entry_list <- split(icd_expanded_df, icd_expanded_df$source)
  unlist(
    x         = lapply(entry_list, split_by_scenario),
    recursive = FALSE,
    use.names = FALSE
  )
}


#' Create An ICD Code Map for a Single Scenario
#'
#' @param scenario_df A data frame with columns \code{source}, \code{target},
#'   \code{approx}, and \code{choice_list}.
#' @return A data frame with exactly 1 row, and the columns \code{source},
#'   \code{scenario}, \code{type}, \code{approx}, and \code{codes}.
#' @noRd
create_icd_map_single_scenario <- function(scenario_df) {

  create_type_str <- function(codes) {
    if (length(codes[[1L]]) == 0L) {
      "no mapping"
    }
    else if (length(codes[[1L]]) == 1L) {
      "simple"
    }
    else {
      "combination"
    }
  }

  # create a length-1 list such that the sole element is a list of character
  # vectors, with each vector the target codes corresponding to a single choice.
  # The scenarios without a mapping are treated as having 0 codes target codes.
  if (any(scenario_df$no_map)) {
    stopifnot(nrow(scenario_df) == 1L, scenario_df$target == "NoDx")
    codes <- list(list())
  }
  else {
    codes <- list(split(scenario_df$target, scenario_df$choice_list))
  }

  # create 1-row data frame and return
  out <- data.frame(
    source           = scenario_df$source[1L],
    scenario         = scenario_df$scenario[1L],
    type             = create_type_str(codes),
    approx           = all(scenario_df$approx),
    stringsAsFactors = FALSE
  )
  out$codes = codes
  out
}


#' Check Some Structural Assumptions Regarding the GEMS
#'
#' Checks whether our assumptions about having constant within-scenario values
#' hold for the columns \code{approx}, \code{no_map}, and \code{combin}.
#'
#' @param icd_source_df A data frame with character columns \code{source},
#'   \code{target}, and \code{flags}, and such that each element of the
#'   \code{flags} column has exactly 5 characters.
#' @return A data frame with columns \code{source}, \code{scenario},
#'   \code{approx}, \code{no_map}, and \code{combin}.  The latter columns are
#'   logical and describe whether the within-scenario assumptions hold for each
#'   scenario.
#' @noRd
check_gem_assumptions <- function(icd_source_df) {

  check_const <- function(x) {
    stopifnot(is.atomic(x), length(x) > 0L)
    all(x == x[1L])
  }

  check_no_map_is_nodx <- function(df) {
    ((! df$no_map) | (df$no_map & (df$target == "NoDx")))
  }

  check_within_scenario_const <- function(df) {
    data.frame(
      source           = df$source[1L],
      scenario         = df$scenario[1L],
      approx           = check_const(df$approx),
      no_map           = check_const(df$no_map),
      no_map_is_nodx   = check_no_map_is_nodx(df),
      combin           = check_const(df$combin),
      stringsAsFactors = FALSE
    )
  }

  # expand the flags field into multiple columns, and split the data frame into
  # a list of one data frame per scenario
  icd_expanded_df <- create_icd_map_expanded_df(icd_source_df)
  scenario_list <- create_icd_map_scenario_list(icd_expanded_df)

  # create a single-row data frame for each scenario, and combine all of the
  # rows into a single data frame
  out <- lapply(scenario_list, check_within_scenario_const)
  do.call(rbind, out)
}




# create tables ----------------------------------------------------------------

# parse the ICD-9 to ICD-10 GEM CSV and store as character columns
icd_gem_9_to_10_raw <- read.table(
  file       = "data-raw/icd-gem-2018-convert-9-to-10.txt",
  header     = FALSE,
  col.names  = c("source", "target", "flags"),
  colClasses = c("character", "character", "character")
)

# parse the ICD-10 to ICD-9 GEM CSV and store as character columns
icd_gem_10_to_9_raw <- read.table(
  file       = "data-raw/icd-gem-2018-convert-10-to-9.txt",
  header     = FALSE,
  col.names  = c("source", "target", "flags"),
  colClasses = c("character", "character", "character")
)

# check some basic assumptions about the GEM.  Takes about a minute to run.
#
# There is 1 scenario where some conversions are marked as approximate while
# others are exact for the ICD-9 to ICD-10 conversion, and 6 scenarios where
# some conversions are marked as approximate while others are exact for the
# other direction.
#
# The all of the other variables pass the checks.
gem_assumptions_9_to_10 <- check_gem_assumptions(icd_gem_9_to_10_raw)
gem_assumptions_10_to_9 <- check_gem_assumptions(icd_gem_10_to_9_raw)

# create the R data frames representing the GEMs.  Takes about a minute to run.
icd_gem_9_to_10 <- create_icd_map(icd_gem_9_to_10_raw)
icd_gem_10_to_9 <- create_icd_map(icd_gem_10_to_9_raw)

# save the RDS files to disk
save(icd_gem_9_to_10, file = "data/icd_gem_9_to_10.rda")
save(icd_gem_10_to_9, file = "data/icd_gem_10_to_9.rda")




# testing ----------------------------------------------------------------------

library(testthat)

icd_data <- data.frame(
  source = c("0010", "24951", "24951", "24951", "24951", "24951", "24951", "24951", "24951", "24951", "36570"),
  target = c("A000", "E08311", "E08319", "E0836", "E0839", "E0865", "E09311", "E09319", "E0936", "E0939", "NoDx"),
  flags  = c("00000", "10111", "10111", "10111", "10000", "10112", "10111", "10111", "10111", "10000", "11000")
)

icd_expanded <- data.frame(
  source           = c("0010", "24951", "24951", "24951", "24951", "24951", "24951", "24951", "24951", "24951", "36570"),
  target           = c("A000", "E08311", "E08319", "E0836", "E0839", "E0865", "E09311", "E09319", "E0936", "E0939", "NoDx"),
  approx           = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  no_map           = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
  combin           = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  scenario         = c("0", "1", "1", "1", "0", "1", "1", "1", "1", "0", "0"),
  choice_list      = c("0", "1", "1", "1", "0", "2", "1", "1", "1", "0", "0"),
  stringsAsFactors = FALSE
)

test_that("create_icd_map", {

  out <- create_icd_map(icd_data)

  actual_codes <- list(
    list("A000"),
    list(c("E0839", "E0939")),
    list(c("E08311", "E08319", "E0836", "E09311", "E09319", "E0936"), "E0865"),
    list()
  )

  # general structure
  expect_true(is.data.frame(out))
  expect_identical(names(out), c("source", "scenario", "type", "approx", "codes"))

  # column values
  expect_identical(out$source,   c("0010", "24951", "24951", "36570"))
  expect_identical(out$scenario, c("0", "0", "1", "0"))
  expect_identical(out$type,     c("simple", "simple", "combination", "no mapping"))
  expect_identical(out$approx,   c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(out$codes,    actual_codes)
})
