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
#' Note that if `[.icd9cm_pc` and `[.icd10cm_pc` methods are implemented, then
#' this code can be simplified quite a bit, because we can simply set the type
#' information of the source and target codes at the beginning, and the
#' information will stick around with the data throughout the transformations.
#'
#' @param icd_source_df A data frame with character columns \code{source},
#'   \code{target}, and \code{flags}, and such that each element of the
#'   \code{flags} column has exactly 5 characters.
#' @param mapping_direction A string specifying which type of mapping is to be
#'   performed.
#' @return A data frame with one row per source code scenario, and the columns
#'   \code{source}, \code{scenario}, \code{type}, \code{approx}, and
#'   \code{codes}.
#' @noRd
create_icd_map <- function(icd_source_df,
                           mapping_direction = c("ICD-9 to ICD-10",
                                                 "ICD-10 to ICD-9",
                                                 "ICD-9-PC to ICD-10-PC",
                                                 "ICD-10-PC to ICD-9-PC")) {

  stopifnot(
    is.data.frame(icd_source_df),
    setequal(names(icd_source_df), c("source", "target", "flags")),
    sapply(icd_source_df, is.character)
  )

  # determine the functions to use for translating short form codes to decimal
  # form codes.  This is needed because ICD-10 PCS doesn't have decimal form
  # codes, so we just use `identity` in that case.
  mdir <- match.arg(mapping_direction)
  source_to_decimal_fcn <- `if`(mdir == "ICD-10-PC to ICD-9-PC", identity, short_to_decimal)
  target_to_decimal_fcn <- `if`(mdir == "ICD-9-PC to ICD-10-PC", identity, short_to_decimal)

  # determine the functions to use for adding the correct classes to the source
  # and target codes
  source_type_fcn <- select_source_type_fcn(mapping_direction)
  target_type_fcn <- select_target_type_fcn(mapping_direction)

  # expand the flags field into multiple columns, and split the data frame into
  # a list of one data frame per scenario
  icd_expanded_df <- create_icd_map_expanded_df(icd_source_df)
  scenario_list <- create_icd_map_scenario_list(icd_expanded_df)

  # create a single-row data frame for each scenario, and combine all of the
  # rows into a single data frame
  out <- lapply(
    X                     = scenario_list,
    FUN                   = create_icd_map_single_scenario,
    target_type_fcn       = target_type_fcn,
    target_to_decimal_fcn = target_to_decimal_fcn
  )
  out <- do.call(rbind, out)

  # add the appropriate class information to the source data, and return the
  # reordered data frame
  out$source_short <- source_type_fcn(unclass(as.short_diag(out$source_short)))
  out$source_decimal <- source_type_fcn(unclass(source_to_decimal_fcn(out$source_short)))
  out[, c("source_short", "source_decimal", "scenario", "type", "approx", "codes_short", "codes_decimal")]
}


#' Construct a Function to Add ICD Class and Attributes Information to the Source Codes
#'
#' @param mapping_direction A string specifying which type of mapping is to be
#'   performed.
#' @return A function that is to be used to encode a character vector with the
#'   ICD class information.
#' @noRd
select_source_type_fcn <- function(mapping_direction = c("ICD-9 to ICD-10",
                                                         "ICD-10 to ICD-9",
                                                         "ICD-9-PC to ICD-10-PC",
                                                         "ICD-10-PC to ICD-9-PC")) {
  switch(
    EXPR                    = match.arg(mapping_direction),
    "ICD-9 to ICD-10"       = as.icd9cm,
    "ICD-10 to ICD-9"       = as.icd10cm,
    "ICD-9-PC to ICD-10-PC" = as.icd9cm_pc,
    "ICD-10-PC to ICD-9-PC" = as.icd10cm_pc
  )
}


#' Construct a Function to Add ICD Class and Attributes Information to the Target Codes
#'
#' @param mapping_direction A string specifying which type of mapping is to be
#'   performed.
#' @return A function that is to be used to encode a character vector with the
#'   ICD class information.
#' @noRd
select_target_type_fcn <- function(mapping_direction = c("ICD-9 to ICD-10",
                                                         "ICD-10 to ICD-9",
                                                         "ICD-9-PC to ICD-10-PC",
                                                         "ICD-10-PC to ICD-9-PC")) {
  switch(
    EXPR                    = match.arg(mapping_direction),
    "ICD-9 to ICD-10"       = as.icd10cm,
    "ICD-10 to ICD-9"       = as.icd9cm,
    "ICD-9-PC to ICD-10-PC" = as.icd10cm_pc,
    "ICD-10-PC to ICD-9-PC" = as.icd9cm_pc
  )
}


#' Replace the Flag Column With Multiple Columns
#'
#' Create a data frame with the same rows and columns as before, but with the
#' flag column split out into multiple columns.
#'
#' @param icd_source_df A data frame with columns \code{source}, \code{target},
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
create_icd_map_single_scenario <- function(scenario_df,
                                           target_type_fcn,
                                           target_to_decimal_fcn) {

  create_type_str <- function(codes_short) {
    if (length(codes_short) == 0L) {
      "no mapping"
    }
    else if (length(codes_short) == 1L) {
      "simple"
    }
    else {
      "combination"
    }
  }

  # create a list of character vectors, with each vector the target codes
  # corresponding to a single choice.  The scenarios without a mapping are
  # treated as having 0 target codes.
  if (any(scenario_df$no_map)) {
    stopifnot(nrow(scenario_df) == 1L, scenario_df$target %in% c("NoDx", "NoPCS"))
    codes_short <- list()
  }
  else {
    target <- scenario_df$target
    codes_short_chr <- split(target, scenario_df$choice_list)
    codes_short <- lapply(codes_short_chr, function(x) {
      target_type_fcn(unclass(as.short_diag(x)))
    })
  }
  codes_decimal <- lapply(codes_short, function(x) {
    target_type_fcn(unclass(target_to_decimal_fcn(x)))
  })

  # create 1-row data frame and return
  out <- data.frame(
    source_short     = scenario_df$source[1L],
    scenario         = scenario_df$scenario[1L],
    type             = create_type_str(codes_short),
    approx           = all(scenario_df$approx),
    stringsAsFactors = FALSE
  )
  out$codes_short = list(codes_short)
  out$codes_decimal = list(codes_decimal)
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
    ((! df$no_map) | (df$no_map & (df$target %in% c("NoDx", "NoPCS"))))
  }

  check_within_scenario_const <- function(df) {
    data.frame(
      source_short     = df$source[1L],
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


# read the raw conversion tables -----------------------------------------------

read_fcn <- function(file) {
  read.table(
    file       = file,
    header     = FALSE,
    col.names  = c("source", "target", "flags"),
    colClasses = c("character", "character", "character")
  )
}

# parse the GEM CSVs and store as character columns
icd_gem_9_to_10_raw <- read_fcn("data-raw/icd-gem-2018-convert-9-to-10.txt")
icd_gem_10_to_9_raw <- read_fcn("data-raw/icd-gem-2018-convert-10-to-9.txt")
icd_gem_9pc_to_10pc_raw <- read_fcn("data-raw/icd-gem-2016-convert-9pc-to-10pc.txt")
icd_gem_10pc_to_9pc_raw <- read_fcn("data-raw/icd-gem-2016-convert-10pc-to-9pc.txt")


# check some basic assumptions about the GEMs ----------------------------------

checks_summary <- function(gem_assumption_checks) {
  lapply(gem_assumption_checks[, -(1L:2L)], function(x) sum(! x))
}

# Takes about 2 minutes to run.  See the comments for `check_gem_assumptions`
# regarding what the checks entail.

gem_assumptions <- list(
  "9_to_10"     = check_gem_assumptions(icd_gem_9_to_10_raw),
  "10_to_9"     = check_gem_assumptions(icd_gem_10_to_9_raw),
  "9pc_to_10pc" = check_gem_assumptions(icd_gem_9pc_to_10pc_raw),
  "10pc_to_9pc" = check_gem_assumptions(icd_gem_10pc_to_9pc_raw)
)
t(sapply(gem_assumptions, checks_summary))


# create tables ----------------------------------------------------------------

# create the R data frames representing the GEMs.  Takes about a minute to run.
icd_gem_9_to_10 <- create_icd_map(icd_gem_9_to_10_raw, "ICD-9 to ICD-10")
icd_gem_10_to_9 <- create_icd_map(icd_gem_10_to_9_raw, "ICD-10 to ICD-9")
icd_gem_9pc_to_10pc <- create_icd_map(icd_gem_9pc_to_10pc_raw, "ICD-9-PC to ICD-10-PC")
icd_gem_10pc_to_9pc <- create_icd_map(icd_gem_10pc_to_9pc_raw, "ICD-10-PC to ICD-9-PC")

# save the RDS files to disk
save(icd_gem_9_to_10, file = "data/icd_gem_9_to_10.rda")
save(icd_gem_10_to_9, file = "data/icd_gem_10_to_9.rda")
save(icd_gem_9pc_to_10pc, file = "data/icd_gem_9pc_to_10pc.rda")
save(icd_gem_10pc_to_9pc, file = "data/icd_gem_10pc_to_9pc.rda")


# testing ----------------------------------------------------------------------

library(testthat)

icd_data <- data.frame(
  source           = c("0010", "24951", "24951", "24951", "24951", "24951", "24951", "24951", "24951", "24951", "36570"),
  target           = c("A000", "E08311", "E08319", "E0836", "E0839", "E0865", "E09311", "E09319", "E0936", "E0939", "NoDx"),
  flags            = c("00000", "10111", "10111", "10111", "10000", "10112", "10111", "10111", "10111", "10000", "11000"),
  stringsAsFactors = FALSE
)

test_that("create_icd_map", {

  out <- create_icd_map(icd_data, "ICD-9 to ICD-10")

  actual_codes_short_chr <- list(
    list(`0` = "A000"),
    list(`0` = c("E0839", "E0939")),
    list(`1` = c("E08311", "E08319", "E0836", "E09311", "E09319", "E0936"), `2` = "E0865"),
    list()
  )
  actual_codes_short <- lapply(actual_codes_short_chr, function(x) {
    lapply(x, function(y) {
      as.icd10cm(as.short_diag(y))
    })
  })
  actual_codes_decimal <- lapply(actual_codes_short, function(x) {
    lapply(x, short_to_decimal)
  })

  # general structure
  expect_true(is.data.frame(out))
  expect_identical(names(out), c("source_short", "source_decimal", "scenario",
                                 "type", "approx", "codes_short",
                                 "codes_decimal"))

  # column values
  expect_identical(out$source_short,   as.short_diag(as.icd9cm(c("0010", "24951", "24951", "36570"))))
  expect_identical(out$source_decimal, as.decimal_diag(as.icd9cm(c("001.0", "249.51", "249.51", "365.70"))))
  expect_identical(out$scenario,       c("0", "0", "1", "0"))
  expect_identical(out$type,           c("simple", "simple", "combination", "no mapping"))
  expect_identical(out$approx,         c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(out$codes_short,    actual_codes_short)
  expect_identical(out$codes_decimal,  actual_codes_decimal)
})

icd_pc_data <- data.frame(
  source           = c("0053", "0053", "0053", "0053", "0053", "0053", "0053", "0053", "0211", "0211", "0211"),
  target           = c("0JH607Z", "0JH607Z", "0JH637Z", "0JH637Z", "0JH807Z", "0JH837Z", "0JPT0PZ", "0JPT3PZ", "00Q20ZZ", "00Q23ZZ", "00Q24ZZ"),
  flags            = c("10000", "10111", "10000", "10111", "10000", "10000", "10112", "10112", "10000", "10000", "10000"),
  stringsAsFactors = FALSE
)

test_that("create_icd_map ICD-9 PC to ICD-10 PC", {

  out <- create_icd_map(icd_pc_data, "ICD-9-PC to ICD-10-PC")

  actual_codes_short_chr <- list(
    list(`0` = c("0JH607Z", "0JH637Z", "0JH807Z", "0JH837Z")),
    list(`1` = c("0JH607Z", "0JH637Z"), `2` = c("0JPT0PZ", "0JPT3PZ")),
    list(`0` = c("00Q20ZZ", "00Q23ZZ", "00Q24ZZ"))
  )
  actual_codes_short <- lapply(actual_codes_short_chr, function(x) {
    lapply(x, function(y) {
      as.icd10cm_pc(as.short_diag(y))
    })
  })
  actual_codes_decimal <- actual_codes_short

  # general structure
  expect_true(is.data.frame(out))
  expect_identical(names(out), c("source_short", "source_decimal", "scenario",
                                 "type", "approx", "codes_short",
                                 "codes_decimal"))

  # column values
  expect_identical(out$source_short,   as.short_diag(as.icd9cm_pc(c("0053", "0053", "0211"))))
  expect_identical(out$source_decimal, as.decimal_diag(as.icd9cm_pc(c("00.53", "00.53", "02.11"))))
  expect_identical(out$scenario,       c("0", "1", "0"))
  expect_identical(out$type,           c("simple", "combination", "simple"))
  expect_identical(out$approx,         c(TRUE, TRUE, TRUE))
  expect_identical(out$codes_short,    actual_codes_short)
  expect_identical(out$codes_decimal,  actual_codes_decimal)
})
