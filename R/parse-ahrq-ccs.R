# nocov start

#' Download AHRQ CCS ICD-9 definitions
#' @template ccs-single
#' @template dotdotdot
#' @keywords internal
#' @noRd
icd9_fetch_ahrq_ccs <- function(single = TRUE, ...) {
  assert_flag(single)
  ccs_base <- "https://www.hcup-us.ahrq.gov/toolssoftware/ccs"
  if (single) {
    .unzip_to_data_raw(
      url = paste0(ccs_base, "Single_Level_CCS_2015.zip"),
      file_name = "$dxref 2015.csv",
      ...
    )
  } else {
    .unzip_to_data_raw(
      url = paste0(ccs_base, "Multi_Level_CCS_2015.zip"),
      file_name = "ccs_multi_dx_tool_2015.csv",
      ...
    )
  }
}

#' Download AHRQ CCS ICD-10 definitions
#' @param version Default \code{2018.1}
#' @template dotdotdot
#' @keywords internal
#' @noRd
icd10_fetch_ahrq_ccs <- function(version = "2018.1", ...) {
  assert_character(version, pattern = "^20[0-9]{2}\\.[1-9]$")
  version <- gsub(".", "_", version, fixed = TRUE)
  # all information in one file, no need for single vs multi
  .unzip_to_data_raw(
    url = paste0(
      "https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs_dx_icd10cm_",
      version, ".zip"
    ),
    file_name = paste0("ccs_dx_icd10cm_", version, ".csv"),
    ...
  )
}

#' parse AHRQ CCS for mapping
#'
#' Data is downloaded from AHRQ website. ICD9 codes were frozen so no updates
#' are needed. CCS codes are available in a Multi level format and a Single
#' level format. Single level is most common for risk adjustment (Ex. CMS
#' Readmission metric)
#' @template ccs-single
#' @param save_data logical whether to save the result in the source tree.
#'   Defaults to \code{FALSE}.
#' @template offline
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' icd:::icd9_parse_ahrq_ccs(single = TRUE, offline = FALSE)
#' icd:::icd9_parse_ahrq_ccs(single = TRUE, offline = TRUE)
#' icd:::icd9_parse_ahrq_ccs(single = FALSE, offline = FALSE)
#' icd:::icd9_parse_ahrq_ccs(single = FALSE, offline = TRUE)
#' }
#' @keywords internal manip
#' @noRd
icd9_parse_ahrq_ccs <- function(single = TRUE,
                                save_data = FALSE,
                                offline = getOption("icd.data.offline")) {
  assert_flag(single)
  assert_flag(save_data)
  assert_flag(offline)
  ahrq_ccs <- icd9_fetch_ahrq_ccs(single = single, offline = offline)
  clean_icd9 <- function(x) as.short_diag(as.icd9(trimws(x)))
  resort_lvls <- function(x) {
    # Function to reorder numbers of CCS
    lvls_names <- names(x)
    lvls_has_empty <- any(lvls_names == " ")
    lvls <- lvls_names[lvls_names != " "]
    lvls <- strsplit(lvls, ".", fixed = TRUE)
    number_splits <- length(lvls[[1]])
    lvls <- matrix(
      as.numeric(unlist(lvls)),
      ncol = number_splits, byrow = TRUE,
      dimnames = list(rownames = lvls_names[lvls_names != " "])
    )
    # complicated call needed or order using all columns of matrix
    lvls <- lvls[do.call(order, as.data.frame(lvls)), ]
    # if only looking at lvl1, then this becomes a vector, not a matrix
    if (is.null(dim(lvls))) {
      lvls <- names(lvls)
    } else {
      lvls <- rownames(lvls)
    }
    if (lvls_has_empty) lvls <- c(lvls, " ")
    x[lvls]
  }
  rsrt <- function(x) comorbidity_map(resort_lvls(x))
  if (!single) {
    ahrq_df <- read.csv(ahrq_ccs$file_path,
      quote = "'\"",
      colClasses = "character"
    )
    lvl1 <- rsrt(tapply(
      ahrq_df[["ICD.9.CM.CODE"]],
      ahrq_df[["CCS.LVL.1"]],
      clean_icd9
    ))
    lvl2 <- rsrt(tapply(
      ahrq_df[["ICD.9.CM.CODE"]],
      ahrq_df[["CCS.LVL.2"]],
      clean_icd9
    ))
    lvl3 <- rsrt(tapply(
      ahrq_df[["ICD.9.CM.CODE"]],
      ahrq_df[["CCS.LVL.3"]],
      clean_icd9
    ))
    lvl4 <- rsrt(tapply(
      ahrq_df[["ICD.9.CM.CODE"]],
      ahrq_df[["CCS.LVL.4"]],
      clean_icd9
    ))
    icd9_map_multi_ccs <- list(
      lvl1 = lvl1,
      lvl2 = lvl2,
      lvl3 = lvl3,
      lvl4 = lvl4
    )
    make_labels <- function(lvl = 1) {
      values_col <- paste0("CCS.LVL.", lvl)
      label_col <- paste0("CCS.LVL.", lvl, ".LABEL")

      lkp_chr <- trimws(ahrq_df[[label_col]])
      names(lkp_chr) <- trimws(ahrq_df[[values_col]])
      lkp_chr <- lkp_chr[nchar(names(lkp_chr)) != 0]
      lkp_chr[!duplicated(names(lkp_chr))]
    }
    icd9_names_multi_ccs <- list(
      lvl1 = make_labels(1),
      lvl2 = make_labels(2),
      lvl3 = make_labels(3),
      lvl4 = make_labels(4)
    )
    if (save_data) {
      .save_in_data_dir(icd9_map_multi_ccs)
      .save_in_data_dir(icd9_names_multi_ccs)
    }
    out <- icd9_map_multi_ccs
  } else {
    ahrq_df <- read.csv(ahrq_ccs$file_path,
      quote = "'\"",
      colClasses = "character", skip = 1
    )


    icd9_names_single_ccs <- trimws(ahrq_df$CCS.CATEGORY.DESCRIPTION)
    names(icd9_names_single_ccs) <- trimws(ahrq_df$CCS.CATEGORY)
    # look for duplicated lkps because there may be different spellings
    # in the labels
    duplicated_rows <- duplicated(names(icd9_names_single_ccs))
    icd9_names_single_ccs <- icd9_names_single_ccs[!duplicated_rows]

    icd9_map_single_ccs <-
      rsrt(tapply(
        ahrq_df[["ICD.9.CM.CODE"]],
        trimws(ahrq_df$CCS.CATEGORY),
        clean_icd9
      ))
    if (save_data) {
      .save_in_data_dir(icd9_map_single_ccs)
      .save_in_data_dir("icd9_names_single_ccs")
    }
    out <- icd9_map_single_ccs
  }
  invisible(out)
}

#' parse AHRQ CCS for mapping - ICD10
#'
#' Data is downloaded from AHRQ website. ICD10 codes are continually being
#' updated so a parameter for \code{version} is provided. This parameter should
#' mimic those found in
#' \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs10.jsp#archive}.
#' These are in the format of \code{YYYY.1}, \code{YYYY.2} etc.
#' @param version string in format like "2018.1" where 1 is the version number
#'   as shown on the website
#' @param save_data logical whether to save the result in the source tree.
#'   Defaults to \code{FALSE}.
#' @template verbose
#' @template offline
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' # offline = FALSE
#' icd:::icd10_parse_ahrq_ccs(
#'   version = "2018.1",
#'   save_data = FALSE, offline = FALSE
#' )
#' icd:::icd10_parse_ahrq_ccs(
#'   version = "2018.1",
#'   save_data = FALSE, offline = TRUE
#' )
#' }
#' @keywords internal manip
#' @noRd
icd10_parse_ahrq_ccs <- function(version = "2018.1",
                                 save_data = FALSE,
                                 offline = TRUE,
                                 verbose = TRUE) {
  assert_character(version, pattern = "^20[0-9]{2}\\.[1-9]$")
  assert_flag(save_data)
  assert_flag(offline)
  ahrq_ccs <-
    icd10_fetch_ahrq_ccs(version = version, offline = offline)
  # simpler structure than icd9, all categories in one file
  ahrq_df <- read.csv(ahrq_ccs$file_path,
    quote = "'\"",
    colClasses = "character"
  )
  # rename columsn to make it easier later to create lookups
  names(ahrq_df) <- gsub(".DESCRIPTION", ".LABEL", names(ahrq_df))
  clean_icd10 <- function(x) as.icd10(trimws(x))
  resort_lvls <- function(x) {
    # Function to reorder numbers of CCS
    lvls_names <- names(x)
    lvls_has_empty <- any(lvls_names == " ")
    lvls <- lvls_names[lvls_names != " "]
    lvls <- strsplit(lvls, ".", fixed = TRUE)
    number_splits <- length(lvls[[1]])
    lvls <- matrix(as.numeric(unlist(lvls)),
      ncol = number_splits,
      byrow = TRUE,
      dimnames = list(rownames = lvls_names[lvls_names != " "])
    )
    # complicated call needed or order using all columns of matrix
    lvls <- lvls[do.call(order, as.data.frame(lvls)), ]
    # if only looking at lvl1, then this becomes a vector, not a matrix
    if (is.null(dim(lvls))) {
      lvls <- names(lvls)
    } else {
      lvls <- rownames(lvls)
    }
    if (lvls_has_empty) {
      lvls <- c(lvls, " ")
    }
    x[lvls]
  }
  ccs_lvl_map <- function(col_name)
    comorbidity_map(
      resort_lvls(
        tapply(
          ahrq_df[["ICD.10.CM.CODE"]],
          ahrq_df[[col_name]],
          clean_icd10
        )
      )
    )
  # list to define which columns match to which definition
  icd10_map_def <- list(
    single = "CCS.CATEGORY",
    lvl1 = "MULTI.CCS.LVL.1",
    lvl2 = "MULTI.CCS.LVL.2"
  )
  # Because data is in one file, only have one mapping file to save
  icd10_map_ccs <- lapply(icd10_map_def, ccs_lvl_map)
  ccs_lvl_name <- function(values_col) {
    label_col <- paste0(values_col, ".LABEL")

    lkp_chr <- trimws(ahrq_df[[label_col]])
    names(lkp_chr) <- trimws(ahrq_df[[values_col]])
    lkp_chr <- lkp_chr[nchar(names(lkp_chr)) != 0]
    lkp_chr[!duplicated(names(lkp_chr))]
  }
  icd10_names_ccs <- lapply(icd10_map_def, ccs_lvl_name)
  # nolint start
  icd10_map_ccs <- sapply(icd10_map_ccs,
    apply_over_icd10cm_vers,
    verbose = verbose,
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  # nolint end
  # not applying over WHO codes because CCS is a US-oriented classification.
  if (save_data) {
    .save_in_data_dir(icd10_map_ccs)
    .save_in_data_dir(icd10_names_ccs)
  }
  invisible(icd10_map_ccs)
}

# nocov end
