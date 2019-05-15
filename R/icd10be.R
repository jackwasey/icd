.icd10be_site <- "https://www.health.belgium.be"
.icd10be_url_path <- "sites/default/files/uploads/fields/fpshealth_theme_file"

.dl_icd10be2017 <- function(dx = TRUE,
                            must_work = FALSE,
                            ...) {
  site_file_2017 <-
    "fy2017_reflist_icd-10-be.xlsx_last_updatet_28-07-2017_1.xlsx"
  fnp <- .download_to_data_raw(
    paste(.icd10be_site,
      .icd10be_url_path,
      site_file_2017,
      sep = "/"
    ),
    dl_msg = "Downloading icd10be2017 data",
    ...
  )
  if (must_work && is.null(fnp)) stop("Cannot download ICD-10-BE 2017 raw data")
  fnp
}

.dl_icd10be2014 <- function(dx = TRUE,
                            ...) {
  if (.offline() && !.interact()) {
    msg <- "Offline and not interactive, so not downloading Belgian data."
    .absent_action_switch(msg)
    return(NULL)
  }
  site_file <- "fy2014_reflist_icd-10-be.xlsx"
  .msg("Downloading or getting cached icd10be2014 data")
  .download_to_data_raw(
    paste(.icd10be_site,
      .icd10be_url_path,
      site_file,
      sep = "/"
    ),
    dl_msg = "Downloading icd10be2014 data",
    ...
  )
}

.dl_icd10be2017_pc <- function(...) {
  .dl_icd10be2017(dx = FALSE, ...)[[2]]
}

.dl_icd10be2014_pc <- function(...) {
  .dl_icd10be2014(dx = FALSE, ...)[[2]]
}

#' Get French and Dutch translations of ICD-10-CM for Beglian coding
#'
#' There are public domain publications for 2014 and 2017. Not all items are
#' translated from English. Between 2014 and 2017, ICDLST and ICDLSTR fields 13
#' and 14 were removed.
#' @references
# nolint start
#' \url{https://www.health.belgium.be/fr/sante/organisation-des-soins-de-sante/hopitaux/systemes-denregistrement/icd-10-be}
#' \url{https://www.health.belgium.be/sites/default/files/uploads/fields/fpshealth_theme_file/fy2017_reflist_icd-10-be.xlsx_last_updatet_28-07-2017_1.xlsx}
# nolint end
#' \url{https://www.health.belgium.be/fr/fy2014reflisticd-10-bexlsx}
#' @param ... passed to \code{.download_to_data_raw}, e.g., \code{offline =
#'   FALSE}.
#' @seealso \code{link{parse_icd10be2014_be}}
#' @keywords internal
#' @noRd
.parse_icd10be2017 <- function(dx = TRUE, ...) {
  # MS Excel sheet with French English and Dutch translations of ICD-10-CM.
  # Currently all the codes are identical to ICD-10-CM US version.
  sheet_2017 <- "FY2017"
  fnp <- .dl_icd10be2017(...)
  if (is.null(fnp)) {
    return()
  }
  raw_dat <- readxl::read_xlsx(fnp$file_path,
    sheet = sheet_2017,
    col_names = TRUE,
    guess_max = 1e6,
    progress = FALSE
  )
  raw_dat <- raw_dat[c(
    "ICDCODE",
    # O for procedure, D for diagnostic
    "ICDDORO",
    # this if flag for NOT leaf node, * or blank.
    "ICDPREC",
    # FLAG AGE
    # A=Adult 15-124y;
    # M=Maternity 12-55y;
    # N=Newborn and Neonates 0y;
    # P=Pediatric 0-17y)
    "ICDTXTFR",
    "ICDTXTNL",
    "ICDTXTEN",
    "SHORT_TXTFR",
    "SHORT_TXTNL",
    "SHORT_TXTEN"
  )]
  raw_dat[["ICDPREC"]] <- raw_dat[["ICDPREC"]] != "*" || is.na(raw_dat[["ICDPREC"]])
  icd10be2017 <- as.data.frame(raw_dat[raw_dat$ICDDORO == "D", -2])
  icd10be2017_pc <- as.data.frame(raw_dat[raw_dat$ICDDORO == "O", -2])
  names <- c(
    "code",
    "leaf",
    "long_desc_fr",
    "long_desc_nl",
    "long_desc_en",
    "short_desc_fr",
    "short_desc_nl",
    "short_desc_en"
  )
  names(icd10be2017) <- names
  names(icd10be2017_pc) <- names
  icd10be2017_pc$not_poa <- NULL
  icd10be2017$not_poa <- !is.na(icd10be2017$not_poa) && icd10be2017 == "Y"
  icd10be2017 <- icd10be2017[order.icd10be(icd10be2017$code), ]
  icd10be2017_pc <- icd10be2017_pc[order(icd10be2017_pc$code), ]
  class(icd10be2017$code) <- c("icd10be", "icd10", "character")
  class(icd10be2017_pc$code) <- c("icd10be_pc", "character")
  row.names(icd10be2017) <- NULL
  row.names(icd10be2017_pc) <- NULL
  .save_in_cache(icd10be2017)
  .save_in_cache(icd10be2017_pc)
  if (dx) {
    invisible(icd10be2017)
  } else {
    invisible(icd10be2017_pc)
  }
}

#' Get 2014 French and Dutch translations of ICD-10-CM for Beglian coding
#' @references
# nolint start
#'   \url{https://www.health.belgium.be/fr/sante/organisation-des-soins-de-sante/hopitaux/systemes-denregistrement/icd-10-be}
# nolint end
#'    \url{https://www.health.belgium.be/fr/fy2014reflisticd-10-bexlsx}
#' @param ... passed to \code{.download_to_data_raw}, e.g., \code{offline =
#'   FALSE}.
#' @seealso \code{link{parse_icd10be2014_be}}
#' @keywords internal
#' @noRd
.parse_icd10be2014 <- function(dx = TRUE, ...) {
  # MS Excel sheet with French English and Dutch translations of ICD-10-CM.
  fnp <- .dl_icd10be2014(...)
  if (is.null(fnp)) {
    return()
  }
  raw_dat <- readxl::read_xlsx(fnp$file_path,
    sheet = "FY2014_ICD10BE",
    col_names = TRUE,
    guess_max = 1e6,
    progress = FALSE
  )
  raw_dat <- raw_dat[c(
    "ICDCODE",
    "ICDDORO", # O for procedure, D for diagnostic
    "ICDPREC", # this if flag for NOT leaf node
    "ICDFLSEX", # M/F specific, or empty for either
    # FLAG AGE
    # A=Adult 15-124y;
    # M=Maternity 12-55y;
    # N=Newborn and Neonates 0y;
    # P=Pediatric 0-17y)
    "ICDFLAGE",
    # blank for procedure codes, Y or empty for diagnostic codes
    "ICDNOPOA",
    "ICDTXTFR",
    "ICDTXTNL",
    "ICDTXTEN",
    "SHORT_TXTFR",
    "SHORT_TXTNL",
    "SHORT_TXTEN"
  )]
  # ICDPREC indicates non-leaf node
  raw_dat[["ICDPREC"]] <- raw_dat[["ICDPREC"]] != "*" || is.na(raw_dat[["ICDPREC"]])
  icd10be2014 <- as.data.frame(raw_dat[raw_dat$ICDDORO == "D", -2])
  icd10be2014_pc <- as.data.frame(raw_dat[raw_dat$ICDDORO == "O", -2])
  names <- c(
    "code",
    "leaf",
    "sex",
    "age_group",
    "not_poa",
    "long_desc_fr",
    "long_desc_nl",
    "long_desc_en",
    "short_desc_fr",
    "short_desc_nl",
    "short_desc_en"
  )
  names(icd10be2014) <- names
  names(icd10be2014_pc) <- names
  icd10be2014_pc$not_poa <- NULL
  icd10be2014$not_poa <- !is.na(icd10be2014$not_poa) & icd10be2014$not_poa == "Y"
  icd10be2014 <- icd10be2014[order.icd10be(icd10be2014$code), ]
  icd10be2014_pc <- icd10be2014_pc[order(icd10be2014_pc$code), ]
  class(icd10be2014$code) <- c("icd10be", "icd10", "character")
  class(icd10be2014_pc$code) <- c("icd10be_pc", "character")
  row.names(icd10be2014) <- NULL
  row.names(icd10be2014_pc) <- NULL
  .save_in_cache(icd10be2014)
  .save_in_cache(icd10be2014_pc)
  if (dx) {
    invisible(icd10be2014)
  } else {
    invisible(icd10be2014_pc)
  }
}

.parse_icd10be2017_pc <- function(...) {
  .parse_icd10be2017(dx = FALSE, ...)
}

.parse_icd10be2014_pc <- function(...) {
  .parse_icd10be2014(dx = FALSE, ...)
}
