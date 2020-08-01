#' Get all ICD-10-CM codes
#'
#' Gets all ICD-10-CM codes from an archive on the CDC web site.
#'
#' The factor generation uses \code{sort} which is locale dependent. This meant
#' a lot of time debugging a problem when white space was ignored for sorting on
#' some platforms, but not others. For vectors of codes with an ICD class of
#' some types, (e.g. ICD-9 and ICD-10-CM), a specific sort function and
#' operators are defined.
#' @param save_pkg_data Save specific year diagnostic codes in the package data
#'   directory for CRAN package. All data retrieved is saved in the user cache
#'   regardless.
#' @source \url{https://www.cms.gov/Medicare/Coding/ICD10} also available from
#'   \url{http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip}.
#'
#' @references
#' \href{https://www.cms.gov/Medicare/Coding/ICD10/downloads/icd-10quickrefer.pdf}{CMS
#' ICD-10 Quick Reference}
#' \href{https://www.cdc.gov/nchs/icd/icd10cm.htm#FY\%202019\%20release\%20of\%20ICD-10-CM}{CDC
#' copy of ICD-10-CM for 2019}
#' @keywords internal datagen
#' @noRd
.parse_icd10cm_all <- function(save_pkg_data = FALSE, ...) {
  .msg("Parsing all ICD-10-CM diagnostic codes.")
  yrs <- names(.icd10cm_sources)
  out <- lapply(
    yrs,
    .parse_icd10cm_year,
    ...
  )
  names(out) <- yrs
  invisible(names(out))
}

.parse_icd10cm_year <- function(year,
                                save_pkg_data = FALSE,
                                dx = TRUE,
                                ...) {
  stopifnot(is.numeric(year) || is.character(year))
  year <- as.character(year)
  stopifnot(as.character(year) %in% names(.icd10cm_sources))
  f_info <- .dl_icd10cm_year(year = year, dx = dx)
  if (is.null(f_info)) {
    .absent_action_switch(
      paste("No path to downloaded data. Options are: "),
      paste(names(.show_options()), unname(.show_options()),
        collapse = ",",
        sep = ":"
      )
    )
    return()
  }
  .msg("Working on parsing ICD-10-CM year: ", year)
  # readLines may muck up encoding, resulting in weird factor order generation
  # later?
  x <- readLines(con = f_info$file_path, encoding = "ASCII")
  .msg("Got flat file for year: ", year)
  stopifnot(all(Encoding(x) == "unknown"))
  dat <- data.frame(
    # id = substr(x, 1, 5),
    code = trimws(substr(x, 7, 13)),
    billable = trimws(substr(x, 14, 15)) == "1",
    short_desc = trimws(substr(x, 16, 76)),
    long_desc = trimws(substr(x, 77, stop = 1e5)),
    stringsAsFactors = FALSE
  )
  dat[["code"]] <-
    icd::as.short_diag(
      if (dx) {
        icd::as.icd10cm(dat[["code"]])
      } else {
        icd::as.icd10cm_pc(dat[["code"]])
      }
    )
  dat[["three_digit"]] <- factor_nosort(get_major(dat[["code"]]))
  # here we must re-factor so we don't have un-used levels in major
  dat[["major"]] <- factor_nosort(
    merge(
      x = dat["three_digit"],
      y = dat[c("code", "short_desc")],
      by.x = "three_digit", by.y = "code",
      all.x = TRUE
    )[["short_desc"]]
  )
  dat[["major"]] <- icd::as.short_diag(icd::as.icd10cm(dat[["major"]]))
  if (dx) {
    .msg("Generating sub-chapter lookup for year: ", year)
    sc_lookup <- .icd10_generate_subchap_lookup()
    mismatch_sub_chap <-
      dat$three_digit[which(dat$three_digit %nin% sc_lookup$sc_major)]
    if (length(mismatch_sub_chap)) {
      message(
        "mismatched sub-chapters: ",
        paste(mismatch_sub_chap, collapse = ", ")
      )
    }
    dat[["sub_chapter"]] <-
      merge(
        x = dat["three_digit"],
        y = sc_lookup,
        by.x = "three_digit",
        by.y = "sc_major",
        all.x = TRUE
      )[["sc_desc"]]
    .msg("Generating chap lookup for year: ", year)
    chap_lookup <- .icd10_generate_chap_lookup()
    dat[["chapter"]] <-
      merge(
        dat["three_digit"], chap_lookup,
        by.x = "three_digit", by.y = "chap_major",
        all.x = TRUE
      )[["chap_desc"]]
  }
  class(dat$code) <- c("icd10cm", "icd10", "character")
  dat$three_digit <- factor_sorted_levels(as.icd10cm(dat$three_digit))
  class(dat$three_digit) <- c("icd10cm", "icd10", "factor")
  .msg("Correcting order of ", nrow(dat), " codes")
  dat <- dat[order.icd10cm(dat$code), ]
  row.names(dat) <- NULL
  .msg("Saving in cache dir")
  .save_in_cache(
    var_name = .get_icd10cm_name(year = year, dx = dx),
    x = dat
  )
  if (save_pkg_data && year == "2019") {
    if (.verbose()) {
      message("Saving in package data: ", .get_icd10cm_name(year, dx))
    }
    .save_in_data_dir(
      var_name = .get_icd10cm_name(year, dx),
      x = dat,
      compress = "xz"
    )
  }
  invisible(dat)
}
