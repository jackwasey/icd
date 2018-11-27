# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

# nocov start
# nolint start
#' get all ICD-10-CM codes
#'
#' Gets all ICD-10-CM codes from an archive on the CDC web site at Initially,
#' this just grabs 2016.
#'
#' The factor generation uses \code{sort.default} which is locale dependent.
#' This meant a lot of time debugging a problem when white space was ignored for
#' sorting on some platforms, but not others.
#' @source
#'   \url{http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip}.
#' @references
#'   https://www.cms.gov/Medicare/Coding/ICD10/downloads/icd-10quickrefer.pdf
#' @keywords internal
icd10cm_get_all_defined <- function(save_data = FALSE, offline = TRUE) {
  # nolint stop
  f_info <- icd10cm_get_flat_file(offline = offline)
  stopifnot(!is.null(f_info))
  # readLines may muck up encoding, resulting in weird factor order generation
  # later?
  x <- readLines(con = f_info$file_path, encoding = "ASCII")
  stopifnot(all(Encoding(x) == "unknown"))
  # Beware: stringr::str_trim may do some encoding tricks which result in
  # different factor order on different platforms. Seems to affect "major" which
  # comes from "short_desc"
  icd10cm2016 <- data.frame(
    #id = substr(x, 1, 5),
    code = trim(substr(x, 7, 13)),
    billable = trim(substr(x, 14, 15)) == "1",
    short_desc = trim(substr(x, 16, 76)),
    long_desc = trim(substr(x, 77, stop = 1e5)),
    stringsAsFactors = FALSE
  )
  icd10cm2016[["code"]] <- as.short_diag(as.icd10cm(icd10cm2016[["code"]]))
  icd10cm2016[["three_digit"]] <-
    factor_nosort(get_major(icd10cm2016[["code"]]))
  # here we must re-factor so we don't have un-used levels in major
  icd10cm2016[["major"]] <- factor_nosort(
    merge(x = icd10cm2016["three_digit"],
          y = icd10cm2016[c("code", "short_desc")],
          by.x = "three_digit", by.y = "code",
          all.x = TRUE)[["short_desc"]]
  )
  # can't use expand_range_major here for ICD-10-CM, because it would use
  # the output of this function (and it can't just do numeric ranges because
  # there are some non-numeric characters scattered around)
  sc_lookup <- icd10_generate_subchap_lookup()
  icd10cm2016[["sub_chapter"]] <-
    merge(x = icd10cm2016["three_digit"], y = sc_lookup,
          by.x = "three_digit", by.y = "sc_major", all.x = TRUE)[["sc_desc"]]
  chap_lookup <- icd10_generate_chap_lookup()
  icd10cm2016[["chapter"]] <-
    merge(icd10cm2016["three_digit"], chap_lookup,
          by.x = "three_digit", by.y = "chap_major", all.x = TRUE)[["chap_desc"]]
  if (save_data)
    save_in_data_dir(icd10cm2016)
  invisible(icd10cm2016)
}

icd10_generate_subchap_lookup <- function(lk_majors, verbose = FALSE) {
  lk_majors <- unique(icd10cm2016[["three_digit"]])
  sc_lookup <- data.frame(major = NULL, desc = NULL)
  for (scn in names(icd10_sub_chapters)) {
    sc <- icd10_sub_chapters[[scn]]
    si <- grep(sc["start"], lk_majors)
    se <- grep(sc["end"], lk_majors)
    sc_majors <- lk_majors[si:se]
    if (verbose)
      message("start = ", sc["start"], ", end = ", sc[["end"]],
              ", si = ", si, ", se = ", se)
    sc_lookup <- rbind(
      sc_lookup,
      data.frame(sc_major = sc_majors, sc_desc = scn)
    )
  }
  sc_lookup
}

icd10_generate_chap_lookup <- function(lk_majors) {
  lk_majors <- unique(icd10cm2016[["three_digit"]])
  chap_lookup <- data.frame(major = NULL, desc = NULL)
  for (chap_n in names(icd10_chapters)) {
    chap <- icd10_chapters[[chap_n]]
    # fix a 2016 error in the CMS XML definitions
    if (chap["end"] == "Y08")
      chap["end"] <- "Y09"
    si <- grep(chap["start"], lk_majors)
    se <- grep(chap["end"], lk_majors)
    chap_lookup <- rbind(
      chap_lookup,
      data.frame(chap_major = lk_majors[si:se], chap_desc = chap_n)
    )
  }
  chap_lookup
}

icd10_parse_ahrq_pcs <- function(save_data = TRUE) {
  f <- unzip_to_data_raw(
    url = "https://www.hcup-us.ahrq.gov/toolssoftware/procedureicd10/pc_icd10pcs_2018_1.zip",
    file_name = "pc_icd10pcs_2018.csv", offline = !save_data)
  dat <- read.csv(file = f$file_path, skip = 1, stringsAsFactors = FALSE,
                  colClasses = "character", encoding = "latin1")
  names(dat) <- c("code", "desc", "class_number", "class")
  dat$class <- factor(dat$class,
                      levels = c("Minor Diagnostic", "Minor Therapeutic",
                                 "Major Diagnostic", "Major Therapeutic"))
  dat$class_number <- NULL
  dat$code <- gsub(dat$code, pattern = "'", replacement = "")
  icd10_pcs <- list("2018" = dat[c("code", "desc")])
  icd10_map_ahrq_pcs <- split(dat$code, dat$class)
  if (save_data) {
    save_in_data_dir(icd10_pcs)
    save_in_data_dir(icd10_map_ahrq_pcs)
  }
}

icd10_parse_cms_pcs_all <- function(save_data = TRUE) {
  for (year in names(icd::icd10_sources)) {
    var_name <- paste0("icd10_pcs_", year)
    assign(var_name, icd10_parse_cms_pcs_year(year))
    save_in_data_dir(var_name)
  }
}

icd10_parse_cms_pcs_year <- function(year = "2018") {
  pcs_file <- icd::icd10_sources[[year]][["pcs_flat"]]
  pcs_path <- file.path(get_raw_data_dir(), pcs_file)
  read.fwf(pcs_path, c(5, 8, 2, 62, 120), header = FALSE,
           col.names = c("count", "pcs", "billable", "short_desc", "long_desc"))
}
# nocov end
