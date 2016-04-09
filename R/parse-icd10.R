# Copyright (C) 2014 - 2016  Jack O. Wasey
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

#' Get sub-chapters from the 2016 XML for ICD-10-CM
#'
#' This is not quite a super-set of ICD-10 sub-chapters: many more codes than
#' ICD-10, but some are abbreviated, notably HIV.
#'
#' This is complicated by the XML document specifying more hierarchical
#' levels,e.g. \code{C00-C96}, \code{C00-75} are both specified within the
#' chapter Neoplasms (\code{C00-D49}). A way of determining whether there are
#' extra levels would be to check the XML tree depth for a member of each
#' putative sub-chapter.
#' @template save_data
#' @keywords internal
icd10cm_extract_sub_chapters <- function(save_data = FALSE) {
  assert_flag(save_data)
  loadNamespace("xml2")
  f_info <- icd10cm_get_xml_file()
  j <- xml2::read_xml(f_info$file_path)

  # using magrittr::equals and extract because I don't want to import them. See
  # \code{icd-package.R} for what is imported. No harm in being explicit, since
  # :: will do an implicit requireNamespace.
  j  %>% xml2::xml_children() %>%
    xml2::xml_name() %>%
    magrittr::equals("chapter" ) -> chapter_indices
  # could do xpath, but harder to loop
  j  %>% xml2::xml_children() %>% magrittr::extract(chapter_indices) -> chaps

  icd10_sub_chapters <- list()
  for (chap in chaps) {
    chap  %>% xml2::xml_children() -> c_kids
    c_kids %>% xml2::xml_name() %>% magrittr::equals("section") -> subchap_indices
    c_kids %>% magrittr::extract(subchap_indices) -> subchaps

    for (subchap in subchaps) {
      subchap  %>%
        xml2::xml_children()  %>%
        magrittr::extract(1) %>%
        xml2::xml_text() %>%
        chapter_to_desc_range.icd10 -> new_sub_chap_range

      # should only match one at a time
      stopifnot(length(new_sub_chap_range) == 1)

      # check that this is a real sub-chapter, not an extra range defined in the
      # XML, e.g. C00-C96 is an empty range declaration for some neoplasms.

      ndiags <- length(xml2::xml_find_all(subchap, "diag"))
      if (ndiags == 0) {
        message("skipping empty range definition for ", new_sub_chap_range)
        next
      }

      # there is a defined Y09 in both ICD-10 WHO and CM, but the range is
      # incorrectly specified (in 2016 version of XML, at least)
      if (new_sub_chap_range[[1]]["end"] == "Y08")
        new_sub_chap_range[[1]]["end"] <- "Y09"

      icd10_sub_chapters <- append(icd10_sub_chapters, new_sub_chap_range)
    } #subchaps
  } #chapters
  if (save_data)
    save_in_data_dir(icd10_sub_chapters)
  invisible(icd10_sub_chapters)
}

#' get all ICD-10-CM codes
#'
#' Gets all ICD-10-CM codes from an archive on the CDC web site at Initially,
#' this just grabs 2016.
#'
#' The factor generation uses \code{sort.default} which is locale dependent.
#' This meant a lot of time debugging a problem when white space was ignored for
#' sorting on some platforms, but not others (e.g. Travis and Wercker).
#' @source
#'   \url{http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip}.
#'
#' @references
#'   https://www.cms.gov/Medicare/Coding/ICD10/downloads/icd-10quickrefer.pdf
#' @keywords internal
icd10cm_get_all_defined <- function(save_data = FALSE) {

  f_info <- icd10cm_get_flat_file()

  # readLines may muck up encoding, resulting in weird factor order generation later?
  x <- readLines(con = f_info$file_path, encoding = "ASCII")
  stopifnot(all(Encoding(x) == "unknown"))

  # str_trim may do some encoding tricks which result in different factor order
  # on different platforms. Seems to affect "major" which comes from "short_desc"

  icd10cm2016 <- data.frame(
    #id = substr(x, 1, 5),
    code = str_trim(substr(x, 7, 13)),
    billable = str_trim(substr(x, 14, 15)) == "1",
    short_desc = str_trim(substr(x, 16, 76)),
    long_desc = str_trim(substr(x, 77, stop = 1e5)),
    stringsAsFactors = FALSE
  )

  icd10cm2016[["code"]] %<>% as.icd10cm %>% as.icd_short_diag
  icd10cm2016[["code"]] %>% icd_get_major %>% factor_nosort -> icd10cm2016[["three_digit"]]

  # here we must re-factor so we don't have un-used levels in major
  merge(x = icd10cm2016["three_digit"],
        y = icd10cm2016[c("code", "short_desc")],
        by.x = "three_digit", by.y = "code",
        all.x = TRUE) %>%
    magrittr::extract2("short_desc") %>% factor_nosort -> icd10cm2016[["major"]]

  # can't use icd_expand_range_major here for ICD-10-CM, because it would use
  # the output of this function (and it can't just do numeric ranges because
  # there are some non-numeric characters scattered around)
  lk_majors <- unique(icd10cm2016[["three_digit"]])

  # generate lookup for sub-chapter
  sc_lookup <- data.frame(major = NULL, desc = NULL)
  for (scn in names(icd::icd10_sub_chapters)) {
    sc <- icd::icd10_sub_chapters[[scn]]
    si <- grep(sc["start"], lk_majors)
    se <- grep(sc["end"], lk_majors)
    sc_majors <- lk_majors[si:se]
    sc_lookup <- rbind(sc_lookup,
                       data.frame(sc_major = sc_majors, sc_desc = scn)
    )
  }

  # due diligence:
  if (any(dupes <- duplicated(sc_lookup$sc_major))) {
    message("duplicates found in sub-chapters:")
    print(unique(sc_lookup$sc_major[dupes]))
    stop("should not have duplicates. check sub-chapter definitions")
  }

  merge(x = icd10cm2016["three_digit"], y = sc_lookup,
        by.x = "three_digit", by.y = "sc_major",
        all.x = TRUE) %>%
    magrittr::extract2("sc_desc") -> icd10cm2016[["sub_chapter"]]

  # now the same for chapters:
  chap_lookup <- data.frame(major = NULL, desc = NULL)
  for (chap_n in names(icd::icd10_chapters)) {

    chap <- icd::icd10_chapters[[chap_n]]
    # fix a 2016 error in the CMS XML definitions
    if (chap["end"] == "Y08")
      chap["end"] <- "Y09"
    si <- grep(chap["start"], lk_majors)
    se <- grep(chap["end"], lk_majors)

    # message("start = ", sc["start"], ", end = ", sc[["end"]], ",
    #   si = ", si, ", se = ", se)

    chap_lookup <- rbind(chap_lookup,
                         data.frame(chap_major = lk_majors[si:se],
                                    chap_desc = chap_n)
    )
  }

  # due diligence:
  if (any(dupes <- duplicated(chap_lookup$chap_major))) {
    message("duplicates found in chapters:")
    print(unique(chap_lookup$chap_major[dupes]))
    stop("should not have duplicates. check sub-chapter definitions")
  }

  merge(icd10cm2016["three_digit"], chap_lookup,
        by.x = "three_digit", by.y = "chap_major", all.x = TRUE) %>%
    magrittr::extract2("chap_desc") -> icd10cm2016[["chapter"]]

  if (save_data)
    save_in_data_dir(icd10cm2016)

  invisible(icd10cm2016)
}
