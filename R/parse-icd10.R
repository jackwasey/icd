# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

# try parsing the RTF, and therefore get subheadings, as well as billable codes.
# ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD9-CM/2011/
#
# see https://github.com/LucaFoschini/ICD-9_Codes for a completely different
# approach in python

icd10cm_xml_node_to_val <- function(x, name) {
  xml2::xml_text(x[xml2::xml_name(x) == name])
}

icd10cm_xml_node_to_code <- function(x) {
  icd10cm_xml_node_to_val(x, "name")
}

icd10cm_xml_node_to_desc <- function(x) {
  icd10cm_xml_node_to_val(x, "desc")
}

icd10cm_xml_node_to_pair <- function(x) {
  code <- icd10cm_xml_node_to_code(x)
  desc <- icd10cm_xml_node_to_desc(x)
  names(desc) <- code
  desc
}

#' Get subchapters from the 2016 XML for ICD-10-CM
#'
#' This is a superset of ICD-10 sub-chapters, I think.
#'
#' This is complicated by the XML document specifying more hierarchical
#' levels,e.g. C00-C96, C00-75 are both specified within the chapter Neoplasms
#' (C00-D49). A way of determining whether there are extra levels would be to
#' check the XML tree depth for a member of each putative sub-chapter. May be
#' easier to hard code these in.
#' @param save_data single logical
#' @keywords internal
icd10cm_extract_sub_chapters <- function(save_data = FALSE) {
  assertFlag(save_data)
  loadNamespace("xml2")
  url <- "http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_Full_XML.ZIP"
  f_info <- unzip_to_data_raw(url, "Tabular.xml")
  j <- xml2::read_xml(f_info$file_path)
  j  %>%  xml2::xml_find_all("//chapter/name")  %>%
    xml2::xml_text() -> chapter_nums

  j  %>%  xml2::xml_find_all("//chapter/desc")  %>%
    xml2::xml_text() -> chapter_names

  j  %>% xml2::xml_children() %>%
    xml2::xml_name() %>%
    equals("chapter" ) -> chapter_indices
  # could do xpath, but harder to loop
  j  %>% xml2::xml_children() %>% magrittr::extract(chapter_indices) -> chaps

  icd10_sub_chapters <- list()
  for (chap in chaps) {
    chap  %>% xml2::xml_children() -> c_kids
    c_kids %>% xml2::xml_name() %>% magrittr::equals("section") -> subchap_indices
    c_kids %>% extract(subchap_indices) -> subchaps

    for (subchap in subchaps) {
      subchap  %>%
        xml2::xml_children()  %>%
        extract(1) %>%
        xml2::xml_text() %>%
        chapter_to_desc_range.icd10 -> new_sub_chap_range

      # should only match one at a time
      stopifnot(length(new_sub_chap_range) == 1)

      # check that this is a real subchapter, not an extra range defined in the
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

#' Get ICD-10 (not ICD-10-CM) as published by CDC
#'
#' @details There is no copyright notice, and, as I understand it, by default US
#'   government publications are public domain
#'   ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10/ and thus
#'   this or derivative data can be included in the package distribution
#' @keywords internal
icd10_get_who_from_cdc <- function() {
  # beware, not all download.file methods can handle %20 etc in URLs correctly.
  url <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10/allvalid2011%20%28detailed%20titles%20headings%29.txt"
  file_path <- download_to_data_raw(url = url)$file_path

  # typically, the file isn't easily machine readable with stupidly placed
  # annotations, e.g. "Added in 2009	A09.9	Gastroenteritis and colitis of
  # unspecified origin" I've no idea what those people are thinking when they do
  # this kind of thing.

  # ignore locale issue right now. This set has a lot of the different cases: dat[70:75,]
  readr::read_lines(file_path, skip = 7) %>%
    str_trim() %>%
    str_match("(.*\\t)?(.+)\\t+(.+)") -> dat

  code_or_range <- dat[, 3]
  desc <- dat[, 4]

  # this data set does not explicitly say which codes are leaves or parents.
  is_range <- str_detect(code_or_range, "-")
  # this is a mix of chapters and sub-chapters, and would require processing to
  # figure out which

  codes <- dat[!is_range, 3]
  codes_desc <- dat[!is_range, 4]

  class(codes) <- c("icd10who", "icd10", "character")
  # do some sanity checks:
  stopifnot(all(icd_is_valid(codes)))

  #> codes[!icd_is_valid(codes)]
  #[1] "*U01"   "*U01.0" "*U01.1" "*U01.2" "*U01.3" "*U01.4" "*U01.5" "*U01.6" "*U01.7" "*U01.8" "*U01.9" "*U02"   "*U03"   "*U03.0"
  #[15] "*U03.9" NA

  stop("work in progress", codes_desc, desc)
}

#' get all ICD-10-CM codes
#'
#' gets all ICD-10-CM codes from an archive on the CDC web site at Initially,
#' this just grabs 2016.
#' @source
#' \url{http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip}.
#' @references
#' https://www.cms.gov/Medicare/Coding/ICD10/downloads/icd-10quickrefer.pdf
#' @keywords internal
icd10cm_get_all_defined <- function(save = FALSE) {

  f_info <- unzip_to_data_raw(
    url = "http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_code_descriptions.zip",
    file_name = "icd10cm_order_2016.txt")

  x <- readLines(con = f_info$file_path)
  icd10cm2016 <- data.frame(#id = substr(x, 1, 5),
    code = substr(x, 7, 13),
    billable = substr(x, 14, 15),
    descShort = substr(x, 16, 76),
    descLong = substr(x, 77, stop = 1e5),
    threedigit = NA,
    major = NA,
    subchapter = NA,
    chapter = NA,
    stringsAsFactors = FALSE
  )

  icd10cm2016 <- as.data.frame(lapply(icd10cm2016, str_trim),
                               stringsAsFactors = FALSE)
  icd10cm2016[["code"]] %<>% icd10cm %>% icd_short_code
  icd10cm2016[["code"]] %>% icd_get_major -> icd10cm2016[["threedigit"]]

  # get description for the major type
  merge(icd10cm2016["threedigit"],
        icd10cm2016[c("code", "descShort")],
        by.x = "threedigit", by.y = "code",
        all.x = TRUE) %>%
    extract2(2) %>% as.factor ->
    icd10cm2016[["major"]]

  # can't use icd_expand_range_major here for ICD-10-CM, because it would use
  # the output of this function (and it can't just do numeric ranges because
  # there are some non-numeric characters scattered around)
  lk_majors <- unique(icd10cm2016[["threedigit"]])

  # generate lookup for sub-chapter
  sc_lookup <- data.frame(major = NULL, desc = NULL)
  for (scn in names(icd9::icd10_sub_chapters)) {

    sc <- icd9::icd10_sub_chapters[[scn]]
    si <- grep(sc["start"], lk_majors)
    se <- grep(sc["end"], lk_majors)

    # message("start = ", sc["start"], ", end = ", sc[["end"]], ",
    #   si = ", si, ", se = ", se)
    sc_majors <- lk_majors[si:se]
    sc_lookup <- rbind(sc_lookup,
                       data.frame(sc_major = sc_majors, sc_desc = scn)
    )
  }

  # due diligence:
  if (any(dupes <- duplicated(sc_lookup$sc_major))) {
    message("duplicates found:")
    print(unique(sc_lookup$sc_major[dupes]))
    stop("should not have duplicates. check subchapter definitions")
  }

  merge(icd10cm2016["threedigit"], sc_lookup,
        by.x = "threedigit", by.y = "sc_major", all.x = TRUE) %>%
    extract2("sc_desc") -> icd10cm2016[["subchapter"]]


  # now the same for chapters:

  # generate lookup for sub-chapter
  chap_lookup <- data.frame(major = NULL, desc = NULL)
  for (chap_n in names(icd9::icd10_chapters)) {

    chap <- icd9::icd10_chapters[[chap_n]]
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
    message("duplicates found:")
    print(unique(chap_lookup$chap_major[dupes]))
    stop("should not have duplicates. check subchapter definitions")
  }

  merge(icd10cm2016["threedigit"], chap_lookup,
        by.x = "threedigit", by.y = "chap_major", all.x = TRUE) %>%
    extract2("chap_desc") -> icd10cm2016[["chapter"]]

  if (save)
    save_in_data_dir(icd10cm2016)
  invisible(icd10cm2016)

  # now some test code to see what permutations there are of ICD-10 codes based
  # on the 2016 CM set.
  #i10 <- icd10cm2016$code

  #alpha_in_tail <- grep("[[:alpha:]]", i10tail, value = TRUE)
  #alpha_in_tail_bool <- grepl("[[:alpha:]].*[[:alpha:]].*", x = i10)
  #alpha_in_tail <- i10[alpha_in_tail_bool]
  #unique(gsub("[[:digit:]]", replacement = "", x = alpha_in_tail))

  # verify, e.g. J in middle?
  #grep("[[:alpha::]].*J.*", i10)

  # find unique characters at each position from 4 to 7
  # for (i in 1:7)
  #   message(i)
  #   substring(alpha_in_tail, i, i) %>% unique %>% sort %>% message
  # }
}
