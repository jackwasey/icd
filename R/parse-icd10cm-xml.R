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
#' @keywords internal datagen
icd10cm_extract_sub_chapters <- function(save_data = FALSE, offline = TRUE) {
  assert_flag(save_data)
  f_info <- icd10cm_get_xml_file(offline = offline)
  stopifnot(!is.null(f_info))
  j <- xml2::read_xml(f_info$file_path)

  # using magrittr::equals and extract because I don't want to import them. See
  # \code{icd-package.R} for what is imported. No harm in being explicit, since
  # :: will do an implicit requireNamespace.
  xml2::xml_name(xml2::xml_children(j)) == "chapter" -> chapter_indices
  # could do xpath, but harder to loop
  chaps <- xml2::xml_children(j)[chapter_indices]
  icd10_sub_chapters <- list()
  for (chap in chaps) {
    c_kids <- xml2::xml_children(chap)
    subchap_indices <- xml2::xml_name(c_kids) == "section"
    subchaps <- c_kids[subchap_indices]
    for (subchap in subchaps) {
      new_sub_chap_range <-
        chapter_to_desc_range.icd10(
          xml2::xml_text(
            xml2::xml_children(
              subchap
            )[1]
          )
        )
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
  if (save_data) save_in_data_dir(icd10_sub_chapters)
  invisible(icd10_sub_chapters)
}

# nocov end
