.dl_icd10cm_xml <- function(ver = "2019", ...) {
  # http://www.cdc.gov/nchs/data/icd/icd10cm/2016/ICD10CM_FY2016_Full_XML.ZIP
  s <- .icd10cm_sources[[ver]]
  .unzip_to_data_raw(
    url = paste0(s$base_url, s$dx_xml_zip),
    file_name = s$dx_xml,
    save_name = .get_versioned_raw_file_name(s$dx_xml, ver),
    dl_msg = "Downloading ICD-10-CM 2019 XML",
    ...
  )
}

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
#' @template save_pkg_data
#' @keywords internal datagen
#' @noRd
.icd10cm_extract_sub_chapters <- function(save_pkg_data = FALSE,
                                          ...) {
  stopifnot(is.logical(save_pkg_data))
  f_info <- .dl_icd10cm_xml(...)
  stopifnot(!is.null(f_info))
  j <- xml2::read_xml(f_info$file_path)
  xml2::xml_name(xml2::xml_children(j)) == "chapter" -> chapter_indices
  chaps <- xml2::xml_children(j)[chapter_indices]
  icd10_sub_chapters <- list()
  for (chap in chaps) {
    c_kids <- xml2::xml_children(chap)
    subchap_indices <- xml2::xml_name(c_kids) == "section"
    subchaps <- c_kids[subchap_indices]
    for (subchap in subchaps) {
      new_sub_chap_range <-
        .chapter_to_desc_range.icd10(
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
      if (new_sub_chap_range[[1]]["end"] == "Y08") {
        new_sub_chap_range[[1]]["end"] <- "Y09"
      }
      icd10_sub_chapters <- append(icd10_sub_chapters, new_sub_chap_range)
    } # subchaps
  } # chapters
  # reorder quirk C7A, C7B, D3A) which are actually subchapters. We should
  # already have included num-alpha-num codes which are within ranges.
  c7ab <- icd10_sub_chapters[35:36]
  icd10_sub_chapters[35:36] <- NULL
  icd10_sub_chapters <- append(icd10_sub_chapters, c7ab, after = 35)
  d3a <- icd10_sub_chapters[41]
  icd10_sub_chapters[41] <- NULL
  icd10_sub_chapters <- append(icd10_sub_chapters, d3a, after = 41)
  if (save_pkg_data) .save_in_data_dir(icd10_sub_chapters)
  invisible(icd10_sub_chapters)
}
