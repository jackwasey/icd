context("icd10 XML parse")

# true for all these tests
skip_icd10cm_xml_avail()

test_that("icd10 sub-chapters are recreated exactly", {
  skip_icd10cm_xml_avail()
  expect_identical(
    .icd10cm_extract_sub_chapters(save_data = FALSE),
    icd10_sub_chapters
  )
})

test_that("icd10 sub_chapters were parsed correctly", {
  expect_icd10_sub_chap_equal(
    paste(
      "Persons with potential health hazards related",
      "to family and personal history and certain",
      "conditions influencing health status"
    ),
    start = "Z77", end = "Z99"
  )
  expect_icd10_sub_chap_equal(
    "Persons encountering health services for examinations",
    "Z00", "Z13"
  )
  expect_icd10_sub_chap_equal(
    "Occupant of three-wheeled motor vehicle injured in transport accident",
    "V30", "V39"
  )
  expect_icd10_sub_chap_equal(
    "Malignant neuroendocrine tumors", "C7A", "C7A"
  )
  expect_icd10_sub_chap_equal(
    "Other human herpesviruses", "B10", "B10"
  )
})

test_that("ICD-10 chapters and sub-chapters are distinct", {
  skip_slow("This is a low yield and quite slow")
  skip_on_cran()
  # and for good measure, make sure that sub-chapters and chapters are not
  # confused. This was really just a problem with RTF parsing for ICD-9, but
  # there are possible similiar problems with some of the XML hierarchy.
  for (chap in names(icd10_chapters))
    expect_icd10_only_chap(chap)
  for (subchap in names(icd10_sub_chapters))
    expect_icd10_only_sub_chap(subchap)
})

test_that("Y09 got picked up in sub-chapter parsing", {
  # this is actually an error in the 2016 CMS XML which declares a range for
  # Assult from X92-Y08, but has a hanging definition for Y09 with no enclosing
  # chapter. Will have to manually correct for this until fixed.
  expect_icd10_sub_chap_equal("Assault", "X92", "Y09")
})

test_that("chapter parsing for ICD-10 went okay", {
  skip_if_not_installed("icd", "3.4")
  skip_icd10cm_xml_avail()
  chap_lookup <- .icd10_generate_chap_lookup()
  expect_false(any(duplicated(chap_lookup$chap_major)), info = y)
})

test_that("sub-chapter parsing for ICD-10 went okay", {
  skip("slow! 12 seconds on laptop")
  skip_on_cran()
  skip_if_not_installed("icd", "3.4")
  for (y in 2014:2019) {
    sc_lookup <- .icd10_generate_subchap_lookup(year = y)
    expect_equal(anyDuplicated(sc_lookup$sc_major), 0, info = y)
    # 2019 duplicated/parse errors?
    sc_lookup[sc_lookup$sc_major %in% c("C7A", "C7B", "D3A"), ]
  }
})
