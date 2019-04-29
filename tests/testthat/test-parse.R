context("RTF parsing")

test_that("bookmark ends only", {
  expect_equal(
    .rtf_strip(
      paste(
        "{\\*\\bkmkend 200.05}{\\*\\bkmkend 200.06}{\\*\\bkmkend 200.07}{\\*\\bkmkend 200.08}", # nolint
        "{\\*\\bkmkend 200.0}\\hich\\af1\\dbch\\af31505\\loch\\f1 200.0\\tab Reticulosarcoma"
      )
    ), # nolint
    "200.0 Reticulosarcoma"
  )
})

test_that(".rtf_strip bug case with combined line", {
  expect_equal(
    .rtf_strip(
      paste(
        sep = "", "\\rtlch\\fcs1 \\af1\\afs20\\alang1025 \\ltrch\\fcs0 \\fs20\\cf1", # nolint
        "\\lang1033\\langfe1033\\loch\\af1\\hich\\af1\\dbch\\af31505\\cgrid", # nolint
        "\\langnp1033\\langfenp1033 {\\rtlch\\fcs1 \\af1 \\ltrch\\fcs0 ",
        "\\insrsid2429293 0\\tab \\hich\\af1\\dbch\\af31505\\loch\\f1 not ",
        "stated as uncontrolled\\par }\\pard\\plain \\ltrpar\\s30\\ql \\fi-360", # nolint
        "\\li1440\\ri0\\widctlpar\\tx180\\tx360\\tx540\\tx720\\tx900\\tx1080\\tx1260", # nolint
        "\\tx1440\\tx1620\\tx1800\\tx1980\\tx2160\\tx2340\\tx2520\\tx2700\\tx2880\\tx3060", # nolint
        "\\tx3240\\tx3420\\tx3600\\tx3780\\tx3960\\tx4140\\wrapdefault",
        "\\faauto\\rin0\\lin1440\\itap0"
      )
    ),
    "0 not stated as uncontrolled"
  )
})

test_that(".rtf_strip does what it says on the tin", {
  expect_equal(
    .rtf_strip(
      paste(
        sep = "", "\\rtlch\\fcs1 \\af1\\afs20\\alang1025 \\ltrch\\fcs0 ",
        "\\fs20\\cf1\\lang1033\\langfe1033\\loch\\af1\\hich\\af1\\dbch\\af31505", # nolint
        "\\cgrid\\langnp1033\\langfenp1033 {\\rtlch\\fcs1 \\ab\\af1 \\ltrch\\fcs0 ", # nolint
        "\\b\\insrsid2429293 0\\tab \\hich\\af1\\dbch\\af31505\\loch\\f1 unspecified"
      )
    ), # nolint
    "0 unspecified"
  )

  expect_equal(
    .rtf_strip(
      paste(
        "The following fifth-digit subclassification is for use with categories", # nolint
        "67\\hich\\af1\\dbch\\af31505\\loch\\f1 8-679 to denote the current episode of care:"
      )
    ), # nolint
    paste(
      "The following fifth-digit subclassification is for use with categories 678-679", # nolint
      "to denote the current episode of care:"
    )
  )

  expect_equal(
    .rtf_strip(
      paste(
        "The following fifth-digit subclassification is for use with",
        "category 711; valid digits are in [brackets] under each code.",
        "see list at beginning of chapter for definitions:"
      )
    ),
    paste(
      "The following fifth-digit subclassification is for use with",
      "category 711; valid digits are in [brackets] under each code.",
      "see list at beginning of chapter for definitions:"
    )
  )

  expect_equal(
    .rtf_strip(
      "229.8\\tab Othe\\hich\\af1\\dbch\\af31505\\loch\\f1 r specified sites"
    ),
    "229.8 Other specified sites"
  )

  expect_equal(
    .rtf_strip(
      paste(
        sep = "",
        "\\lsdsemihidden0 \\lsdunhideused0 \\lsdpriority71 ",
        "\\lsdlocked0 Colorful Shading Accent 6;",
        "\\lsdsemihidden0 \\lsdunhideused0 \\lsdpriority72 ",
        "\\lsdlocked0 Colorful List Accent 6;",
        "\\lsdsemihidden0 \\lsdunhideused0 \\lsdpriority73 ",
        "\\lsdlocked0 Colorful Grid Accent 6;"
      )
    ),
    ""
  )

  # make sure we pick up unusual characters within Rtf expressions, otherwise we
  # spill numbers etc into later parsing:
  expect_equal(
    .rtf_strip(
      paste(
        sep = "",
        "\\par }\\pard\\plain \\ltrpar\\s82\\ql \\fi-1080\\li2160\\ri0",
        "\\widctlpar\\tx180\\tx360\\tx540\\tx720\\tx900\\tx1080\\tx1260",
        "\\tx1440\\tx1620\\tx1800\\tx1980\\tx2160\\tx2340\\tx2520\\tx2700",
        "\\tx2880\\tx3060\\tx3240\\tx3420\\tx3600\\tx3780\\tx3960\\tx4140",
        "\\wrapdefault\\faauto\\rin0\\lin2160\\itap0 "
      )
    ),
    ""
  )
})

test_that("extraction from qualifier subset works", {
  all2015 <- c(
    "[0-6]", "[0-3]", "[0-5,9]", "[0-8]", "[0-2]", "[0-1]", "[0-5]",
    "[0,1,3]", "[0-4]", "[0,3]", "[0-1,3]", "[1-2]", "[0, 1, 3]",
    "[0,1,4]", "[0,1]", "[0,2,4]", "[0-2,4]", "[0-9]", "[0,4,9]",
    "[0,9]", "[0-5,7-9]", "[5]", "[0-7,9]", "[0-6,9]", "[0-3,9]",
    "[0-4,9]", "[0-6, 9]", "[0]", "[0-7]", "[0,2-4,8,9]",
    "[0,2,4,8,9]", "[0,4,8,9]", "[6-9]", "[0,8,9]"
  )
  expect_equal(
    .rtf_parse_qualifier_subset("[0-6]"),
    as.character(c(0, 1, 2, 3, 4, 5, 6))
  )
  expect_equal(
    .rtf_parse_qualifier_subset("[0,2-4,8,9]"),
    as.character(c(0, 2, 3, 4, 8, 9))
  )
  expect_equal(
    .rtf_parse_qualifier_subset("[0]"),
    "0"
  )
  expect_true(
    all(vapply(all2015,
      FUN = function(f) length(.rtf_parse_qualifier_subset(f)) > 0,
      FUN.VALUE = logical(1)
    ))
  )
})

# The following tests on the RTF parsing get the RTF source over internet, so
# package doesn't have to include the big RTF source file
context("RTF tests")

test_year <- "2014"
skip_slow()
# if we are in offline mode, and the data is not available, we can't proceed.
# test whether the RTF is available offline. N.b. we skip in a 'context' so all
# subsequent tests are skipped.
skip_no_icd_data_resource()
if (rtf_year_ok(test_year)) {
  rtf_dat <- .icd9cm_sources[.icd9cm_sources$f_year == test_year, ]
  f_info_short <- .unzip_to_data_raw(
    url = rtf_dat$rtf_url,
    file_name = rtf_dat$rtf_filename
  )
  rtf <- .rtf_parse_lines(
    rtf_lines = readLines(f_info_short$file_path, warn = FALSE),
    year = "2014"
  )
  nrtf <- names(rtf)
  test_that("all parsed codes are valid decimals", {
    expect_true(all(icd::is_valid(nrtf, short_code = FALSE)),
      info = paste(
        "invalid codes are :",
        paste(icd::get_invalid(nrtf),
          collapse = ", "
        )
      )
    )
  })
  test_that("no rtf formatting left in descriptions", {
    expect_false(any(grepl("[\\\\{}]", rtf)),
      info = paste(
        "rtf codes in descriptions:",
        paste(grep("[\\\\{}]", rtf, value = TRUE))
      )
    )
  })
  test_that("all defined codes from csv are in rtf extract", {
    missing_from_rtf <- setdiff(
      icd::short_to_decimal(icd9cm_hierarchy[["code"]]),
      nrtf
    )
    expect_equal(
      length(missing_from_rtf), 0,
      info = paste(
        "missing codes are:",
        paste(missing_from_rtf, collapse = ", ")
      )
    )
  })

  test_that("majors extracted from web page are the same as those from RTF", {
    # why is this even a list not a named vector?
    webmajors <- unlist(icd9_majors)
    work <- .swap_names_vals(rtf)
    rtfmajors <- work[is_major(work)]

    expect_identical(
      setdiff(rtfmajors, webmajors), character(0),
      info = paste(
        "these majors are from RTF but not retrieved from web: ",
        paste(setdiff(rtfmajors, webmajors), collapse = ", ")
      )
    )
    expect_identical(
      setdiff(webmajors, rtfmajors), character(0),
      info = paste(
        "these majors are on web but not retrieved from RTF: ",
        paste(setdiff(webmajors, rtfmajors), collapse = ", ")
      )
    )
  })

  test_that("all leaf codes from TXT are in flat file extract", {
    skip_flat_icd9_avail(year = "2014")
    skip_if_not_installed("icd", "4.0")
    v32 <- .parse_icd9cm_leaf_year(
      year = "2014",
    )
    leaves <- icd::short_to_decimal(v32$code)
    expect_true(all(leaves %in% nrtf))
    rtf_leaves <- sort(
      .swap_names_vals(
        rtf[nrtf %in% icd::short_to_decimal(v32$code)]
      )
    )
  })

  test_that("RTF extract has no duplicates", {
    expect_false(
      anyDuplicated(nrtf) > 0,
      info = paste(
        "first few duplicates: ",
        paste(nrtf[duplicated(nrtf)][1:10], collapse = ", ")
      )
    )
  })

  test_that("mid-level descriptions are in RTF extract", {
    skip_on_no_rtf(test_year)
    expect_identical(rtf[["611"]], "Other disorders of breast")
    expect_identical(rtf[["611.7"]], "Signs and symptoms in breast")
    expect_identical(rtf[["611.8"]], "Other specified disorders of breast")
  })

  test_that("at this early stage, hotfix failures are present", {
    expect_identical(rtf[["038.1"]], "Staphylococcal septicemia")
    expect_identical(rtf[["737"]], "Curvature of spine")
    expect_identical(rtf[["345.1"]], "Generalized convulsive epilepsy")
    expect_identical(rtf[["414.0"]], "Coronary atherosclerosis")
    expect_identical(rtf[["414.1"]], "Aneurysm and dissection of heart")
    expect_identical(rtf[["291"]], "Alcohol-induced mental disorders")
    expect_identical(rtf[["361"]], "Retinal detachments and defects")
    expect_identical(
      rtf[["414"]],
      "Other forms of chronic ischemic heart disease"
    )
    expect_identical(
      rtf[["780.6"]],
      "Fever and other physiologic disturbances of temperature regulation"
    )
    expect_identical(
      rtf[["294"]],
      "Persistent mental disorders due to conditions classified elsewhere"
    )
  })

  # in rtf we didn't incorrectly assign fifth (or fourth?) digit codes which are
  # not defined
  test_that("correct assignment of undefined last digit codes", {
    # e.g. 640.01 exists but 640.02 doesn't, even though fifth-digits are
    # defined for group from 0-4
    expect_false("640.02" %in% nrtf)
  })

  test_that("some rtf wierdness single codes should be missing", {
    # i think this is just because I look for all possible sub-codes while
    # parsing; I never matched these codes
    bad_ones <- c(
      paste0("010.", 2:7),
      paste0("854.", 2:7),
      NULL
    )
    expect_true(all(bad_ones %nin% nrtf))
    expect_true(all(icd::children(bad_ones) %nin% nrtf))
    expect_true("012.4" %nin% nrtf)
    expect_true("012.40" %nin% nrtf)
    expect_true("012.46" %nin% nrtf)
  })
}

test_that("get chap and subchap for numeric, V, E", {
  df <- data.frame(code = c("V201", "E9990", "0101"))
  res <- .lookup_icd9_hier(df, short_code = TRUE)
  expect_true(res[1, "chapter"] ==
    "Infectious And Parasitic Diseases")
})
