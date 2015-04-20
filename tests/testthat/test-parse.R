context("test RTF parsing")

library(magrittr, quietly = TRUE, warn.conflicts = FALSE)

test_that("multiple lines in one fifth digit disciminator", {
  # nolint start
  testlines <- c("The following fifth-digit subclassification is for use with category 203:",
                 "\\par }\\pard\\plain \\ltrpar\\s59\\ql \\fi-720\\li2340\\ri0\\widctlpar\\tx180\\tx360\\tx540\\tx720\\tx900\\tx1080\\tx1260\\tx1440\\tx1620\\tx1800\\tx1980\\tx2160\\tx2340\\tx2520\\tx2700\\tx2880\\tx3060\\tx3240\\tx3420\\tx3600\\tx3780\\tx3960\\tx4140\\wrapdefault\\faauto\\rin0\\lin2340\\itap0 ",
                 "\\rtlch\\fcs1 \\af1\\afs20\\alang1025 \\ltrch\\fcs0 \\fs20\\cf1\\lang1033\\langfe1033\\loch\\af1\\hich\\af1\\dbch\\af31505\\cgrid\\langnp1033\\langfenp1033 {\\rtlch\\fcs1 \\af1 \\ltrch\\fcs0 \\insrsid2429293 \\hich\\af1\\dbch\\af31505\\loch\\f1 Hodgkin's:",
                 "\\par }\\pard\\plain \\ltrpar\\s60\\ql \\fi-720\\li2520\\ri0\\widctlpar\\tx180\\tx360\\tx540\\tx720\\tx900\\tx1080\\tx1260\\tx1440\\tx1620\\tx1800\\tx1980\\tx2160\\tx2340\\tx2520\\tx2700\\tx2880\\tx3060\\tx3240\\tx3420\\tx3600\\tx3780\\tx3960\\tx4140\\wrapdefault\\faauto\\rin0\\lin2520\\itap0 ",
                 "\\rtlch\\fcs1 \\af1\\afs20\\alang1025 \\ltrch\\fcs0 \\fs20\\cf1\\lang1033\\langfe1033\\loch\\af1\\hich\\af1\\dbch\\af31505\\cgrid\\langnp1033\\langfenp1033 {\\rtlch\\fcs1 \\af1 \\ltrch\\fcs0 \\insrsid2429293 \\hich\\af1\\dbch\\af31505\\loch\\f1 disease NOS",
                 "\\par \\hich\\af1\\dbch\\af31505\\loch\\f1 lymphom\\hich\\af1\\dbch\\af31505\\loch\\f1 a NOS",
                 "\\par }\\pard\\plain \\ltrpar\\s59\\ql \\fi-720\\li2340\\ri0\\widctlpar\\tx180\\tx360\\tx540\\tx720\\tx900\\tx1080\\tx1260\\tx1440\\tx1620\\tx1800\\tx1980\\tx2160\\tx2340\\tx2520\\tx2700\\tx2880\\tx3060\\tx3240\\tx3420\\tx3600\\tx3780\\tx3960\\tx4140\\wrapdefault\\faauto\\rin0\\lin2340\\itap0 ",
                 "\\rtlch\\fcs1 \\af1\\afs20\\alang1025 \\ltrch\\fcs0 \\fs20\\cf1\\lang1033\\langfe1033\\loch\\af1\\hich\\af1\\dbch\\af31505\\cgrid\\langnp1033\\langfenp1033 {\\rtlch\\fcs1 \\af1 \\ltrch\\fcs0 \\insrsid2429293 \\hich\\af1\\dbch\\af31505\\loch\\f1 Malignant:",
                 "\\par }\\pard\\plain \\ltrpar\\s60\\ql \\fi-720\\li2520\\ri0\\widctlpar\\tx180\\tx360\\tx540\\tx720\\tx900\\tx1080\\tx1260\\tx1440\\tx1620\\tx1800\\tx1980\\tx2160\\tx2340\\tx2520\\tx2700\\tx2880\\tx3060\\tx3240\\tx3420\\tx3600\\tx3780\\tx3960\\tx4140\\wrapdefault\\faauto\\rin0\\lin2520\\itap0 ",
                 "\\rtlch\\fcs1 \\af1\\afs20\\alang1025 \\ltrch\\fcs0 \\fs20\\cf1\\lang1033\\langfe1033\\loch\\af1\\hich\\af1\\dbch\\af31505\\cgrid\\langnp1033\\langfenp1033 {\\rtlch\\fcs1 \\af1 \\ltrch\\fcs0 \\insrsid2429293 \\hich\\af1\\dbch\\af31505\\loch\\f1 lymphogranuloma",
                 "\\par \\hich\\af1\\dbch\\af31505\\loch\\f1 lymphogranulomatosis"
  )



})

test_that("bookmark ends only", {
  expect_equal(stripRtf("{\\*\\bkmkend 200.05}{\\*\\bkmkend 200.06}{\\*\\bkmkend 200.07}{\\*\\bkmkend 200.08}{\\*\\bkmkend 200.0}\\hich\\af1\\dbch\\af31505\\loch\\f1 200.0\\tab Reticulosarcoma"),
               "200.0 Reticulosarcoma")
})

test_that("stripRtf bug case with combined line", {
  expect_equal(stripRtf(x = "\\rtlch\\fcs1 \\af1\\afs20\\alang1025 \\ltrch\\fcs0 \\fs20\\cf1\\lang1033\\langfe1033\\loch\\af1\\hich\\af1\\dbch\\af31505\\cgrid\\langnp1033\\langfenp1033 {\\rtlch\\fcs1 \\af1 \\ltrch\\fcs0 \\insrsid2429293 0\\tab \\hich\\af1\\dbch\\af31505\\loch\\f1 not stated as uncontrolled\\par }\\pard\\plain \\ltrpar\\s30\\ql \\fi-360\\li1440\\ri0\\widctlpar\\tx180\\tx360\\tx540\\tx720\\tx900\\tx1080\\tx1260\\tx1440\\tx1620\\tx1800\\tx1980\\tx2160\\tx2340\\tx2520\\tx2700\\tx2880\\tx3060\\tx3240\\tx3420\\tx3600\\tx3780\\tx3960\\tx4140\\wrapdefault\\faauto\\rin0\\lin1440\\itap0"),
               "0 not stated as uncontrolled")
})

test_that("stripRtf does what it says on the tin", {
  expect_equal(
    stripRtf("\\rtlch\\fcs1 \\af1\\afs20\\alang1025 \\ltrch\\fcs0 \\fs20\\cf1\\lang1033\\langfe1033\\loch\\af1\\hich\\af1\\dbch\\af31505\\cgrid\\langnp1033\\langfenp1033 {\\rtlch\\fcs1 \\ab\\af1 \\ltrch\\fcs0 \\b\\insrsid2429293 0\\tab \\hich\\af1\\dbch\\af31505\\loch\\f1 unspecified"),
    "0 unspecified")

  expect_equal(
    stripRtf("The following fifth-digit subclassification is for use with categories 67\\hich\\af1\\dbch\\af31505\\loch\\f1 8-679 to denote the current episode of care:"),
    "The following fifth-digit subclassification is for use with categories 678-679 to denote the current episode of care:")

  expect_equal(
    stripRtf("The following fifth-digit subclassification is for use with category 711; valid digits are in [brackets] under each code. see list at beginning of chapter for definitions:"),
    "The following fifth-digit subclassification is for use with category 711; valid digits are in [brackets] under each code. see list at beginning of chapter for definitions:")

  expect_equal(
    stripRtf("229.8\\tab Othe\\hich\\af1\\dbch\\af31505\\loch\\f1 r specified sites"),
    "229.8 Other specified sites")

  expect_equal(stripRtf("\\lsdsemihidden0 \\lsdunhideused0 \\lsdpriority71 \\lsdlocked0 Colorful Shading Accent 6;\\lsdsemihidden0 \\lsdunhideused0 \\lsdpriority72 \\lsdlocked0 Colorful List Accent 6;\\lsdsemihidden0 \\lsdunhideused0 \\lsdpriority73 \\lsdlocked0 Colorful Grid Accent 6;
"),
               "")

  # make sure we pick up unusual characters within Rtf expressions, otherwise we spill numbers etc into later parsing:
  expect_equal(stripRtf("\\par }\\pard\\plain \\ltrpar\\s82\\ql \\fi-1080\\li2160\\ri0\\widctlpar\\tx180\\tx360\\tx540\\tx720\\tx900\\tx1080\\tx1260\\tx1440\\tx1620\\tx1800\\tx1980\\tx2160\\tx2340\\tx2520\\tx2700\\tx2880\\tx3060\\tx3240\\tx3420\\tx3600\\tx3780\\tx3960\\tx4140\\wrapdefault\\faauto\\rin0\\lin2160\\itap0 "),
               "")
  # nolint end
})


test_that("tricky v91.9 works", {
  expect_equal(
    icd9Hierarchy[icd9Hierarchy$icd9 == "V9192", "descLong"],
    "Other specified multiple gestation, with two or more monoamniotic fetuses")
})

rtf_conn <- file(system.file("extdata", "Dtab12.rtf", mustWork = TRUE, package = "icd9"), encoding = "ASCII")
rtf <- parseRtfLines(readLines(rtf_conn, warn = FALSE))
close(rtf_conn)
nrtf <- names(rtf)

test_that("all parsed codes are valid decimals", {
  expect_true(all(icd9IsValidDecimal(nrtf)),
              info = paste("invalid codes are :",
                           paste(icd9GetInvalid(nrtf), collapse = ", ")))
})

test_that("no rtf formatting left in descriptions", {
  expect_false(any(grepl("[\\\\{}]", rtf)),
               info = paste("rtf codes in descriptions:",
                            paste(grep("[\\\\{}]", rtf, value = TRUE))))

})

test_that("all csv extract codes are in rtf extract", {
  missing_from_rtf <- setdiff(icd9ShortToDecimal(icd9::icd9Hierarchy$icd9), nrtf)
  expect_equal(length(missing_from_rtf), 0,
               info = paste("missing codes are:", paste(missing_from_rtf, collapse = ", ")))
})

test_that("majors extracted from web page are the same as those from RTF", {
  webmajors <- unlist(icd9ChaptersMajor) # why is this even a list not a named vector?
  work <- swapNamesWithVals(rtf)
  rtfmajors <- work[icd9IsMajor(work)]

  expect_identical(setdiff(rtfmajors, webmajors), character(0),
                   info = paste("these majors are from RTF but not retrieved from web: ",
                                paste(setdiff(rtfmajors, webmajors), collapse = ", ")))
  expect_identical(setdiff(webmajors, rtfmajors), character(0),
                   info = paste("these majors are on web but not retrieved from RTF: ",
                                paste(setdiff(webmajors, rtfmajors), collapse = ", ")))
})

v32 <- parseLeafDescriptionsVersion(version = "32", save = FALSE, fromWeb = FALSE)

test_that("all leaf codes from TXT are in RTF extract", {
  v32$icd9 %>% icd9ShortToDecimal -> leaves
  expect_true(all(leaves %in% nrtf))
})

test_that("RTF extract has no duplicates", {
  expect_equal(sum(duplicated(nrtf)),
               0,
               info = paste("first few duplicates: ",
                            paste(nrtf[duplicated(nrtf)][1:10], collapse = ", ")
               ))
})

test_that("mid-level descriptions are in RTF extract", {
  expect_equivalent(rtf["611"], "Other disorders of breast")
  expect_equivalent(rtf["611.7"], "Signs and symptoms in breast")
  expect_equivalent(rtf["611.8"], "Other specified disorders of breast")
})

test_that("manual check to look at description differences between RTF and TXT", {
  skip("manual check")
  rtf[nrtf %in% icd9ShortToDecimal(v32$icd9)] %>%
    swapNamesWithVals %>%
    sort -> rtf_leaves
  print(data.frame("From TXT" = v32$descLong, "From RTF = rtf_leaves" = names(rtf_leaves)))
})

test_that("we didn't incorrectly assign fifth (or fourth?) digit codes which are not defined", {
  # e.g. 640.01 exists but 640.02 doesn't, even though fifth-digits are defined for group from 0-4
  expect_false("640.02" %in% nrtf)
  # grep "\[[[:digit:]],.*\]" Dtab12.rtf
})

test_that("extraction from qualifier subset works", {
  all2015 <- c("[0-6]", "[0-3]", "[0-5,9]", "[0-8]", "[0-2]", "[0-1]", "[0-5]",
               "[0,1,3]", "[0-4]", "[0,3]", "[0-1,3]", "[1-2]", "[0, 1, 3]",
               "[0,1,4]", "[0,1]", "[0,2,4]", "[0-2,4]", "[0-9]", "[0,4,9]",
               "[0,9]", "[0-5,7-9]", "[5]", "[0-7,9]", "[0-6,9]", "[0-3,9]",
               "[0-4,9]", "[0-6, 9]", "[0]", "[0-7]", "[0,2-4,8,9]", "[0,2,4,8,9]",
               "[0,4,8,9]", "[6-9]", "[0,8,9]")
  expect_equal(
    parseRtfQualifierSubset("[0-6]"),
    as.character(c(0, 1, 2, 3, 4, 5, 6)))

  expect_equal(
    parseRtfQualifierSubset("[0,2-4,8,9]"),
    as.character(c(0, 2, 3, 4, 8, 9)))

  expect_equal(
    parseRtfQualifierSubset("[0]"),
    "0")

  expect_true(all(sapply(all2015, FUN = function(f) length(parseRtfQualifierSubset(f)) > 0)))
})

context("icd9Hierarchy is parsed as expected")
# at present, icd9::icd9Hierarchy is derived from RTF parsing, a little web
# scraping, some manually entered data, and (for the short description only)
# another text file parsing.`

test_that("no NA or zero-length values", {
  expect_false(any(sapply(icd9::icd9Hierarchy, is.na)))
  expect_false(any(nchar(unlist(icd9::icd9Hierarchy)) == 0))
})

test_that("factors are in the right place", {
  expect_is(icd9::icd9Hierarchy$icd9, "character")
  expect_is(icd9::icd9Hierarchy$descShort, "character")
  expect_is(icd9::icd9Hierarchy$descLong, "character")
  expect_is(icd9::icd9Hierarchy$threedigit, "factor")
  expect_is(icd9::icd9Hierarchy$major, "factor")
  expect_is(icd9::icd9Hierarchy$subchapter, "factor")
  expect_is(icd9::icd9Hierarchy$chapter, "factor")
})

test_that("codes and descriptions are valid and unique", {
  expect_equal(anyDuplicated(icd9::icd9Hierarchy$icd9), 0)
  expect_true(all(icd9IsValidShort(icd9::icd9Hierarchy$icd9)))
})

test_that("some chapters are correct", {
  chaps <- icd9::icd9Hierarchy$chapter %>% asCharacterNoWarn
  codes <- icd9::icd9Hierarchy$icd9
  # first and last rows (E codes should be last)
  expect_equal(chaps[1], "Infectious And Parasitic Diseases")
  expect_equal(chaps[nrow(icd9::icd9Hierarchy)],
               "Supplementary Classification Of External Causes Of Injury And Poisoning")

  # first and last rows of a block in the middle
  neoplasm_first_row <- which(codes == "140")
  neoplasm_last_row <- which(codes == "240") - 1
  expect_equal(chaps[neoplasm_first_row - 1], "Infectious And Parasitic Diseases")
  expect_equal(chaps[neoplasm_first_row], "Neoplasms")
  expect_equal(chaps[neoplasm_last_row], "Neoplasms")
  expect_equal(chaps[neoplasm_last_row + 1],
               "Endocrine, Nutritional And Metabolic Diseases, And Immunity Disorders")
})

test_that("some subchapters are correct", {
  subchaps <- icd9::icd9Hierarchy$subchapter %>% asCharacterNoWarn
  codes <- icd9::icd9Hierarchy$icd9

  # first and last
  expect_equal(subchaps[1], "Intestinal Infectious Diseases")
  expect_equal(subchaps[nrow(icd9::icd9Hierarchy)], "Injury Resulting From Operations Of War")

  # first and last of a block in the middle
  suicide_rows <- which(codes %in% ("E950" %i9sa% "E959"))
  expect_equal(subchaps[suicide_rows[1] - 1],
               "Drugs, Medicinal And Biological Substances Causing Adverse Effects In Therapeutic Use")
  expect_equal(subchaps[suicide_rows[1]], "Suicide And Self-Inflicted Injury")
  expect_equal(subchaps[suicide_rows[length(suicide_rows)]], "Suicide And Self-Inflicted Injury")
  expect_equal(subchaps[suicide_rows[length(suicide_rows)] + 1],
               "Homicide And Injury Purposely Inflicted By Other Persons")
})

test_that("some randomly selected rows are correct", {
  expect_equal(
    icd9::icd9Hierarchy[icd9::icd9Hierarchy$icd9 == "5060", ]  %>% sapply(asCharacterNoWarn) %>% unname,
    c("5060", "Fum/vapor bronc/pneumon", "Bronchitis and pneumonitis due to fumes and vapors",
      "506", "Respiratory conditions due to chemical fumes and vapors",
      "Pneumoconioses And Other Lung Diseases Due To External Agents",
      "Diseases Of The Respiratory System")
  )
})

test_that("billable codes are recreated", {
  check_billable <- parseLeafDescriptionsAll(save = FALSE)

  for (ver in c("27", "28", "29", "30", "31", "32")) {
    v <- icd9::icd9Billable[[ver]][["descLong"]]
    cb <- check_billable[[ver]][["descLong"]]
    diff <- v != cb
    expect_identical(check_billable[[ver]], icd9::icd9Billable[[ver]],
                     info = paste("descLong differences for v32:
                                original: ", paste(v[diff], collapse = ", "),
                                  "\nprocess:", paste(cb[diff], collapse = ", ")
                     ))
  }
})

test_that("billable codes for expected versions exist", {
  expect_true(all(as.character(23:32) %in% names(icd9Billable)))
  expect_true(all(sapply(icd9Billable, is.data.frame)))
})

test_that("billable codes are all in order", {
  skip_on_cran()
  for (v in names(icd9Billable)) {
    icd9 <- icd9::icd9Billable[[v]][["icd9"]]
    expect_identical(icd9, icd9SortShort(icd9),
                     info = paste("version = ", v))
  }
})

test_that("parsing 27 gives zero-padded digit icd9 codes", {
  expect_equal(icd9Billable[["27"]][1, "icd9"], "0010")
})
