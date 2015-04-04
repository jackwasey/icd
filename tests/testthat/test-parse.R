context("test RTF parsing")

test_that("multiple lines in one fifth digit disciminator", {
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

})

test_that("parse fifth digit from rtf", {
  expect_equal("\\rtlch\\fcs1 \\af1\\afs20\\alang1025 \\ltrch\\fcs0 \\fs20\\cf1\\lang1033\\langfe1033\\loch\\af1\\hich\\af1\\dbch\\af31505\\cgrid\\langnp1033\\langfenp1033 {\\rtlch\\fcs1 \\ab\\af1 \\ltrch\\fcs0 \\b\\insrsid2429293 0\\tab \\hich\\af1\\dbch\\af31505\\loch\\f1 unspecified" %>% parseFifthDigitDef,
               c("0", "unspecified"))
})

# now, this is probably year version dependent, but some ranges of codes have
# common fifth digits, specified by 'major' range. E.g. 010-018 has sub-classifications for tuberculosis diagnoses.
# tb <- "010" %i9da% "018"
# tb.fifth <- list(
#   "0" = "unspecified",
#   "1" = "bacteriological or histological examination not done",
#   "2" = "bacteriological or histological examination unknown (at present)",
#   "3" = "tubercle bacilli found (in sputum) by microscopy",
#   "4" = "tubercle bacilli not found (in sputum) by microscopy, but found by bacterial culture",
#   "5" = "tubercle bacilli not found by bacteriological examination, but tuberculosis confirmed histologically",
#   "6" = "tubercle bacilli not found by bacteriological or histological examination, but tuberculosis confirmed by other methods [inoculation of animals]")
#
# polio <- "045" %i9da% "045"
# polio.fifth <- list(
#   "0"	= "poliovirus, unspecified type",
#   "1"	= "poliovirus type I",
#   "2"	= "poliovirus type II",
#   "3"	= "poliovirus type III")
#
# hep <- "070.2" %i9da% "070.3"
# hep.fifth <- list(
#   "0" = "acute or unspecified, without mention of hepatitis delta",
#   "1" = "acute or unspecified, with hepatitis delta",
#   "2" =  "chronic, without mention of hepatitis delta",
#   "3" = "chronic, with hepatitis delta")

alllines <- readLines(system.file("extdata", "Dtab12.rtf", package = "icd9"), warn = FALSE)
v91.9_line_nums <- grep("V91\\.9", alllines)[-1]
testlines <- alllines[seq(min(v91.9_line_nums) - 1, max(v91.9_line_nums))]

test_that("sub-parse v91.9", {
  res <- parseRtf(testlines)
  expect_equal(names(res), c("V91.9", "V91.90", "V91.91", "V91.92", "V91.99"))
  expect_equal(res[["V91.92"]], "Other specified multiple gestation, with two or more monoamniotic fetuses")
})

rtf_res <- parseRtf()

test_that("tricky V91.9 and similar", {

  # first make sure we even have the codes somewhere in the source data
  v91.9_codes <- c("V91.90", "V91.91", "V91.92", "V91.99")
  for (vc in v91.9_codes)
    expect_true(any(grep(sub(".", "\\.", vc), testlines)))

  # show missing ones:
  # setdiff(v91.9_codes, names(rtf_res))
  expect_true(all(v91.9_codes %in% names(rtf_res)),
              info = paste("missing codes are:",
                           paste(setdiff(v91.9_codes, names(rtf_res)), collapse = ", ")))
})

test_that("all parsed codes are valid", {
  expect_true(all(icd9IsValidDecimal(names(rtf_res))),
              info = paste("invalid codes are :",
                           paste(icd9GetInvalid(names(rtf_res)), collapse = ", ")))
})

test_that("200-202 with eight suffix combinations", {
  alllines <- readLines(system.file("extdata", "Dtab12.rtf", package = "icd9"), warn = FALSE)
  th_line_nums <- grep("200\\.0", alllines)[-1]
  testlines <- alllines[seq(min(v91.9_line_nums), max(v91.9_line_nums))]

  # first make sure we even have the codes somewhere in the source data
  v91.9_codes <- c("V91.90", "V91.91", "V91.92", "V91.99")
  for (vc in v91.9_codes)
    expect_true(any(grep(sub(".", "\\.", vc), testlines)))

  # show missing ones:
  # setdiff(v91.9_codes, names(rtf_res))
  expect_true(all(v91.9_codes %in% names(rtf_res)),
              info = paste("missing codes are:",
                           paste(setdiff(v91.9_codes, names(rtf_res)), collapse = ", ")))
})

test_that("no rtf formatting left in descriptions", {
  expect_false(any(grepl("[{}]", rtf_res)),
               info = paste("rtf codes in descriptions:",
                            paste(grep("[{}]", rtf_res, value = TRUE))))

})

test_that("all csv extract codes are in rtf extract", {
  missing_from_rtf <- setdiff(icd9ShortToDecimal(icd9Hierarchy$icd9), names(rtf_res))
  expect_equal(length(missing_from_rtf), 0,
               info = paste("first fifty are:",
                            paste(missing_from_rtf[1:50], collapse = ", "),
                            "last fifty are:",
                            paste(tail(missing_from_rtf, 50), collapse = ", ")
               ))
})
