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

context("deprecated icd9 ranges")

test_that("deprecated - expand icd9 range definition", {
  expect_equal_no_icd(
    icd9ExpandRangeShort("4012", "40145",
                         onlyReal = FALSE,
                         excludeAmbiguousStart = FALSE,
                         excludeAmbiguousEnd = FALSE),
    sort(c("4012", "40120", "40121", "40122", "40123", "40124", "40125",
           "40126", "40127", "40128", "40129", "4013", "40130", "40131",
           "40132", "40133", "40134", "40135", "40136", "40137", "40138",
           "40139", "4014", "40140", "40141", "40142", "40143", "40144", "40145")))
  expect_equal_no_icd(
    icd9ExpandRangeShort("4012", "40145",
                         onlyReal = FALSE,
                         excludeAmbiguousStart = TRUE,
                         excludeAmbiguousEnd = TRUE),
    sort(c("4012", "40120", "40121", "40122", "40123", "40124", "40125",
           "40126", "40127", "40128", "40129", "4013", "40130", "40131",
           "40132", "40133", "40134", "40135", "40136", "40137", "40138",
           "40139", NULL, "40140", "40141", "40142", "40143", "40144", "40145")))
  # the following tests the unimplemented omitParents = TRUE
  #   expect_equal(
  #     icd9ExpandRangeShort("4012", "40145", omitParents = TRUE),
  #     sort(c("4012", "40120", "40121", "40122", "40123", "40124", "40125",
  #            "40126", "40127", "40128", "40129", "4013", "40130", "40131",
  #            "40132", "40133", "40134", "40135", "40136", "40137", "40138",
  #            "40139", "40140", "40141", "40142", "40143", "40144", "40145")))
  #
  expect_equal_no_icd(icd9ExpandRangeShort("40100", "40101", onlyReal = FALSE), c("40100", "40101"))
  expect_equal_no_icd(icd9ExpandRange("40108", "40109", isShort = TRUE, onlyReal = FALSE), c("40108", "40109"))
  expect_equal_no_icd(icd9ExpandRangeShort("40198", "40199", onlyReal = FALSE), c("40198", "40199"))
  # must be in ICD9 order, otherwise error:
  expect_error(icd9ExpandRangeShort("40109", "40108"))
  expect_error(icd9ExpandRangeShort("4019", "4018"))
  expect_error(icd9ExpandRangeShort("402", "401"))
  expect_error(icd9ExpandRangeShort("2", "1"))
  expect_error(icd9ExpandRangeShort("002", "1"))
  expect_error(icd9ExpandRangeShort("002", "001"))
  expect_error(icd9ExpandRangeShort("2", "001"))
  expect_error(icd9ExpandRangeShort("4010", "401"))

  expect_equal(icd9ExpandRangeShort(" 4280 ", " 4280 ", onlyReal = FALSE),
               icd9ExpandRangeShort("4280", "4280", onlyReal = FALSE))

  # the range 44100-4419 from the AHRQ found a gap in the code.
  expect_equal_no_icd(
    sort(icd9ExpandRangeShort("4410", "4412", onlyReal = FALSE)),
    sort(c("4410", icd9ExpandRangeShort("44100", "4412", onlyReal = FALSE)))
  )

  expect_equal(icd9ExpandRangeShort("401", "401", onlyReal = FALSE),
               sort(icd9Children("401", isShort = TRUE, onlyReal = FALSE)))
  # expand range should already be sorted. do i want to sort children by default
  # or with an option?
  expect_equal(icd9ExpandRangeShort("401", "402", onlyReal = FALSE),
               sort(icd9Children(c("401", "402"), isShort = TRUE, onlyReal = FALSE)))
  # the next two cases cover the HIV ranges in the co-morbidities, wherein the
  # final code is included, in which case the parent ("044" in this case) is
  # implied strongly. CAN'T EXPECT RANGE TO ACCOUNT FOR THIS, but we can make next test work with flag as follows:
  expect_equal(icd9ExpandRangeShort("043", "0449", onlyReal = FALSE, excludeAmbiguousEnd = FALSE),
               icd9ExpandRangeShort("043", "044", onlyReal = FALSE))
  expect_equal(icd9ExpandRangeShort("043", "04499", onlyReal = FALSE),
               icd9ExpandRangeShort("043", "044", onlyReal = FALSE))
  expect_equal_no_icd(
    icd9ExpandRangeShort("401", "402", onlyReal = FALSE),
    sort(c("401", "4010", "4011", "4012", "4013", "4014", "4015", "4016",
           "4017", "4018", "4019", "40100", "40110", "40120", "40130", "40140",
           "40150", "40160", "40170", "40180", "40190", "40101", "40111",
           "40121", "40131", "40141", "40151", "40161", "40171", "40181",
           "40191", "40102", "40112", "40122", "40132", "40142", "40152",
           "40162", "40172", "40182", "40192", "40103", "40113", "40123",
           "40133", "40143", "40153", "40163", "40173", "40183", "40193",
           "40104", "40114", "40124", "40134", "40144", "40154", "40164",
           "40174", "40184", "40194", "40105", "40115", "40125", "40135",
           "40145", "40155", "40165", "40175", "40185", "40195", "40106",
           "40116", "40126", "40136", "40146", "40156", "40166", "40176",
           "40186", "40196", "40107", "40117", "40127", "40137", "40147",
           "40157", "40167", "40177", "40187", "40197", "40108", "40118",
           "40128", "40138", "40148", "40158", "40168", "40178", "40188",
           "40198", "40109", "40119", "40129", "40139", "40149", "40159",
           "40169", "40179", "40189", "40199", "402", "4020", "4021", "4022",
           "4023", "4024", "4025", "4026", "4027", "4028", "4029", "40200",
           "40210", "40220", "40230", "40240", "40250", "40260", "40270",
           "40280", "40290", "40201", "40211", "40221", "40231", "40241",
           "40251", "40261", "40271", "40281", "40291", "40202", "40212",
           "40222", "40232", "40242", "40252", "40262", "40272", "40282",
           "40292", "40203", "40213", "40223", "40233", "40243", "40253",
           "40263", "40273", "40283", "40293", "40204", "40214", "40224",
           "40234", "40244", "40254", "40264", "40274", "40284", "40294",
           "40205", "40215", "40225", "40235", "40245", "40255", "40265",
           "40275", "40285", "40295", "40206", "40216", "40226", "40236",
           "40246", "40256", "40266", "40276", "40286", "40296", "40207",
           "40217", "40227", "40237", "40247", "40257", "40267", "40277",
           "40287", "40297", "40208", "40218", "40228", "40238", "40248",
           "40258", "40268", "40278", "40288", "40298", "40209", "40219",
           "40229", "40239", "40249", "40259", "40269", "40279", "40289",
           "40299"))
  )

  expect_equal_no_icd(icd9ExpandRangeShort("401", "40102", onlyReal = FALSE,
                                           excludeAmbiguousStart = FALSE,
                                           excludeAmbiguousEnd = FALSE),
                      c("401", "4010", "40100", "40101", "40102"))

  # only works with single range
  expect_error(icd9ExpandRangeShort(c("10", "20"), c("11", "21")))

  # found bugs when expanding Injury and Poisoning chapter.
  expect_error(icd9ExpandRangeShort("997", "998"), regexp = NA)
  expect_false("999" %in% icd9ExpandRangeShort("998", "998", onlyReal = FALSE))
  expect_false("009" %in% icd9ExpandRangeShort("8", "8", onlyReal = FALSE))

})

test_that("deprecated - deprecated - expand range defined by two four digit codes includes last code", {
  expect_true("1991" %in% icd9ExpandRangeShort("1960", "1991", onlyReal = FALSE))
  expect_true("19919" %in% icd9ExpandRangeShort("1960", "1991", onlyReal = FALSE))
})

test_that("deprecated - expand range worker gives correct ranges", {
  # really, the test is against icd9ExpandRange family, but we can isolate an
  # error to the sub-function
  expect_equal(
    expandRangeWorker("V10", "V1001", lookup = icd:::icd9VShort,
                      onlyReal = TRUE, excludeAmbiguousStart = FALSE, excludeAmbiguousEnd = FALSE),
    c("V10", "V100", "V1000", "V1001"))
})

test_that("deprecated - V code with ambiguous parent", {
  # although we don't usually return parents whose scope overlaps the upper
  # limit, if the range specification already has this 'anomaly', we just roll
  # with it.

  # the default should be to include the stated higher-level code, and enough
  # descendants just to reach the specified codes, but not all the children of
  # the higher-level code.
  expect_equal_no_icd(icd9ExpandRangeShort("V10", "V1001",
                                           onlyReal = FALSE, excludeAmbiguousStart = FALSE, excludeAmbiguousEnd = FALSE),
                      c("V10", "V100", "V1000", "V1001"))
  expect_equal_no_icd(icd9ExpandRangeShort("V10", "V1001", onlyReal = FALSE),
                      c("V1000", "V1001"))
})

test_that("deprecated - V code ranges", {
  expect_equal_no_icd(icd9ExpandRangeShort("V1000", "V1002", onlyReal = FALSE),
                      c("V1000", "V1001", "V1002"))
  # but we cap off the upper range correctly:
  expect_equal_no_icd(icd9ExpandRangeShort("V1009", "V101", onlyReal = FALSE),
                      c("V1009", "V101", "V1010", "V1011",
                        "V1012", "V1013", "V1014", "V1015",
                        "V1016", "V1017", "V1018", "V1019"))
  # and with narrower top end
  expect_equal_no_icd(icd9ExpandRangeShort("V1009", "V1011",
                                           onlyReal = FALSE, excludeAmbiguousStart = TRUE, excludeAmbiguousEnd = TRUE),
                      c("V1009", "V1010", "V1011"))
  expect_equal_no_icd(icd9ExpandRangeShort("V1009", "V1011",
                                           onlyReal = FALSE, excludeAmbiguousStart = FALSE, excludeAmbiguousEnd = FALSE),
                      c("V1009", "V101", "V1010", "V1011"))
  # but include those pesky parents when requested:
  expect_true(
    all(c("V10", "V100") %in% icd9ExpandRangeShort("V099", "V1011", onlyReal = FALSE,
                                                   excludeAmbiguousStart = FALSE, excludeAmbiguousEnd = FALSE)))

  # should fail despite end being 'longer' than start
  expect_error(icd9ExpandRangeShort("V10", " V1 "))

  expect_equal(icd9ExpandRangeShort("V1009", "V101", onlyReal = FALSE),
               icd9ExpandRangeShort("V1009", "V1019", onlyReal = FALSE))

  # failed similar test in Elixhauser mapping generation.
  expect_false("V11" %in% icd9ExpandRangeDecimal("V10.89", "V10.9", onlyReal = FALSE))
  expect_false("V11" %in% icd9ExpandRange("V10.89", "V10.9", isShort = FALSE, onlyReal = FALSE))
  expect_false("V11" %in% icd9ExpandRangeDecimal("V10.89", "V10.99", onlyReal = FALSE))

})

test_that("deprecated - E code ranges", {
  expect_equal_no_icd(icd9ExpandRangeShort("E9501", "E9502", onlyReal = FALSE), c("E9501", "E9502"))
  expect_equal_no_icd(icd9ExpandRangeShort("E950", "E9509", onlyReal = FALSE),
                      c("E950", "E9500", "E9501", "E9502", "E9503", "E9504",
                        "E9505", "E9506", "E9507", "E9508", "E9509")
  )
  expect_equal(icd9AddLeadingZeroesShort("E9501"), "E9501")
})

test_that("deprecated - major ranges", {
  resall <- icd9ExpandRangeMajor("E000", "E999", onlyReal = FALSE)
  expect_equal(length(resall), 1000)
  expect_true("E000" %in% resall)
  expect_true("E123" %in% resall)
  expect_true("E999" %in% resall)
  resallbut <- icd9ExpandRangeMajor("E001", "E998", onlyReal = FALSE)
  expect_equal(length(resallbut), 998)
  expect_false("E000" %in% resallbut)
  expect_true("E001" %in% resallbut)
  expect_true("E123" %in% resallbut)
  expect_true("E998" %in% resallbut)
  expect_false("E999" %in% resallbut)

  expect_equal_no_icd(icd9ExpandRangeMajor("E99", "E101", onlyReal = FALSE),
                      c("E099", "E100", "E101"))
})

test_that("deprecated - range bugs", {
  # these both failed - need zero padding for the first
  expect_equal_no_icd( ("042 " %i9s% "042 ")[1], "042")
  expect_equal_no_icd( ("42" %i9s% "042 ")[1], "042")
  expect_true("345" %nin% ("3420 " %i9s% "3449 "))

  # unclass these, because new version of icd9 (aka icd?) will set class on result
  expect_equivalent(unclass("042.11" %i9da% "042.13"), c("042.11", "042.12", "042.13"))

  # no presumption that missing leading zeroes will be missed on output:
  expect_equivalent(unclass("42.11" %i9da% "42.13"), c("042.11", "042.12", "042.13"))
})

test_that("deprecated - range doesn't include higher level parent github issue #14", {
  # by default, any code returned in a range should also have all of its
  # children, if any, in the range (whether including or excluding non-real.
  expect_false("0101" %in% ("01006" %i9sa% "01010"))
  # 0101 isn't billable itself
  expect_false("0101" %in% ("01006" %i9s% "01010"))
  # if real codes, then we can tolerate a higher level code if it is billable,
  # e.g. 390 (no children)
  expect_true("390" %in% ("389.9" %i9d% "391.1"))
  # but not if we are looking at all possible codes. This is a subtle strange
  # distinction. It is primarily of importance when expanding codes describing
  # ICD to comorbodity mappings. We might want to either include all possible
  # sub-codes, even if they are not (yet, or anymore) 'real'.
  expect_false("390" %in% ("389.9" %i9da% "390.1"))
  # and if range definitely covers the higher level code, re-affirm it is still in there:
  expect_true("390" %in% ("389.9" %i9d% "391.1"))
  expect_true("390" %in% ("389.9" %i9da% "391.1"))
})

test_that("deprecated - ranges can include ambiguous parents, optionally", {
  expect_equal_no_icd(
    icd9ExpandRange("01006", "01010", onlyReal = TRUE, excludeAmbiguousStart = TRUE, excludeAmbiguousEnd = TRUE),
    c("01006", "01010"))
  expect_equal_no_icd(
    icd9ExpandRange("01006", "01010", onlyReal = TRUE, excludeAmbiguousStart = FALSE, excludeAmbiguousEnd = FALSE),
    c("01006", "0101", "01010"))
  expect_equal_no_icd(
    icd9ExpandRange("01006", "01010", onlyReal = FALSE, excludeAmbiguousStart = TRUE, excludeAmbiguousEnd = TRUE),
    c("01006", "01007", "01008", "01009", "01010"))
  expect_equal_no_icd(
    icd9ExpandRange("01006", "01010", onlyReal = FALSE, excludeAmbiguousStart = FALSE, excludeAmbiguousEnd = FALSE),
    c("01006", "01007", "01008", "01009", "0101", "01010"))

  # if real codes, then we can tolerate a higher level code if it is billable,
  # e.g. 390 (no children)
  expect_true("390" %in% ("389.9" %i9d% "391.1"))
  # but not if we are looking at all possible codes. This is a subtle strange
  # distinction. It is primarily of importance when expanding codes describing
  # ICD to comorbodity mappings. We might want to either include all possible
  # sub-codes, even if they are not (yet, or anymore) 'real'.
  expect_false("390" %in% ("389.9" %i9da% "390.1"))
  # and if range definitely covers the higher level code, re-affirm it is still in there:
  expect_true("390" %in% ("389.9" %i9d% "391.1"))
  expect_true("390" %in% ("389.9" %i9da% "391.1"))
})

test_that("deprecated - range abbrevs", {
  expect_identical(icd9ExpandRange("123", "123.6", isShort = FALSE, onlyReal = FALSE),
                   "123" %i9da% "123.6")
  expect_identical(icd9ExpandRange("123", "123.6", isShort = FALSE, onlyReal = TRUE),
                   "123" %i9d% "123.6")
  expect_identical(icd9ExpandRange("1234", "125", isShort = TRUE, onlyReal = FALSE),
                   "1234" %i9sa% "125")
})

test_that("deprecated - icd9ExpandMinor: invalid", {
  expect_error(icd9ExpandMinor(c(1, 2)))
  expect_error(icd9ExpandMinor("JACK"))
  expect_error(icd9ExpandMinor(c(123)))
  expect_error(icd9ExpandMinor(c("123")))
  expect_error(icd9ExpandMinor(c(1, 2), isE = TRUE))
  expect_error(icd9ExpandMinor("JACK"), isE = TRUE)
  expect_error(icd9ExpandMinor(c(123), isE = TRUE))
  expect_error(icd9ExpandMinor("00", isE = TRUE))
  expect_error(icd9ExpandMinor("E0000", isE = TRUE))
  expect_error(icd9ExpandMinor("99", isE = TRUE))
})

test_that("deprecated - icd9ExpandMinor: valid", {
  expect_equal(length(icd9ExpandMinor("", isE = FALSE)), 111)
  expect_equal(length(icd9ExpandMinor("", isE = TRUE)), 11)
  expect_identical(icd9ExpandMinor("00", isE = FALSE), "00")
  expect_identical(icd9ExpandMinor("9", isE = FALSE), as.character(c(9, 90:99)))
  expect_identical(icd9ExpandMinor("9", isE = TRUE), "9")

  expect_equal(icd9ExpandMinor("0", isE = TRUE), "0")
  expect_equal(icd9ExpandMinor("9", isE = TRUE), "9")
  expect_equal(icd9ExpandMinor("", isE = TRUE), c("", as.character(0:9)))

})

test_that("deprecated - icd9ChildrenDecimal valid input", {
  expect_equal_no_icd(
    icd9ChildrenDecimal("V10.0", onlyReal = FALSE),
    append("V10.0", paste("V10.0", 0:9, sep = "")))
  expect_equal(
    toupper(icd9ChildrenDecimal("v10.0", onlyReal = FALSE)),
    icd9ChildrenDecimal("V10.0", onlyReal = FALSE))
  expect_equal(
    icd9ChildrenDecimal(" V10.0 ", onlyReal = FALSE),
    icd9ChildrenDecimal("V10.0", onlyReal = FALSE))
  expect_equal(
    icd9ChildrenDecimal("10.0", onlyReal = FALSE),
    icd9ChildrenDecimal("010.0", onlyReal = FALSE))
  expect_equal_no_icd(
    icd9ChildrenDecimal("010.0", onlyReal = FALSE),
    append("010.0", paste("010.0", 0:9, sep = "")))
  expect_equal(icd9ChildrenDecimal("010.0"), icd9ChildrenDecimal("10.0"))
})

test_that("deprecated - icd9ChildrenShort valid input", {
  expect_equal_no_icd(icd9ChildrenShort("V100", onlyReal = FALSE),
                      paste("V100", c("", 0:9), sep = ""))
  expect_equal_no_icd(icd9ChildrenShort("v100"), icd9Children("V100"))
  expect_equal(icd9ChildrenShort(" V100 ", onlyReal = FALSE),
               icd9ChildrenShort("V100", onlyReal = FALSE))
  expect_equal_no_icd(icd9ChildrenShort("0100", onlyReal = FALSE),
                      paste("0100", c("", 0:9), sep = ""))
  expect_equal_no_icd(icd9ChildrenShort("1", onlyReal = FALSE)[1], "001")
  expect_equal_no_icd(icd9ChildrenShort("01", onlyReal = FALSE)[1], "001")
  expect_equal_no_icd(icd9ChildrenShort("001", onlyReal = FALSE)[1], "001")
  expect_equal_no_icd(icd9ChildrenShort("023", onlyReal = FALSE)[1], "023")
  expect_equal_no_icd(icd9ChildrenShort("23", onlyReal = FALSE)[1], "023")
  expect_equal_no_icd(icd9ChildrenShort("456", onlyReal = FALSE)[1], "456")
  expect_equal_no_icd(
    icd9ChildrenShort("E100", onlyReal = FALSE),
    c("E100", "E1000", "E1001", "E1002", "E1003", "E1004",
      "E1005", "E1006", "E1007", "E1008", "E1009"))
  expect_equal_no_icd(icd9ChildrenShort("390", onlyReal = TRUE), "390")
})

test_that("deprecated - icd9InReferenceCode deals with bad input", {
  expect_equal(icd9InReferenceCode(NA, "123", isShort = TRUE), FALSE) # arguable: could return NA here
  expect_equal(icd9InReferenceCode("", "123", isShort = TRUE), FALSE)
  expect_equal(icd9InReferenceCode("bratwurst", "123", isShort = TRUE), FALSE)
})

test_that("deprecated - icd9InReferenceCode mixing short and decimals", {
  expect_equal(icd9InReferenceCode("123.45", "12345", isShort = FALSE, isShortReference = TRUE), TRUE)
  expect_equal(icd9InReferenceCode("12345", "123.45", isShort = TRUE, isShortReference = FALSE), TRUE)
})

test_that("deprecated - icd9InReferenceCode test code format matches mapping format", {
  expect_equal(icd9InReferenceCode("123.45", "123.45", isShort = FALSE, isShortReference = FALSE), TRUE)
  expect_equal(icd9InReferenceCode("12345", "12345", isShort = TRUE, isShortReference = TRUE), TRUE)
})

test_that("deprecated - icd9InReferenceCode produces the right length output", {
  expect_equal(
    icd9InReferenceCode(c("100", "200"), c("400", "100", "200", "300"), isShort = TRUE),
    c(TRUE, TRUE)
  )
  expect_equal(
    icd9InReferenceCode(c("100", "99"), c("400", "100", "200", "300"), isShort = TRUE),
    c(TRUE, FALSE)
  )
  expect_equal(
    icd9InReferenceCode(c("99", "100"), c("400", "100", "200", "300"), isShort = TRUE),
    c(FALSE, TRUE)
  )
  expect_equal(
    icd9InReferenceCode(c("99", "97"), c("400", "100", "200", "300"), isShort = TRUE),
    c(FALSE, FALSE)
  )
  expect_equal(
    icd9InReferenceCode(c("100", "200", "99"), c("400", "100", "200", "300"), isShort = TRUE),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("deprecated - icd9InReferenceCode", {

  expect_equal(icd9InReferenceCode(c("421", "123"), c("123", "V"), isShort = FALSE), c(FALSE, TRUE))
  expect_equal(
    icd9InReferenceCode(c("421", "123"), c("123", "V"), isShort = FALSE, isShortReference = FALSE),
    c(FALSE, TRUE))
  expect_equal(icd9InReferenceCode(c("421", "123"), c("123", "V42"), isShort = FALSE), c(FALSE, TRUE))
  expect_equal(icd9InReferenceCode(c("123", "V43210"), c("421", "123"), isShort = TRUE), c(TRUE, FALSE))
  expect_equal(icd9InReferenceCode(c("100.1", "200"), "200", isShort = TRUE), c(FALSE, TRUE))

  expect_identical(icd9InReferenceCode(c("2501", "25001", "999"), c("V101", "250"), isShort = TRUE),
                   c(TRUE, TRUE, FALSE))
})

test_that("deprecated - icd9InReferenceCode works for numeric codes with major < 100", {
  expect_true(icd9InReferenceCode("1", "1", isShort = TRUE))
  expect_true(icd9InReferenceCode(" 1", "01", isShort = TRUE))
  expect_true(icd9InReferenceCode("1 ", "001", isShort = TRUE))
  expect_true(icd9InReferenceCode("01", "1", isShort = TRUE))
  expect_true(icd9InReferenceCode(" 01", "01", isShort = TRUE))
  expect_true(icd9InReferenceCode("001", "1", isShort = TRUE))
  expect_true(icd9InReferenceCode("001", "001", isShort = TRUE))

  expect_identical(icd9InReferenceCode("1", "001", isShort = TRUE),
                   icd9InReferenceCode("01", "001", isShort = TRUE))
  expect_identical(icd9InReferenceCode("1", "001", isShort = TRUE),
                   icd9InReferenceCode("001", "001", isShort = TRUE))
  expect_identical(icd9InReferenceCode("1", "1", isShort = TRUE),
                   icd9InReferenceCode("01", "1", isShort = TRUE))
  expect_identical(icd9InReferenceCode("1", "1", isShort = TRUE),
                   icd9InReferenceCode("001", "1", isShort = TRUE))
  expect_identical(icd9InReferenceCode("0011", "001", isShort = TRUE),
                   icd9InReferenceCode("0011", "1", isShort = TRUE))
  expect_identical(icd9InReferenceCode("0011", "001", isShort = TRUE),
                   icd9InReferenceCode("0011", "01", isShort = TRUE))
})

test_that("deprecated - sorting char vectors", {
  expect_equal(icd9SortShort(c("3", "02", "001", "003")), c("001", "02", "3", "003"))
  expect_equal(icd9Sort(c("1", "V02", "V1", "E003"), isShort = TRUE), c("1", "V1", "V02", "E003"))
  expect_equal(icd9SortShort(c("0032", "0288", "0019", "0031")), c("0019", "0031", "0032", "0288"))
  expect_equal(icd9Sort(c("V251", "V25", "E0039", "E003"), isShort = TRUE), c("V25", "V251", "E003", "E0039"))
  expect_equal(icd9Sort(c("V25.1", "V25", "E003.9", "E003"), isShort = FALSE), c("V25", "V25.1", "E003", "E003.9"))
  expect_equal(icd9SortDecimal(c("E1.1", "V2.2", "E001", "V02.1", "999.99", "88.8", "77")),
               c("77", "88.8", "999.99", "V02.1", "V2.2", "E001", "E1.1"))
})

test_that("deprecated - sorting char factors", {
  expect_equal(icd9SortShort(factor(c("3", "02", "001", "003"))),
               factor(c("001", "02", "3", "003")))
  expect_equal(icd9Sort(factor(c("1", "V02", "V1", "E003")), isShort = TRUE),
               factor(c("1", "V1", "V02", "E003")))
  expect_equal(icd9SortShort(factor(c("0032", "0288", "0019", "0031"))),
               factor(c("0019", "0031", "0032", "0288")))
  expect_equal(icd9Sort(factor(c("V251", "V25", "E0039", "E003")), isShort = TRUE),
               factor(c("V25", "V251", "E003", "E0039")))
  expect_equal(icd9Sort(factor(c("V25.1", "V25", "E003.9", "E003")), isShort = FALSE),
               factor(c("V25", "V25.1", "E003", "E003.9")))
  expect_equal(icd9SortDecimal(factor(c("E1.1", "V2.2", "E001", "V02.1", "999.99", "88.8", "77"))),
               factor(c("77", "88.8", "999.99", "V02.1", "V2.2", "E001", "E1.1")))
})

test_that("deprecated - sysdata.rda is okay", {
  skip_slow_tests()
  lknames <- c("icd9NShort", "icd9VShort", "icd9EShort",
               "icd9NShortBillable", "icd9VShortBillable", "icd9EShortBillable",
               "icd9NShortReal", "icd9VShortReal", "icd9EShortReal",
               "icd9_sources")

  sysdat <- generateSysData(save = FALSE)
  expect_true(all(lknames %in% names(sysdat)))

  expect_lt(length(icd9NShortBillable), length(icd9NShortReal))
  expect_lt(length(icd9VShortBillable), length(icd9VShortReal))
  expect_lt(length(icd9EShortBillable), length(icd9EShortReal))
  expect_lt(length(icd9NShortReal), length(icd9NShort))
  expect_lt(length(icd9VShortReal), length(icd9VShort))
  expect_lt(length(icd9EShortReal), length(icd9EShort))
  expect_true(all(icd9NShortReal %in% icd9NShort))
  expect_true(all(icd9VShortReal %in% icd9VShort))
  expect_true(all(icd9EShortReal %in% icd9EShort))

})
