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

context("icd9 ranges")

test_that("expand icd9 range definition", {
  expect_equal_no_icd(
    icd_expand_range(short_code = TRUE, "4012", "40145",
                     defined = FALSE,
                     ex_ambig_start = FALSE,
                     ex_ambig_end = FALSE),
    sort(c("4012", "40120", "40121", "40122", "40123", "40124", "40125",
           "40126", "40127", "40128", "40129", "4013", "40130", "40131",
           "40132", "40133", "40134", "40135", "40136", "40137", "40138",
           "40139", "4014", "40140", "40141", "40142", "40143", "40144", "40145")))
  expect_equal_no_icd(
    icd_expand_range(short_code = TRUE, "4012", "40145",
                     defined = FALSE,
                     ex_ambig_start = TRUE,
                     ex_ambig_end = TRUE),
    sort(c("4012", "40120", "40121", "40122", "40123", "40124", "40125",
           "40126", "40127", "40128", "40129", "4013", "40130", "40131",
           "40132", "40133", "40134", "40135", "40136", "40137", "40138",
           "40139", NULL, "40140", "40141", "40142", "40143", "40144", "40145")))
  # the following tests the unimplemented omitParents = TRUE
  #   expect_equal(
  #     icd_expand_range(short_code = TRUE, "4012", "40145", omitParents = TRUE),
  #     sort(c("4012", "40120", "40121", "40122", "40123", "40124", "40125",
  #            "40126", "40127", "40128", "40129", "4013", "40130", "40131",
  #            "40132", "40133", "40134", "40135", "40136", "40137", "40138",
  #            "40139", "40140", "40141", "40142", "40143", "40144", "40145")))
  #
  expect_equal_no_icd(
    icd_expand_range(short_code = TRUE, "40100", "40101", defined = FALSE),
    c("40100", "40101")
  )
  expect_equal_no_icd(
    icd_expand_range.icd9("40108", "40109", short_code = TRUE, defined = FALSE),
    c("40108", "40109")
  )
  expect_equal_no_icd(
    icd_expand_range(short_code = TRUE, "40198", "40199", defined = FALSE),
    c("40198", "40199")
  )
  # must be in ICD9 order, otherwise error:
  expect_error(icd_expand_range(short_code = TRUE, "40109", "40108"))
  expect_error(icd_expand_range(short_code = TRUE, "4019", "4018"))
  expect_error(icd_expand_range(short_code = TRUE, "402", "401"))
  expect_error(icd_expand_range(short_code = TRUE, "2", "1"))
  expect_error(icd_expand_range(short_code = TRUE, "002", "1"))
  expect_error(icd_expand_range(short_code = TRUE, "002", "001"))
  expect_error(icd_expand_range(short_code = TRUE, "2", "001"))
  expect_error(icd_expand_range(short_code = TRUE, "4010", "401"))

  expect_identical(
    icd_expand_range(short_code = TRUE, " 4280 ", " 4280 ", defined = FALSE),
    icd_expand_range(short_code = TRUE, "4280", "4280", defined = FALSE)
  )


  # the range 44100-4419 from the AHRQ found a gap in the code.
  expect_equal_no_icd(
    sort(icd_expand_range(short_code = TRUE, "4410", "4412", defined = FALSE)),
    sort(c("4410", icd_expand_range(short_code = TRUE, "44100", "4412", defined = FALSE)))
  )

  expect_equal(
    icd_expand_range(short_code = TRUE, "401", "401", defined = FALSE),
    sort(icd_children.icd9("401", short_code = TRUE, defined = FALSE))
  )
  # expand range should already be sorted. do i want to sort children by default
  # or with an option?
  expect_equal(
    icd_expand_range(short_code = TRUE, "401", "402", defined = FALSE),
    sort(icd_children.icd9(c("401", "402"), short_code = TRUE, defined = FALSE))
  )
  # the next two cases cover the HIV ranges in the co-morbidities, wherein the
  # final code is included, in which case the parent ("044" in this case) is
  # implied strongly. CAN'T EXPECT RANGE TO ACCOUNT FOR THIS, but we can make next test work with flag as follows:
  expect_equal(icd_expand_range(short_code = TRUE, "043", "0449", defined = FALSE, ex_ambig_end = FALSE),
               icd_expand_range(short_code = TRUE, "043", "044", defined = FALSE))
  expect_equal(icd_expand_range(short_code = TRUE, "043", "04499", defined = FALSE),
               icd_expand_range(short_code = TRUE, "043", "044", defined = FALSE))
  expect_equal_no_icd(
    icd_expand_range(short_code = TRUE, "401", "402", defined = FALSE),
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

  expect_equal_no_icd(
    icd_expand_range(short_code = TRUE, "401", "40102", defined = FALSE,
                     ex_ambig_start = FALSE,
                     ex_ambig_end = FALSE),
    c("401", "4010", "40100", "40101", "40102")
  )

  # only works with single range
  expect_error(icd_expand_range(short_code = TRUE, c("10", "20"), c("11", "21")))

  # found bugs when expanding Injury and Poisoning chapter.
  icd_expand_range(short_code = TRUE, "997", "998")
  expect_false("999" %in% icd_expand_range(short_code = TRUE, "998", "998", defined = FALSE))
  expect_false("009" %in% icd_expand_range(short_code = TRUE, "8", "8", defined = FALSE))

})

test_that("expand range defined by two four digit codes includes last code", {
  expect_true("1991" %in% icd_expand_range(short_code = TRUE, "1960", "1991", defined = FALSE))
  expect_true("19919" %in% icd_expand_range(short_code = TRUE, "1960", "1991", defined = FALSE))
})

test_that("expand range worker gives correct ranges", {
  # really, the test is against icd9ExpandRange family, but we can isolate an
  # error to the sub-function
  expect_equal(
    expand_range_worker("V10", "V1001", lookup = icd:::icd9VShort,
                        defined = TRUE, ex_ambig_start = FALSE, ex_ambig_end = FALSE),
    c("V10", "V100", "V1000", "V1001"))
})

test_that("V code with ambiguous parent", {
  # although we don't usually return parents whose scope overlaps the upper
  # limit, if the range specification already has this 'anomaly', we just roll
  # with it.

  # the default should be to include the stated higher-level code, and enough
  # descendants just to reach the specified codes, but not all the children of
  # the higher-level code.
  expect_equal_no_icd(
    icd_expand_range.icd9(short_code = TRUE, "V10", "V1001",
                          defined = FALSE, ex_ambig_start = FALSE, ex_ambig_end = FALSE),
    c("V10", "V100", "V1000", "V1001")
  )
  expect_equal_no_icd(
    icd_expand_range.icd9(short_code = TRUE, "V10", "V1001", defined = FALSE),
    c("V1000", "V1001")
  )
})

test_that("V code ranges", {
  expect_equal_no_icd(
    icd_expand_range.icd9(short_code = TRUE, "V1000", "V1002", defined = FALSE),
    c("V1000", "V1001", "V1002"))
  # but we cap off the upper range correctly:
  expect_equal_no_icd(
    icd_expand_range.icd9(short_code = TRUE, "V1009", "V101", defined = FALSE),
    c("V1009", "V101", "V1010", "V1011",
      "V1012", "V1013", "V1014", "V1015",
      "V1016", "V1017", "V1018", "V1019")
  )
  # and with narrower top end
  expect_equal_no_icd(
    icd_expand_range.icd9(short_code = TRUE, "V1009", "V1011",
                          defined = FALSE, ex_ambig_start = TRUE, ex_ambig_end = TRUE),
    c("V1009", "V1010", "V1011"))
  expect_equal_no_icd(
    icd_expand_range.icd9(short_code = TRUE, "V1009", "V1011",
                          defined = FALSE, ex_ambig_start = FALSE, ex_ambig_end = FALSE),
    c("V1009", "V101", "V1010", "V1011"))
  # but include those pesky parents when requested:
  expect_true(
    all(c("V10", "V100") %in% icd_expand_range.icd9(short_code = TRUE, "V099", "V1011", defined = FALSE,
                                                    ex_ambig_start = FALSE, ex_ambig_end = FALSE)))

  # should fail despite end being 'longer' than start
  expect_error(icd_expand_range.icd9(short_code = TRUE, "V10", " V1 "))

  expect_identical(
    icd_expand_range.icd9(short_code = TRUE, "V1009", "V101", defined = FALSE),
    icd_expand_range.icd9(short_code = TRUE, "V1009", "V1019", defined = FALSE)
  )

  # failed similar test in Elixhauser mapping generation.
  expect_false("V11" %in% icd_expand_range.icd9(short_code = FALSE, "V10.89", "V10.9", defined = FALSE))
  expect_false("V11" %in% icd_expand_range.icd9(short_code = FALSE, "V10.89", "V10.99", defined = FALSE))

})

test_that("E code ranges", {
  expect_equal_no_icd(
    icd_expand_range.icd9(short_code = TRUE, "E9501", "E9502", defined = FALSE),
    c("E9501", "E9502"))
  expect_equal_no_icd(
    icd_expand_range.icd9(short_code = TRUE, "E950", "E9509", defined = FALSE),
    c("E950", "E9500", "E9501", "E9502", "E9503", "E9504",
      "E9505", "E9506", "E9507", "E9508", "E9509")
  )
  expect_equal(icd9_add_leading_zeroes("E9501", short_code = TRUE), "E9501")
})

test_that("major ranges", {
  resall <- icd_expand_range_major.icd9("E000", "E999", defined = FALSE)
  expect_equal(length(resall), 1000)
  expect_true("E000" %in% resall)
  expect_true("E123" %in% resall)
  expect_true("E999" %in% resall)
  resallbut <- icd_expand_range_major.icd9("E001", "E998", defined = FALSE)
  expect_equal(length(resallbut), 998)

  expect_identical(
    "111" %i9mj% "789",
    icd_expand_range_major.icd9("111", "789", defined = TRUE)
  )

  expect_identical(
    "V01" %i9mj% "V99",
    icd_expand_range_major.icd9("V01", "V99", defined = TRUE)
  )

  expect_identical(
    "E001" %i9mj% "E998",
    icd_expand_range_major.icd9("E001", "E998", defined = TRUE)
  )

  expect_false("E000" %in% resallbut)
  expect_true("E001" %in% resallbut)
  expect_true("E123" %in% resallbut)
  expect_true("E998" %in% resallbut)
  expect_false("E999" %in% resallbut)

  expect_equal_no_icd(icd_expand_range_major.icd9("E99", "E101", defined = FALSE),
                      c("E099", "E100", "E101"))
})

test_that("range bugs", {
  # these both failed - need zero padding for the first
  expect_equal_no_icd( ("042 " %i9s% "042 ")[1], "042")
  expect_equal_no_icd( ("42" %i9s% "042 ")[1], "042")
  expect_true("345" %nin% ("3420 " %i9s% "3449 "))

  expect_identical("042.11" %i9da% "042.13", icd9(as.icd_decimal_diag(c("042.11", "042.12", "042.13"))))

  # no presumption that missing leading zeroes will be missed on output:
  expect_equivalent("42.11" %i9da% "42.13", icd9(as.icd_decimal_diag(c("042.11", "042.12", "042.13"))))
})

test_that("range doesn't include higher level parent github issue #14", {
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

test_that("ranges can include ambiguous parents, optionally", {
  expect_equal_no_icd(
    icd_expand_range.icd9("01006", "01010", defined = TRUE, ex_ambig_start = TRUE, ex_ambig_end = TRUE),
    c("01006", "01010"))
  expect_equal_no_icd(
    icd_expand_range.icd9("01006", "01010", defined = TRUE, ex_ambig_start = FALSE, ex_ambig_end = FALSE),
    c("01006", "0101", "01010"))
  expect_equal_no_icd(
    icd_expand_range.icd9("01006", "01010", defined = FALSE, ex_ambig_start = TRUE, ex_ambig_end = TRUE),
    c("01006", "01007", "01008", "01009", "01010"))
  expect_equal_no_icd(
    icd_expand_range.icd9("01006", "01010", defined = FALSE, ex_ambig_start = FALSE, ex_ambig_end = FALSE),
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

test_that("range abbrevs", {
  expect_identical(icd_expand_range.icd9("123", "123.6", short_code = FALSE, defined = FALSE),
                   "123" %i9da% "123.6")
  expect_identical(icd_expand_range.icd9("123", "123.6", short_code = FALSE, defined = TRUE),
                   "123" %i9d% "123.6")
  expect_identical(icd_expand_range.icd9("1234", "125", short_code = TRUE, defined = FALSE),
                   "1234" %i9sa% "125")
})

test_that("icd_expand_minor.icd9: invalid", {
  expect_error(icd_expand_minor.icd9(c(1, 2)))
  expect_error(icd_expand_minor.icd9("JACK"))
  expect_error(icd_expand_minor.icd9(c(123)))
  expect_error(icd_expand_minor.icd9(c("123")))
  expect_error(icd_expand_minor.icd9(c(1, 2), is_e = TRUE))
  # just do convenient, not comprehensive validation here:
  expect_error(icd_expand_minor.icd9("999", is_e = FALSE), "minor of more than two characters")
  expect_error(icd_expand_minor.icd9("JACK", is_e = TRUE), "characters")
  expect_error(icd_expand_minor.icd9("J", is_e = FALSE), "unrecognized minor")
  expect_error(icd_expand_minor.icd9(c(123), is_e = TRUE))
  expect_error(icd_expand_minor.icd9("00", is_e = TRUE))
  expect_error(icd_expand_minor.icd9("E0000", is_e = TRUE))
  expect_error(icd_expand_minor.icd9("99", is_e = TRUE))
})

test_that("icd_expand_minor.icd9: valid", {
  expect_equal(length(icd_expand_minor.icd9("", is_e = FALSE)), 111)
  expect_equal(length(icd_expand_minor.icd9("", is_e = TRUE)), 11)
  expect_identical(icd_expand_minor.icd9("00", is_e = FALSE), "00")
  expect_identical(icd_expand_minor.icd9("9", is_e = FALSE), as.character(c(9, 90:99)))
  expect_identical(icd_expand_minor.icd9("9", is_e = TRUE), "9")

  expect_equal(icd_expand_minor.icd9("0", is_e = TRUE), "0")
  expect_equal(icd_expand_minor.icd9("9", is_e = TRUE), "9")
  expect_equal(icd_expand_minor.icd9("", is_e = TRUE), c("", as.character(0:9)))

})

test_that("icd9 expand minor dispatches", {
  expect_identical(icd_expand_minor(icd9("")),
                   icd_expand_minor.icd9(""))

  # do NOT want to dispatch when we don't know ICD-9 vs 10 vs 10-CM etc., in
  # this internal-only function
  expect_error(icd_expand_minor("0"))
})

test_that("icd9 children decimal with valid input", {
  expect_equal_no_icd(
    icd_children.icd9(short_code = FALSE, "V10.0", defined = FALSE),
    append("V10.0", paste("V10.0", 0:9, sep = "")))
  expect_equal(
    toupper(icd_children.icd9(short_code = FALSE, "v10.0", defined = FALSE)),
    icd_children.icd9(short_code = FALSE, "V10.0", defined = FALSE))
  expect_equal(
    icd_children.icd9(short_code = FALSE, " V10.0 ", defined = FALSE),
    icd_children.icd9(short_code = FALSE, "V10.0", defined = FALSE))
  expect_equal(
    icd_children.icd9(short_code = FALSE, "10.0", defined = FALSE),
    icd_children.icd9(short_code = FALSE, "010.0", defined = FALSE))
  expect_equal_no_icd(
    icd_children.icd9(short_code = FALSE, "010.0", defined = FALSE),
    append("010.0", paste("010.0", 0:9, sep = "")))
  expect_equal(icd_children.icd9(short_code = FALSE, "010.0"), icd_children.icd9(short_code = FALSE, "10.0"))
})

test_that("icd9 children short with valid input", {
  expect_equal_no_icd(icd_children.icd9(short_code = TRUE, "V100", defined = FALSE),
                      paste("V100", c("", 0:9), sep = ""))
  expect_equal(icd_children.icd9(short_code = TRUE, "v100"),
               icd_children.icd9("V100"))
  expect_equal(icd_children.icd9(short_code = TRUE, " V100 ", defined = FALSE),
               icd_children.icd9(short_code = TRUE, "V100", defined = FALSE))
  expect_equal_no_icd(icd_children.icd9(short_code = TRUE, "0100", defined = FALSE),
                      paste("0100", c("", 0:9), sep = ""))
  expect_equal_no_icd(icd_children.icd9(short_code = TRUE, "1", defined = FALSE)[1], "001")
  expect_equal_no_icd(icd_children.icd9(short_code = TRUE, "01", defined = FALSE)[1], "001")
  expect_equal_no_icd(icd_children.icd9(short_code = TRUE, "001", defined = FALSE)[1], "001")
  expect_equal_no_icd(icd_children.icd9(short_code = TRUE, "023", defined = FALSE)[1], "023")
  expect_equal_no_icd(icd_children.icd9(short_code = TRUE, "23", defined = FALSE)[1], "023")
  expect_equal_no_icd(icd_children.icd9(short_code = TRUE, "456", defined = FALSE)[1], "456")
  expect_equal_no_icd(
    icd_children.icd9(short_code = TRUE, "E100", defined = FALSE),
    c("E100", "E1000", "E1001", "E1002", "E1003", "E1004",
      "E1005", "E1006", "E1007", "E1008", "E1009"))
  expect_equal_no_icd(icd_children.icd9(short_code = TRUE, "390", defined = TRUE), "390")
})

test_that("is_short doesn't cause error with icd_children when redundantly specified", {
  expect_identical(
    icd_children("10201", defined = FALSE),
    icd_children("10201", short_code = TRUE, defined = FALSE)
  )
})

test_that("icd_in_reference_code deals with bad input", {
  expect_equal(icd_in_reference_code(NA, "123", short_code = TRUE), FALSE) # arguable: could return NA here
  expect_equal(icd_in_reference_code("", "123", short_code = TRUE), FALSE)
  expect_equal(icd_in_reference_code("bratwurst", "123", short_code = TRUE), FALSE)
})

test_that("icd_in_reference_code mixing short and decimals", {
  expect_equal(icd_in_reference_code("123.45", "12345", short_code = FALSE, short_reference = TRUE), TRUE)
  expect_equal(icd_in_reference_code("12345", "123.45", short_code = TRUE, short_reference = FALSE), TRUE)
})

test_that("icd_in_reference_code test code format matches mapping format", {
  expect_equal(icd_in_reference_code("123.45", "123.45", short_code = FALSE, short_reference = FALSE), TRUE)
  expect_equal(icd_in_reference_code("12345", "12345", short_code = TRUE, short_reference = TRUE), TRUE)
})

test_that("icd_in_reference_code produces the right length output", {
  expect_equal(
    icd_in_reference_code(c("100", "200"), c("400", "100", "200", "300"), short_code = TRUE),
    c(TRUE, TRUE)
  )
  expect_equal(
    icd_in_reference_code(c("100", "99"), c("400", "100", "200", "300"), short_code = TRUE),
    c(TRUE, FALSE)
  )
  expect_equal(
    icd_in_reference_code(c("99", "100"), c("400", "100", "200", "300"), short_code = TRUE),
    c(FALSE, TRUE)
  )
  expect_equal(
    icd_in_reference_code(c("99", "97"), c("400", "100", "200", "300"), short_code = TRUE),
    c(FALSE, FALSE)
  )
  expect_equal(
    icd_in_reference_code(c("100", "200", "99"), c("400", "100", "200", "300"), short_code = TRUE),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("icd_in_reference_code", {

  expect_equal(icd_in_reference_code(c("421", "123"), c("123", "V"), short_code = FALSE), c(FALSE, TRUE))
  expect_equal(
    icd_in_reference_code(c("421", "123"), c("123", "V"), short_code = FALSE, short_reference = FALSE),
    c(FALSE, TRUE))
  expect_equal(icd_in_reference_code(c("421", "123"), c("123", "V42"), short_code = FALSE), c(FALSE, TRUE))
  expect_equal(icd_in_reference_code(c("123", "V43210"), c("421", "123"), short_code = TRUE), c(TRUE, FALSE))
  expect_equal(icd_in_reference_code(c("100.1", "200"), "200", short_code = TRUE), c(FALSE, TRUE))

  expect_identical(icd_in_reference_code(c("2501", "25001", "999"), c("V101", "250"), short_code = TRUE),
                   c(TRUE, TRUE, FALSE))
})

test_that("icd_in_reference_code works for numeric codes with major < 100", {
  expect_true(icd_in_reference_code("1", "1", short_code = TRUE))
  expect_true(icd_in_reference_code(" 1", "01", short_code = TRUE))
  expect_true(icd_in_reference_code("1 ", "001", short_code = TRUE))
  expect_true(icd_in_reference_code("01", "1", short_code = TRUE))
  expect_true(icd_in_reference_code(" 01", "01", short_code = TRUE))
  expect_true(icd_in_reference_code("001", "1", short_code = TRUE))
  expect_true(icd_in_reference_code("001", "001", short_code = TRUE))

  expect_identical(icd_in_reference_code("1", "001", short_code = TRUE),
                   icd_in_reference_code("01", "001", short_code = TRUE))
  expect_identical(icd_in_reference_code("1", "001", short_code = TRUE),
                   icd_in_reference_code("001", "001", short_code = TRUE))
  expect_identical(icd_in_reference_code("1", "1", short_code = TRUE),
                   icd_in_reference_code("01", "1", short_code = TRUE))
  expect_identical(icd_in_reference_code("1", "1", short_code = TRUE),
                   icd_in_reference_code("001", "1", short_code = TRUE))
  expect_identical(icd_in_reference_code("0011", "001", short_code = TRUE),
                   icd_in_reference_code("0011", "1", short_code = TRUE))
  expect_identical(icd_in_reference_code("0011", "001", short_code = TRUE),
                   icd_in_reference_code("0011", "01", short_code = TRUE))
})

test_that("sorting char vectors", {
  expect_equal(icd_sort.icd9(short_code = TRUE, c("3", "02", "001", "003")),
               c("001", "02", "3", "003"))
  # same with dispatch
  expect_equal(icd_sort(short_code = TRUE, c("3", "02", "001", "003")),
               c("001", "02", "3", "003"))

  expect_equal(icd_sort.icd9(c("1", "V02", "V1", "E003"), short_code = TRUE),
               c("1", "V1", "V02", "E003"))
  expect_equal(icd_sort.icd9(short_code = TRUE, c("0032", "0288", "0019", "0031")),
               c("0019", "0031", "0032", "0288"))
  expect_equal(icd_sort.icd9(c("V251", "V25", "E0039", "E003"), short_code = TRUE),
               c("V25", "V251", "E003", "E0039"))
  expect_equal(icd_sort.icd9(c("V25.1", "V25", "E003.9", "E003"), short_code = FALSE),
               c("V25", "V25.1", "E003", "E003.9"))
  expect_equal(icd_sort.icd9(short_code = FALSE,
                             c("E1.1", "V2.2", "E001", "V02.1", "999.99", "88.8", "77")),
               c("77", "88.8", "999.99", "V02.1", "V2.2", "E001", "E1.1"))
})

test_that("sorting char factors", {
  expect_equal(icd_sort.icd9(short_code = TRUE, factor(c("3", "02", "001", "003"))),
               factor(c("001", "02", "3", "003")))
  expect_equal(icd_sort.icd9(factor(c("1", "V02", "V1", "E003")), short_code = TRUE),
               factor(c("1", "V1", "V02", "E003")))
  expect_equal(icd_sort.icd9(short_code = TRUE, factor(c("0032", "0288", "0019", "0031"))),
               factor(c("0019", "0031", "0032", "0288")))
  expect_equal(icd_sort.icd9(factor(c("V251", "V25", "E0039", "E003")), short_code = TRUE),
               factor(c("V25", "V251", "E003", "E0039")))
  expect_equal(icd_sort.icd9(factor(c("V25.1", "V25", "E003.9", "E003")), short_code = FALSE),
               factor(c("V25", "V25.1", "E003", "E003.9")))
  expect_equal(icd_sort.icd9(short_code = FALSE,
                             factor(c("E1.1", "V2.2", "E001", "V02.1", "999.99", "88.8", "77"))),
               factor(c("77", "88.8", "999.99", "V02.1", "V2.2", "E001", "E1.1")))
})

test_that("sysdata.rda is okay", {
  skip_slow_tests()
  lknames <- c("icd9NShort", "icd9VShort", "icd9EShort",
               "icd9NShortBillable", "icd9VShortBillable", "icd9EShortBillable",
               "icd9NShortReal", "icd9VShortReal", "icd9EShortReal",
               "icd9_sources", ".nc")

  sysdat <- generate_sysdata(save_data = FALSE)
  expect_equal(names(sysdat), lknames)

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

test_that("expand ICD-9 range character class deals with short vs long types", {
  expect_true(all(icd_is_valid.icd9(
    res_a <- icd_expand_range("410", "410.8"), short_code = FALSE)))
  expect_true(all(icd_is_valid.icd9(
    res_b <- icd_expand_range("410", "4108"), short_code = TRUE)))
  expect_true(is.icd_decimal_diag(res_a))
  expect_true(is.icd_short_diag(res_b))
})


test_that("sort icd10", {

  expect_equal(icd_sort(as.icd10cm(c("Z00", "A99", "J4C"))), as.icd10cm(c("A99", "J4C", "Z00")))
  expect_equal(icd_sort(as.icd10cm("Z04")), as.icd10cm("Z04"))

})

test_that("chapter major expansion works for basic test", {
  expect_equal(
    icd9_expand_chapter_majors("Diseases Of The Respiratory System"),
    icd9(c("460", "461", "462", "463", "464", "465", "466",
           "467", "468", "469", "470", "471", "472", "473", "474", "475",
           "476", "477", "478", "479", "480", "481", "482", "483", "484",
           "485", "486", "487", "488", "489", "490", "491", "492", "493",
           "494", "495", "496", "497", "498", "499", "500", "501", "502",
           "503", "504", "505", "506", "507", "508", "509", "510", "511",
           "512", "513", "514", "515", "516", "517", "518", "519"))
  )
})

test_that("chapter major expansion works for basic test", {
  expect_equal(
    icd9_expand_sub_chapter_majors("Other Accidents"),
    icd9(c("E916", "E917", "E918", "E919", "E920", "E921", "E922",
           "E923", "E924", "E925", "E926", "E927", "E928"))
  )
})
