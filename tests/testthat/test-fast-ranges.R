context("icd9 ranges")

test_that("expand icd9 range definition", {
  expect_equal(
    icd9ExpandRangeShort("4012", "40145", onlyReal = FALSE, omitParents = FALSE),
    sort(c("4012", "40120", "40121", "40122", "40123", "40124", "40125",
           "40126", "40127", "40128", "40129", "4013", "40130", "40131",
           "40132", "40133", "40134", "40135", "40136", "40137", "40138",
           "40139", "4014", "40140", "40141", "40142", "40143", "40144", "40145")))
# the following tests the unimplemented omitParents = TRUE
#   expect_equal(
#     icd9ExpandRangeShort("4012", "40145", omitParents = TRUE),
#     sort(c("4012", "40120", "40121", "40122", "40123", "40124", "40125",
#            "40126", "40127", "40128", "40129", "4013", "40130", "40131",
#            "40132", "40133", "40134", "40135", "40136", "40137", "40138",
#            "40139", "40140", "40141", "40142", "40143", "40144", "40145")))
#
  expect_equal(icd9ExpandRangeShort("40100", "40101", onlyReal = FALSE), c("40100", "40101"))
  expect_equal(icd9ExpandRangeShort("40108", "40109", onlyReal = FALSE), c("40108", "40109"))
  expect_equal(icd9ExpandRangeShort("40198", "40199", onlyReal = FALSE), c("40198", "40199"))
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
  expect_equal(
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
  # implied strongly.
  expect_equal(icd9ExpandRangeShort("043", "0449", onlyReal = FALSE),
               icd9ExpandRangeShort("043", "044", onlyReal = FALSE))
  expect_equal(icd9ExpandRangeShort("043", "04499", onlyReal = FALSE),
               icd9ExpandRangeShort("043", "044", onlyReal = FALSE))
  expect_equal(
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

  expect_equal(icd9ExpandRangeShort("401", "40102", onlyReal = FALSE),
               c("401", "4010", "40100", "40101", "40102"))
  expect_equal(icd9ExpandRangeShort("V1000", "V1002", onlyReal = FALSE),
               c("V1000", "V1001", "V1002"))
  # although we don't usually return parents whose scope overlaps the upper
  # limit, if the range specification already has this 'anomaly', we just roll
  # with it.
  expect_equal(icd9ExpandRangeShort("V10", "V1001", onlyReal = FALSE),
               c("V10", "V100", "V1000", "V1001"))
  # but we cap off the upper range correctly:
  expect_equal(icd9ExpandRangeShort("V1009", "V101", onlyReal = FALSE),
               c("V1009", "V101", "V1010", "V1011",
                 "V1012", "V1013", "V1014", "V1015",
                 "V1016", "V1017", "V1018", "V1019"))

  # should fail despite end being 'longer' than start
  expect_error(icd9ExpandRangeShort("V10", " V1 "))

  # only works with single range
  expect_error(icd9ExpandRangeShort(c("10", "20"), c("11", "21")))

})

test_that("E code ranges", {
  expect_equal(icd9ExpandRangeShort("E9501", "E9502", onlyReal = FALSE), c("E9501", "E9502"))
  expect_equal(icd9ExpandRangeShort("E950", "E9509", onlyReal = FALSE),
               c("E950", "E9500", "E9501", "E9502", "E9503", "E9504",
                 "E9505", "E9506", "E9507", "E9508", "E9509")
  )
  expect_equal(icd9AddLeadingZeroesShort("E9501"), "E9501")
})

test_that("major ranges", {
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

  expect_equal(icd9ExpandRangeMajor("E99", "E101", onlyReal = FALSE),
               c("E099", "E100", "E101"))
  skip("now need to test major ranges with onlyReal = TRUE")
})

test_that("range bugs", {
  # these both failed - need zero padding for the first
  expect_equal( ("042 " %i9s% "043 ")[1], "042")
  expect_equal( ("42" %i9s% "043 ")[1], "042")
  expect_true("345" %nin% ("3420 " %i9s% "3449 "))

  expect_equal("042.11" %i9da% "042.13", c("042.11", "042.12", "042.13"))

  # no presumption that missing leading zeroes will be missed on output:
  expect_equal("42.11" %i9da% "42.13", c("042.11", "042.12", "042.13"))
})


test_that("icd9ExpandMinor: invalid", {
  expect_error(icd9ExpandMinor(c(1, 2)))
  expect_error(icd9ExpandMinor("JACK"))
  expect_error(icd9ExpandMinor(c(123)))
  expect_error(icd9ExpandMinor(c("123")))
  expect_error(icd9ExpandMinor(c(1, 2), isE = TRUE))
  expect_error(icd9ExpandMinor("JACK"), isE = TRUE)
  expect_error(icd9ExpandMinor(c(123), isE = TRUE))
})

test_that("icd9ExpandMinor: valid", {
  expect_equal(length(icd9ExpandMinor("", isE = FALSE)), 111)
  expect_equal(length(icd9ExpandMinor("", isE = TRUE)), 11)
  expect_identical(icd9ExpandMinor("00", isE = FALSE), "00")
  expect_identical(icd9ExpandMinor("9", isE = FALSE), as.character(c(9, 90:99)))
  expect_equal(icd9ExpandMinor("00", isE = TRUE), NA_character_)
  expect_identical(icd9ExpandMinor("9", isE = TRUE), "9")

  expect_equal(icd9ExpandMinor("0", isE = TRUE), "0")
  expect_equal(icd9ExpandMinor("9", isE = TRUE), "9")
  expect_equal(icd9ExpandMinor("", isE = TRUE), c("", as.character(0:9)))

})

test_that("icd9ChildrenDecimal invalid", {
  skip("not testing invalid inputs to everything")
  # too long major
  expect_error(icd9ChildrenDecimal("1234"))
  # too long V major
  expect_error(icd9ChildrenDecimal("V234"))
  # too long major
  expect_error(icd9ChildrenDecimal("v101.1"))
  # wrong in three ways
  expect_error(icd9ChildrenDecimal("e123.45"))
  # not number or V format
  expect_error(icd9ChildrenDecimal("JACK"))
  # too long minor
  expect_error(icd9ChildrenDecimal("123.456"))
  # too long major and minor
  expect_error(icd9ChildrenDecimal("9123.456"))

})

test_that("icd9ChildrenDecimal valid input", {
  expect_equal(
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
  expect_equal(
    icd9ChildrenDecimal("010.0", onlyReal = FALSE),
    append("010.0", paste("010.0", 0:9, sep = "")))
  #expect_equal(icd9ChildrenDecimal("010.0"), icd9ChildrenDecimal("10.0"))

})

test_that("icd9ChildrenShort invalid input", {
  expect_equal(icd9ChildrenShort(character()), character())
  skip("moving away from checking errors on input to every function")
  expect_error(icd9Children(list(c(1, 2), "crap"))) # junk
  # too long
  expect_error(icd9ChildrenShort("123456"))
  # even longer with whitespace
  expect_error(icd9ChildrenShort(" 09123456 "))
  # too long V
  expect_error(icd9ChildrenShort("V12345"))
  # too long E
  expect_error(icd9ChildrenShort("E987654"))
  # not number or V or E format
  expect_error(icd9ChildrenShort("JACK"))

  expect_error(icd9ChildrenShort())
})

test_that("icd9ChildrenShort valid input", {
  expect_equal(icd9ChildrenShort("V100", onlyReal = FALSE),
               paste("V100", c("", 0:9), sep = ""))
  #expect_equal(toupper(icd9ChildrenShort("v100")), icd9Children("V100"))
  expect_equal(icd9ChildrenShort(" V100 ", onlyReal = FALSE),
               icd9ChildrenShort("V100", onlyReal = FALSE))
  expect_equal(icd9ChildrenShort("0100", onlyReal = FALSE),
               paste("0100", c("", 0:9), sep = ""))
  expect_equal(icd9ChildrenShort("1", onlyReal = FALSE)[1], "001")
  expect_equal(icd9ChildrenShort("01", onlyReal = FALSE)[1], "001")
  expect_equal(icd9ChildrenShort("001", onlyReal = FALSE)[1], "001")
  expect_equal(icd9ChildrenShort("023", onlyReal = FALSE)[1], "023")
  expect_equal(icd9ChildrenShort("23", onlyReal = FALSE)[1], "023")
  expect_equal(icd9ChildrenShort("456", onlyReal = FALSE)[1], "456")
  expect_equal(
    icd9ChildrenShort("E100", onlyReal = FALSE),
    c("E100", "E1000", "E1001", "E1002", "E1003", "E1004",
      "E1005", "E1006", "E1007", "E1008", "E1009"))
  expect_equal(icd9ChildrenShort("390", onlyReal = TRUE), "390")
})

test_that("condense ranges which do consense", {
  expect_equal(
    icd9CondenseToMajorShort(icd9ChildrenShort("123", onlyReal = TRUE),
                        onlyReal = TRUE),
    "123")
  expect_equal(
    icd9CondenseToMajorShort(icd9ChildrenShort("1", onlyReal = TRUE),
                        onlyReal = TRUE),
    "001")
  for (or1 in c(TRUE, FALSE)) {
    for (or2 in c(TRUE, FALSE)) {
      expect_equal(
        icd9CondenseToMajorShort(icd9ChildrenShort("00321", onlyReal = or1),
                            onlyReal = or2),
        "00321", info = paste(or1, or2))
      expect_equal(
        icd9CondenseToMajorShort(icd9ChildrenShort("V1221", onlyReal = or1),
                            onlyReal = or2),
        "V1221", info = paste(or1, or2))
    }
  }
  expect_equal(icd9CondenseToMajorShort(icd9ChildrenShort("V12", onlyReal = TRUE),
                                   onlyReal = TRUE), "V12")
  expect_equal(icd9CondenseToMajorShort(icd9ChildrenShort("V12", onlyReal = FALSE),
                                   onlyReal = FALSE), "V12")
})

test_that("condense ranges that don't condense at all", {
  expect_equal(
    sort(icd9CondenseToMajorShort(icd9ChildrenShort("123", onlyReal = TRUE),
                             onlyReal = FALSE)),
    sort(icd9ChildrenShort("123", onlyReal = TRUE)))
  # the parent "1000" is not included.
  expect_equal(sort(icd9CondenseToMajorShort(as.character(10000:10009),
                                        onlyReal = FALSE)),
               as.character(10000:10009))
  # missing 10009
  expect_equal(sort(icd9CondenseToMajorShort(c("1000", as.character(10000:10008)),
                                        onlyReal = FALSE)),
               c("1000", as.character(10000:10008)))
})

test_that("condense range invalid data" ,{
  # no automatic validation, so we just get it back. We can validate separately.
  # e.g. "turnpike" %>% icd9GetRealShort
  expect_equal(icd9CondenseToMajorShort("turnpike", onlyReal = FALSE), "turnpike")
  # TODO more tests here
})

test_that("icd9InReferenceCode", {
  # if the input icd9 code is definitely junk, e.g. longer than 5 char, or 0 char, we get an NA back
  expect_equal(icd9InReferenceCode("bratwurst", "123", isShort = TRUE), FALSE)
  # but if, with absolutely minimal validation, it could be okay...
  expect_equal(icd9InReferenceCode("bdn", "123", isShort = FALSE), FALSE)
  #expect_error(n <- icd9InReferenceCode(c("421", "123"), c("123", "V432"))) #

  expect_equal(icd9InReferenceCode("123.45", "12345", isShort = FALSE, isShortReference = TRUE), TRUE)
  expect_equal(icd9InReferenceCode("123.45", "123.45", isShort = FALSE, isShortReference = FALSE), TRUE)
  expect_equal(icd9InReferenceCode("12345", "123.45", isShort = TRUE, isShortReference = FALSE), TRUE)
  expect_equal(icd9InReferenceCode("12345", "12345", isShort = TRUE, isShortReference = TRUE), TRUE)

  expect_equal(icd9InReferenceCode(c("421", "123"), c("123", "V"), isShort = FALSE),
               c(FALSE, TRUE))
  expect_equal(icd9InReferenceCode(c("421", "123"), c("123", "V"), isShort = FALSE, isShortReference = FALSE),
               c(FALSE, TRUE))
  expect_equal(icd9InReferenceCode(c("421", "123"), c("123", "V42"), isShort = FALSE),
               c(FALSE, TRUE))
  expect_equal(icd9InReferenceCode(c("123", "V43210"), c("421", "123"), isShort = TRUE),
               c(TRUE, FALSE))
  expect_equal(icd9InReferenceCode(c("100.1", "200"), "200", isShort = TRUE), c(FALSE, TRUE))

  expect_identical(icd9InReferenceCode(c("2501", "25001", "999"), c("V101", "250"), isShort = TRUE),
                   c(TRUE, TRUE, FALSE))

  # the function must not care whether either the mapping codes or the test
  # codes are zero padded:

  # basic tests for numeric codes with major < 100
  expect_true(icd9InReferenceCode("1", "1", isShort = TRUE))
  expect_true(icd9InReferenceCode("1", "01", isShort = TRUE))
  expect_true(icd9InReferenceCode("1", "001", isShort = TRUE))
  expect_true(icd9InReferenceCode("01", "1", isShort = TRUE))
  expect_true(icd9InReferenceCode("01", "01", isShort = TRUE))
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

  # create a large set of valid icd9 codes (of the integer variety)
  #ni = runif(n=1000000, min=100, max=99999)
  # ni <- c(36211,
  # seq(from=399, to=406, by=1),
  # seq(from=4009, to=4061, by=1),
  # seq(from=40099, to=40601, by=1)
  # )
  # i <- as.character(ni)
  #
  # #TODO: expect_identical(icd9InReferenceCode(i, c("V101", "250")),
  # #  ni>=25000 & ni<25100)
  # expect_identical(
  # icd9InReferenceCode(icd9Codes=i,
  #   baseCodes=c(401, 402, 403, 404, 405, 362.11),
  #   icd9CodeShort= TRUE, baseCodeShort=FALSE),
  # (ni>=401 & ni<406) | (ni>=4010 & ni<4060) | (ni>=40100 & ni<40600) |
  #   (ni==36211)
  # )
  #
  # # all the same again with an additional NA value:
  # # should give exactly the same result
  # i[length(i)+1] <- NA
  # idm = (i==250 | (i>=2500 & i<=2509) | (i>=25000 & i<=25099) |
  # (i==3572) | (i>=35720 & i<35730) |'
  # '
  # (i==36641) |
  # (i==3620) | (i>=36200 & i<36210)
  # )
  # expect_identical(icd9InReferenceCode(i, c(250, 3572, 36641, 3620)), idm)
  # expect_identical(
  # icd9InReferenceCode(i, c(401, 402, 403, 404, 405, 362.11)),
  # (i>=401 & i<406) | (i>=4010 & i<4060) | (i>=40100 & i<40600) | (i==36211)
  # )
})

test_that("sort works as expected", {
  expect_equal(icd9SortShort(c("3", "02", "001", "003")), c("001", "02", "3", "003"))
  expect_equal(icd9Sort(c("1", "V02", "V1", "E003"), isShort = TRUE), c("1", "E003", "V1", "V02"))
  expect_equal(icd9SortShort(c("0032", "0288", "0019", "0031")), c("0019", "0031", "0032", "0288"))
  expect_equal(icd9Sort(c("V251", "V25", "E0039", "E003"), isShort = TRUE), c("E003", "E0039", "V25", "V251"))
  expect_equal(icd9Sort(c("V25.1", "V25", "E003.9", "E003"), isShort = FALSE), c("E003", "E003.9", "V25", "V25.1"))
  expect_equal(icd9SortDecimal(c("E1.1", "V2.2", "E001", "V02.1", "999.99", "88.8", "77")),
               c("77", "88.8", "999.99", "E001", "E1.1", "V02.1", "V2.2"))
})

test_that("sysdata.rda is okay", {
  lknames <- c("icd9NShort", "icd9VShort", "icd9EShort",
    "icd9NShortReal", "icd9VShortReal", "icd9EShortReal")

  expect_that(sysdat <- icd9GenerateSysData(do.save = FALSE), testthat::not(throws_error()))
  expect_equal(names(sysdat), lknames)
})
