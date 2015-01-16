context("icd9 ranges")

test_that("expand icd9 range definition", {
  expect_equal(
    sort(icd9ExpandRangeShort("4012", "40145")),
    sort(c("4012", "40120", "40121", "40122", "40123", "40124", "40125",
           "40126", "40127", "40128", "40129", "4013", "40130", "40131",
           "40132", "40133", "40134", "40135", "40136", "40137", "40138",
           "40139", "40140", "40141", "40142", "40143", "40144", "40145")))

  expect_equal(icd9ExpandRangeShort("40100", "40101"), c("40100", "40101"))
  expect_equal(icd9ExpandRangeShort("40108", "40109"), c("40108", "40109"))
  expect_equal(icd9ExpandRangeShort("40198", "40199"), c("40198", "40199"))
  # must be in ICD9 order, otherwise error:
  expect_error(icd9ExpandRangeShort("40109", "40108"))
  expect_error(icd9ExpandRangeShort("4019", "4018"))
  expect_error(icd9ExpandRangeShort("402", "401"))
  expect_error(icd9ExpandRangeShort("2", "1"))
  expect_error(icd9ExpandRangeShort("002", "1", invalidAction = "stop"))
  expect_error(icd9ExpandRangeShort("002", "001"))
  expect_error(icd9ExpandRangeShort("2", "001"))
  expect_error(icd9ExpandRangeShort("4010", "401"))

  expect_equal(icd9ExpandRangeShort("4280", "4280 "), "4280")
  expect_equal(icd9ExpandRangeShort("4280", " 4280 "), "4280")
  expect_equal(icd9ExpandRangeShort("4280", "4280 "), "4280")
  expect_equal(icd9ExpandRangeShort("4280", " 4280"), "4280")
  expect_equal(icd9ExpandRangeShort("4280", "4280"), "4280")
  expect_equal(icd9ExpandRangeShort("4280 ", "4280"), "4280")
  expect_equal(icd9ExpandRangeShort(" 4280", "4280"), "4280")
  expect_equal(icd9ExpandRangeShort(" 4280 ", "4280"), "4280")

  # the range 44100-4419 from the AHRQ found a gap in the code.
  expect_equal(
    sort(icd9ExpandRangeShort("4410", "4412")),
    sort(c("4410", icd9ExpandRangeShort("44100", "4412")))
  )

  expect_equal(icd9ExpandRangeShort("401", "401"),
               icd9Children("401", isShort = TRUE))
  expect_equal(icd9ExpandRangeShort("401", "402"),
               icd9Children(c("401", "402"), isShort = TRUE))
  # the next two cases cover the HIV ranges in the co-morbidities, wherein the
  # final code is included, in which case the parent ("044" in this case) is
  # implied strongly.
  expect_equal(icd9ExpandRangeShort("043", "0449"),
               icd9ExpandRangeShort("043", "044"))
  expect_equal(icd9ExpandRangeShort("043", "04499"),
               icd9ExpandRangeShort("043", "044"))
  expect_equal(
    icd9ExpandRangeShort("401", "402"),
    c("401", "4010", "4011", "4012", "4013", "4014", "4015", "4016",
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
      "40299")
  )

  expect_equal(icd9ExpandRangeShort("401", "40102"),
               c("401", "4010", "40100", "40101", "40102"))
  expect_equal(icd9ExpandRangeShort("V1000", "V1002"),
               c("V1000", "V1001", "V1002"))
  # although we don't usually return parents whose scope overlaps the upper
  # limit, if the range specification already has this 'anomaly', we just roll
  # with it.
  expect_equal(icd9ExpandRangeShort("V10", "V1001"),
               c("V10", "V100", "V1000", "V1001"))
  # but we cap off the upper range correctly:
  expect_equal(icd9ExpandRangeShort("V1009", "V101"),
               c("V1009", "V101", "V1010", "V1011",
                 "V1012", "V1013", "V1014", "V1015",
                 "V1016", "V1017", "V1018", "V1019"))

  # should fail despite end being 'longer' than start
  expect_error(icd9ExpandRangeShort("V10", " V1 "))

  # only works with single range
  expect_error(icd9ExpandRangeShort(c("10", "20"), c("11", "21")))

})

test_that("E code ranges", {
  expect_equal(icd9ExpandRangeShort("E9501", "E9502"), c("E9501", "E9502"))
  expect_equal(icd9ExpandRangeShort("E950", "E9509"),
               c("E950", "E9500", "E9501", "E9502", "E9503", "E9504",
                 "E9505", "E9506", "E9507", "E9508", "E9509")
  )
  expect_error(icd9ExpandRangeShort("E95012", "E95013", invalidAction = "stop"))
  expect_equal(icd9AddLeadingZeroesShort("E9501"), "E9501")
})

test_that("major ranges", {
  resall <- "E000" %i9mj% "E999"
  expect_equal(length(resall), 1000)
  expect_true("E000" %in% resall)
  expect_true("E123" %in% resall)
  expect_true("E999" %in% resall)
  resallbut <- "E1" %i9mj% "E998"
  expect_equal(length(resallbut), 998)
  expect_false("E000" %in% resallbut)
  expect_true("E001" %in% resallbut)
  expect_true("E123" %in% resallbut)
  expect_true("E998" %in% resallbut)
  expect_false("E999" %in% resallbut)

  expect_equal("E99" %i9mj% "E101", c("E099", "E100", "E101"))
})

test_that("preceding and subsequent minors", {
  expect_equal(icd9PrecedingMinors("20", isE = FALSE),
               c("0", "00", "01", "02", "03", "04", "05", "06", "07", "08",
                 "09", "1", "10", "11", "12", "13", "14", "15", "16", "17",
                 "18", "19", "20"))
  expect_equal(sort(icd9PrecedingMinors("19", isE = FALSE)),
               c("0", "00", "01", "02", "03", "04", "05", "06", "07", "08",
                 "09", "10", "11", "12", "13", "14", "15", "16", "17", "18",
                 "19"))

  expect_equal(icd9PrecedingMinors("2", isE = TRUE), c("0", "1", "2"))
  expect_equal(icd9SubsequentMinors("8", isE = TRUE), c("8", "9"))
  expect_error(icd9PrecedingMinors("00", isE = TRUE))
  expect_error(icd9SubsequentMinors("00", isE = TRUE))

  expect_equal(
    sort(icd9SubsequentMinors("08", isE = FALSE)),
    sort(c("08", "09",
           "1", "10", "11", "12", "13", "14", "15", "16", "17", "18",
           "19", "2", "20", "21", "22", "23", "24", "25", "26", "27", "28",
           "29", "3", "30", "31", "32", "33", "34", "35", "36", "37", "38",
           "39", "4", "40", "41", "42", "43", "44", "45", "46", "47", "48",
           "49", "5", "50", "51", "52", "53", "54", "55", "56", "57", "58",
           "59", "6", "60", "61", "62", "63", "64", "65", "66", "67", "68",
           "69", "7", "70", "71", "72", "73", "74", "75", "76", "77", "78",
           "79", "8", "80", "81", "82", "83", "84", "85", "86", "87", "88",
           "89", "9", "90", "91", "92", "93", "94", "95", "96", "97", "98",
           "99")))

  # these both failed - need zero padding for the first
  expect_equal( ("042 " %i9s% "043 ")[1], "042")
  expect_equal( ("42" %i9s% "043 ")[1], "042")
  "3420 " %i9s% "3449 "

  expect_equal("042.11" %i9d% "042.13", c("042.11", "042.12", "042.13"))

  # no presumption that missing leading zeroes will be missed on output:
  expect_equal("42.11" %i9d% "42.13", c("042.11", "042.12", "042.13"))
})


test_that("icd9ExpandMinor: invalid", {
  expect_error(icd9ExpandMinor(c(1, 2)))
  expect_error(icd9ExpandMinor("JACK"))
  expect_error(icd9ExpandMinor(c(123)))
  expect_error(icd9ExpandMinor(c("123")))
  expect_error(icd9ExpandMinor(c(1, 2), isE = TRUE))
  expect_error(icd9ExpandMinor("JACK"), isE = TRUE)
  expect_error(icd9ExpandMinor("00", isE = TRUE))
  expect_error(icd9ExpandMinor(c(123), isE = TRUE))
  expect_error(icd9ExpandMinor(c("123"), isE = TRUE))
})

test_that("icd9ExpandMinor: valid", {
  expect_equal(length(icd9ExpandMinor("", isE = FALSE)), 111)
  expect_equal(length(icd9ExpandMinor("", isE = TRUE)), 11)
  expect_identical(icd9ExpandMinor("00", isE = FALSE), "00")
  expect_identical(icd9ExpandMinor("9", isE = FALSE), as.character(c(9, 90:99)))
  expect_error(icd9ExpandMinor("00", isE = TRUE))
  expect_identical(icd9ExpandMinor("9", isE = TRUE), "9")

  expect_equal(icd9ExpandMinor("0", isE = TRUE), "0")
  expect_equal(icd9ExpandMinor("9", isE = TRUE), "9")
  expect_equal(icd9ExpandMinor("", isE = TRUE), c("", as.character(0:9)))

})

test_that("icd9ChildrenDecimal", {
  # too long major
  expect_error(icd9ChildrenDecimal("1234", invalidAction = "stop"))
  # too long V major
  expect_error(icd9ChildrenDecimal("V234", invalidAction = "stop"))
  # too long major
  expect_error(icd9ChildrenDecimal("v101.1", invalidAction = "stop"))
  # wrong in three ways
  expect_error(icd9ChildrenDecimal("e123.45", invalidAction = "stop"))
  # not number or V format
  expect_error(icd9ChildrenDecimal("JACK", invalidAction = "stop"))
  # too long minor
  expect_error(icd9ChildrenDecimal("123.456", invalidAction = "stop"))
  # too long major and minor
  expect_error(icd9ChildrenDecimal("9123.456", invalidAction = "stop"))

  expect_equal(
    icd9ChildrenDecimal("V10.0"),
    append("V10.0", paste("V10.0", 0:9, sep = "")))
  expect_equal(
    toupper(icd9ChildrenDecimal("v10.0")),
    icd9ChildrenDecimal("V10.0"))
  expect_equal(
    icd9ChildrenDecimal(" V10.0 "),
    icd9ChildrenDecimal("V10.0"))
  expect_equal(
    icd9ChildrenDecimal("10.0"),
    icd9ChildrenDecimal("010.0"))
  expect_equal(
    icd9ChildrenDecimal("010.0"),
    append("010.0", paste("010.0", 0:9, sep = "")))
  #expect_equal(icd9ChildrenDecimal("010.0"), icd9ChildrenDecimal("10.0"))

})

test_that("icd9ChildrenShort invalid input", {
  expect_error(icd9Children(list(c(1, 2), "crap"))) # junk
  # too long
  expect_error(icd9ChildrenShort("123456", invalidAction = "stop"))
  # even longer with whitespace
  expect_error(icd9ChildrenShort(" 09123456 ", invalidAction = "stop"))
  # too long V
  expect_error(icd9ChildrenShort("V12345", invalidAction = "stop"))
  # too long E
  expect_error(icd9ChildrenShort("E987654", invalidAction = "stop"))
  # not number or V or E format
  expect_error(icd9ChildrenShort("JACK", invalidAction = "stop"))

  expect_error(icd9ChildrenShort())
  expect_equal(icd9ChildrenShort(character()), character())
})

test_that("icd9ChildrenShort valid input", {
  expect_equal(icd9ChildrenShort("V100"), paste("V100", c("", 0:9), sep = ""))
  #expect_equal(toupper(icd9ChildrenShort("v100")), icd9Children("V100"))
  expect_equal(icd9ChildrenShort(" V100 "), icd9ChildrenShort("V100"))
  expect_equal(icd9ChildrenShort("0100"), paste("0100", c("", 0:9), sep = ""))
  expect_equal(icd9ChildrenShort("1")[1], "001")
  expect_equal(icd9ChildrenShort("01")[1], "001")
  expect_equal(icd9ChildrenShort("001")[1], "001")
  expect_equal(icd9ChildrenShort("023")[1], "023")
  expect_equal(icd9ChildrenShort("23")[1], "023")
  expect_equal(icd9ChildrenShort("456")[1], "456")
  expect_equal(
    icd9ChildrenShort("E100"),
    c("E100", "E1000", "E1001", "E1002", "E1003", "E1004",
      "E1005", "E1006", "E1007", "E1008", "E1009"))
  expect_equal(icd9ChildrenShort("390", onlyReal = TRUE), "390")
})

# TODO:
test_that("onlyReal flag", {

})

test_that("condense ranges which do consense", {
  expect_equal(
    icd9CondenseToMajor(icd9ChildrenShort("123", onlyReal = TRUE),
                        onlyReal = TRUE),
    "123")
  expect_equal(
    icd9CondenseToMajor(icd9ChildrenShort("1", onlyReal = TRUE),
                        onlyReal = TRUE),
    "001")
  for (or1 in c(TRUE, FALSE)) {
    for (or2 in c(TRUE, FALSE)) {
      expect_equal(
        icd9CondenseToMajor(icd9ChildrenShort("00321", onlyReal = or1),
                            onlyReal = or2),
        "00321", info = paste(or1, or2))
      expect_equal(
        icd9CondenseToMajor(icd9ChildrenShort("V1221", onlyReal = or1),
                            onlyReal = or2),
        "V1221", info = paste(or1, or2))
    }
  }
  expect_equal(icd9CondenseToMajor(icd9ChildrenShort("V12", onlyReal = TRUE),
                                   onlyReal = TRUE), "V12")
  expect_equal(icd9CondenseToMajor(icd9ChildrenShort("V12", onlyReal = FALSE),
                                   onlyReal = FALSE), "V12")
})

test_that("condense ranges that don't condense at all", {
  expect_equal(
    sort(icd9CondenseToMajor(icd9ChildrenShort("123", onlyReal = TRUE),
                        onlyReal = FALSE)),
    sort(icd9ChildrenShort("123", onlyReal = TRUE)))
  # the parent "1000" is not included.
  expect_equal(sort(icd9CondenseToMajor(as.character(10000:10009),
                                        onlyReal = FALSE)),
               as.character(10000:10009))
  # missing 10009
  expect_equal(sort(icd9CondenseToMajor(c("1000", as.character(10000:10008)),
                                        onlyReal = FALSE)),
               c("1000", as.character(10000:10008)))
})

test_that("condense range invalid data" ,{
  #expect_equal(icd9CondenseToMajor("turnpike"), NA) # ? or character(0)
  expect_error(icd9CondenseToMajor("turnpike", invalidAction = "stop"))
  # TODO more tests here
})

test_that("icd9InReferenceCodeLong", {
  expect_error(icd9InReferenceCode())
  expect_error(icd9InReferenceCode(""))
  expect_error(icd9InReferenceCode(123)) # no numbers, just strings
  expect_error(icd9InReferenceCode("salami"))
  expect_error(icd9InReferenceCode("bratwurst", "123",
                                   invalidAction = "stop"))
  expect_equal(icd9InReferenceCode("bratwurst", "123",
                                   invalidAction = "silent"), FALSE)
  # base codes definitely must be valid regardless of invalidAction = "stop"
  # (for the input data): so do generate errors
  expect_error(icd9InReferenceCode("421", "boudin",
                                   invalidActionReference = TRUE))
  #expect_error(n <- icd9InReferenceCode(c("421", "123"), c("123", "V432"))) #
  #invalid V code # automatically validate? TODO invalid reference code
  expect_error(icd9InReferenceCode(c("421", "123"),
                                   c("123", "E"),
                                   invalidActionReference = TRUE))
  # invalid reference code
  expect_error(icd9InReferenceCode(c("421", "123"),
                                   c("123", "V"),
                                   invalidActionReference = TRUE))
  expect_equal(icd9InReferenceCode(c("421", "123"), c("123", "V42")),
               c(FALSE, TRUE))
  expect_error(icd9InReferenceCode(c("123", "V43210"),
                                   c("421", "123"),
                                   invalidAction = "stop"))
  expect_equal(icd9InReferenceCode(c("123", "V43210"),
                                   c("421", "123"),
                                   invalidAction = "silent"),
               c(TRUE, FALSE))

  # not expecting decimals in input data (default is always short)
  expect_error(icd9InReferenceCode(c("100.1", "200"), "200",
                                   invalidAction = "stop"))
  expect_equal(icd9InReferenceCode(c("100.1", "200"), "200",
                                   invalidAction = "silent"), c(FALSE, TRUE))

  expect_identical(icd9InReferenceCode(c("2501", "25001", "999"),
                                       c("V101", "250")),
                   c(TRUE, TRUE, FALSE))

  # the function must not care whether either the mapping codes or the test
  # codes are zero padded:

  # basic tests for numeric codes with major < 100
  expect_true(icd9InReferenceCode("1", "1"))
  expect_true(icd9InReferenceCode("1", "01"))
  expect_true(icd9InReferenceCode("1", "001"))
  expect_true(icd9InReferenceCode("01", "1"))
  expect_true(icd9InReferenceCode("01", "01"))
  expect_true(icd9InReferenceCode("001", "1"))
  expect_true(icd9InReferenceCode("001", "001"))

  expect_identical(icd9InReferenceCode("1", "001"),
                   icd9InReferenceCode("01", "001"))
  expect_identical(icd9InReferenceCode("1", "001"),
                   icd9InReferenceCode("001", "001"))
  expect_identical(icd9InReferenceCode("1", "1"),
                   icd9InReferenceCode("01", "1"))
  expect_identical(icd9InReferenceCode("1", "1"),
                   icd9InReferenceCode("001", "1"))
  expect_identical(icd9InReferenceCode("0011", "001"),
                   icd9InReferenceCode("0011", "1"))
  expect_identical(icd9InReferenceCode("0011", "001"),
                   icd9InReferenceCode("0011", "01"))

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
