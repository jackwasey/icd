context("test icd9 package")

test_that("all the ICD9 codes for the comorbidities I declare are valid", {
  
  #expect_true(all(icd9ValidDecimal(unlist(ahrq))))
  #expect_true(all(icd9ValidDecimal(unlist(deyo))))
  #expect_true(all(icd9ValidDecimal(unlist(elixhauser))))
  
})

test_that("appendZeroToNine", {
  expect_error(appendZeroToNine(list(a=c(1,2)))) # random crap
  expect_identical(appendZeroToNine("1"), as.character(10:19))
  expect_identical(appendZeroToNine(1), as.character(10:19))
  expect_identical(appendZeroToNine(""), as.character(0:9))
  expect_identical(sort(appendZeroToNine(c("1","2"))), as.character(c(10:19,20:29)))
  expect_identical(sort(appendZeroToNine(c("","9"))), as.character(c(0:9,90:99)))
})

test_that("icd9ExpandMinor", {
  expect_error(icd9ExpandMinor(c(1,2)))
  expect_error(icd9ExpandMinor("JACK"))
  expect_error(icd9ExpandMinor(c(123)))
  expect_error(naVal <- icd9ExpandMinor(c("123")))
  expect_equal(length(icd9ExpandMinor("")), 111)
  expect_identical(icd9ExpandMinor("00"), "00")
  expect_identical(icd9ExpandMinor("9"), as.character(c(9,90:99)))
})

test_that("icd9ExpandBaseCodeDecimal", {
  #expect_error(icd9ExpandBaseCode(list(c(1,2),"crap"))) # junk
  expect_error(icd9ExpandBaseCodeDecimal("1234")) # too long major
  expect_error(icd9ExpandBaseCodeDecimal("V234")) # too long V major
  expect_error(icd9ExpandBaseCodeDecimal("v101.1")) # too long major
  expect_error(icd9ExpandBaseCodeDecimal("JACK")) # not number or V format
  expect_error(icd9ExpandBaseCodeDecimal("123.456")) # too long minor
  expect_error(icd9ExpandBaseCodeDecimal("9123.456")) # too long major and minor
  expect_equal(icd9ExpandBaseCodeDecimal("V10.0"), append("V10.0",paste("V10.0",0:9, sep="")))
  expect_equal(toupper(icd9ExpandBaseCodeDecimal("v10.0")), icd9ExpandBaseCodeDecimal("V10.0"))
  expect_equal(icd9ExpandBaseCodeDecimal(" V10.0  "), icd9ExpandBaseCodeDecimal("V10.0"))
  expect_equal(icd9ExpandBaseCodeDecimal("10.0"), append("010.0",paste("010.0",0:9, sep="")))
  #expect_equal(icd9ExpandBaseCodeDecimal("010.0"), icd9ExpandBaseCodeDecimal("10.0"))
  
})

test_that("icd9ExpandBaseCodeShort", {
  #expect_error(icd9ExpandBaseCode(list(c(1,2),"crap"))) # junk
  expect_error(icd9ExpandBaseCodeShort("123456")) # too long
  expect_error(icd9ExpandBaseCodeShort(" 09123456  ")) # even longer
  expect_error(icd9ExpandBaseCodeShort("V12345")) # too long V
  expect_error(icd9ExpandBaseCodeShort("JACK")) # not number or V or E format
  expect_equal(icd9ExpandBaseCodeShort("V100"), paste("V100",c("",0:9), sep=""))
  #expect_equal(toupper(icd9ExpandBaseCodeShort("v100")), icd9ExpandBaseCode("V100"))
  expect_equal(icd9ExpandBaseCodeShort(" V100  "), icd9ExpandBaseCodeShort("V100"))
  expect_equal(icd9ExpandBaseCodeShort("0100"), paste("0100",c("",0:9), sep=""))
  expect_equal(icd9ExpandBaseCodeShort("1")[1], "001")
  expect_equal(icd9ExpandBaseCodeShort("23")[1], "023")
  expect_equal(icd9ExpandBaseCodeShort("456")[1], "456")
})

test_that("icd9ValidDecimal", {
  expect_error(icd9ValidDecimal(list(1230,c(12323,2323), c("nonesnseses"))))
  expect_error(icd9ValidDecimal(c()))
  expect_false(icd9ValidDecimal(""))
  expect_true(icd9ValidDecimal("0"))
  expect_true(icd9ValidDecimal("00"))
  expect_true(icd9ValidDecimal("000"))
  expect_true(icd9ValidDecimal("0.00")) # maybe warn for this one?
  expect_true(icd9ValidDecimal("000.00"))
  expect_false(icd9ValidDecimal("0000"))
  expect_true(icd9ValidDecimal("100"))
  expect_true(icd9ValidDecimal("010")) # a bit weird, but should validate
  expect_true(icd9ValidDecimal("01")) # not enough zero padding? but not ambiguous.
  expect_true(icd9ValidDecimal("1.1")) # a subtype of cholera
  expect_true(icd9ValidDecimal("01.1")) # a subtype of cholera
  expect_true(icd9ValidDecimal("001.1")) # a subtype of cholera
  expect_true(icd9ValidDecimal("01.10"))
  expect_true(icd9ValidDecimal("999.99"))
  expect_true(icd9ValidDecimal("V55.55"))
  expect_true(icd9ValidDecimal("E999"))
  expect_true(icd9ValidDecimal(" E999"))
  expect_true(icd9ValidDecimal("E999 "))
  expect_true(icd9ValidDecimal("E999. "))
  expect_true(icd9ValidDecimal("V99. "))
  expect_true(icd9ValidDecimal("V1.")) # looks horrible, but not ambiguous
  expect_true(icd9ValidDecimal("  22.2 "))
  expect_true(icd9ValidDecimal("  33 "))
  expect_true(icd9ValidDecimal("01.10"))
  expect_true(icd9ValidDecimal("01.10"))
  expect_true(icd9ValidDecimal("100."))
  expect_false(icd9ValidDecimal("V0"))
  expect_false(icd9ValidDecimal("E1"))
  expect_false(icd9ValidDecimal("."))
  expect_false(icd9ValidDecimal("E."))
  expect_false(icd9ValidDecimal("V."))
  expect_false(icd9ValidDecimal("V.1"))
  expect_false(icd9ValidDecimal("E.1"))
  expect_false(icd9ValidDecimal("E1."))
  expect_false(icd9ValidDecimal("V123"))
  expect_false(icd9ValidDecimal("E00000"))
  expect_false(icd9ValidDecimal("E000.00"))
  expect_false(icd9ValidDecimal("E100")) # E800-E999 defined
  expect_false(icd9ValidDecimal("E0999")) # this is 099.9 which is not a valid E code
  # TODO: more E validation
  expect_equal(icd9ValidDecimal(c("100","chestnut")), c(T,F))
  #expect_warning(naVal <- icd9ValidDecimal("100,200")) # note this is a string with two numbers in it...
  #expect_equal(naVal, NA)
  expect_equal(icd9ValidDecimal(c("two","things")), c(F,F))
  expect_equal(icd9ValidDecimal(c("100","200.55","V01.11")), c(T,T,T))
  expect_equal(icd9ValidDecimal(as.factor(c("0", "100", "222.22", "100", "1", "0"))), c(T, T, T, T, T, T))
  expect_equal(icd9ValidDecimal(c("10.1","100","999.99","0.01")), c(T,T,T,T))
})

test_that("icd9ValidShort", {
  expect_error(icd9ValidShort(list(1230,c(12323,2323), c("nonesnseses"))))
  expect_false(icd9ValidShort("jericho"))
  expect_false(icd9ValidShort(""))
  expect_false(icd9ValidShort("123456"))
  #expect_false(icd9ValidShort("0"))
  expect_false(icd9ValidShort("11.22")) # am not expecting decimal points
  expect_true(icd9ValidShort("0"))
  expect_equal(icd9ValidShort(c("0","00","000","0000","00000")), c(T,T,T,T,T))
  expect_true(icd9ValidShort("12345"))
  expect_true(icd9ValidShort("12 "))
  expect_error(icd9ValidShort(1))
  expect_equal(icd9ValidShort(c("99999","1")),c(T,T))
  expect_false(icd9ValidShort("10.1"))
  expect_equal(icd9ValidShort(c("1", "100", "222.22")), c(T,T,F))
  expect_equal(icd9ValidShort(as.factor(c("1", "100", "222.22", "100", "1.1"))), c(T, T, F, T, F))
  
  expect_false(icd9ValidShort("V"))
  expect_false(icd9ValidShort("VV"))
  expect_false(icd9ValidShort("V0"))
  expect_false(icd9ValidShort("V1"))
  expect_false(icd9ValidShort("V00000"))
  expect_false(icd9ValidShort("V123456"))
  expect_true(icd9ValidShort("V234"))
  expect_true(icd9ValidShort(" V45"))

  expect_false(icd9ValidShort("0E111"))
  expect_false(icd9ValidShort(" E0345"))
  
})

test_that("icd9ToComorbidLong", {
  expect_error(icd9ToComorbid())
  expect_error(icd9ToComorbid(""))
  expect_error(icd9ToComorbid(123)) # no numbers, just strings
  expect_error(icd9ToComorbid("salami"))
  expect_warning(n <- icd9ToComorbid("bratwurst", "123"))
  expect_equal(n, FALSE)
  expect_error(icd9ToComorbid("421", "boudin")) # base codes definitely must be valid: so generate errors
  expect_error(n <- icd9ToComorbid(c("421","123"), c("123", "V432"))) # invalid V code
  expect_error(n <- icd9ToComorbid(c("421","123"), c("123", "E"))) # invalid 
  expect_error(n <- icd9ToComorbid(c("421","123"), c("123", "V"))) # invalid 
  expect_equal(icd9ToComorbid(c("421","123"),c("123","V42")), c(F, T))
  expect_warning(n <- icd9ToComorbid(c("123","V43210"), c("421","123")))
  expect_equal(n, c(T,F))
  expect_warning(n <- icd9ToComorbid(c("100.1", "200"), "200")) # not expecting decimals in input data
  expect_equal(n, c(F,T))
  
  expect_identical(icd9ToComorbid(c("2501", "25001", "999"), c("V10.1","250")), c(T, T, F))
  
  #ni = runif(n=1000000, min=100, max=99999) # create a large set of valid icd9 codes (of the integer variety)
  #   ni <- c(36211,
  #           seq(from=399,to=406, by=1),
  #           seq(from=4009,to=4061, by=1),
  #           seq(from=40099,to=40601, by=1)
  #   )
  #   i <- as.character(ni)
  #   
  #   #TODO: expect_identical(icd9ToComorbid(i, c("V101","250")), ni>=25000 & ni<25100)
  #   expect_identical(
  #     icd9ToComorbid(icd9codes=i, baseCodes=c(401,402,403,404,405,362.11), icd9codeShort=TRUE, baseCodeShort=FALSE), 
  #     (ni>=401 & ni<406) | (ni>=4010 & ni<4060) | (ni>=40100 & ni<40600) | (ni==36211)
  #   )
  #   
  #   # all the same again with an additional NA value: should give exactly the same result
  #   i[length(i)+1] <- NA
  #   idm = (i==250 |  (i>=2500 & i<=2509) | (i>=25000 & i<=25099) | 
  #            (i==3572) | (i>=35720 & i<35730) |'
  #          '
  #          (i==36641) | 
  #            (i==3620) | (i>=36200 & i<36210)
  #   )
  #   expect_identical(icd9ToComorbid(i, c(250,3572,36641,3620)), idm)
  #   expect_identical(
  #     icd9ToComorbid(i, c(401,402,403,404,405,362.11)), 
  #     (i>=401 & i<406) | (i>=4010 & i<4060) | (i>=40100 & i<40600) | (i==36211)
  #   )
})

test_that("extract decimal parts", { 
  expect_equal(icd9ExtractPartsDecimal("0"), data.frame(major="000", minor="", stringsAsFactors=F)) # zero is valid, means no code.
  expect_equal(icd9ExtractPartsDecimal("V1.2"), data.frame(major="V1", minor="2", stringsAsFactors=F))
  expect_equal(icd9ExtractPartsDecimal("1.1"), data.frame(major="001", minor="1", stringsAsFactors=F))
  expect_equal(icd9ExtractPartsDecimal("22.22"), data.frame(major="022", minor="22", stringsAsFactors=F))
  expect_equal(icd9ExtractPartsDecimal("333.3"), data.frame(major="333", minor="3", stringsAsFactors=F))
  expect_equal(icd9ExtractPartsDecimal("444"), data.frame(major="444", minor="", stringsAsFactors=F))
  expect_equal(icd9ExtractPartsDecimal("444", minorEmpty=NA_character_), data.frame(major="444", minor=NA_character_, stringsAsFactors=F))
  expect_equal(icd9ExtractPartsDecimal("444", minorEmpty=""), data.frame(major="444", minor="", stringsAsFactors=F))
  expect_equal(icd9ExtractPartsDecimal("12.3", padMajor=T), data.frame(major="012", minor="3", stringsAsFactors=F))
  expect_equal(icd9ExtractPartsDecimal(c("9.9", "88.88", "777.6")), 
               data.frame(
                 major=c("009", "088", "777"),
                 minor=c("9", "88", "6"),
                 stringsAsFactors=F
               )
  )
  expect_equal(icd9ExtractPartsDecimal(c("9.9", "88", "777.6"), minorEmpty=NA), 
               data.frame(
                 major=c("009", "088", "777"),
                 minor=c("9", NA, "6"),
                 stringsAsFactors=F
               )
  )
  expect_equal(icd9ExtractPartsDecimal(c("1", "g", "", "991.23"), validate=T, minorEmpty=NA),
               data.frame(
                 major=c("001", NA, NA, "991"),
                 minor=c(NA, NA, NA, "23"),
                 stringsAsFactors=F
               )
  )               
  expect_equal(icd9ExtractPartsDecimal(c("1", "g", "", "991.23"), validate=F, minorEmpty=NA),
               data.frame(
                 major=c("001", "g", NA, "991"),
                 minor=c(NA, NA, NA, "23"),
                 stringsAsFactors=F
               )
  )               
})

test_that("icd9 decimal to short form", {
  
  expect_equal(icd9DecimalToShort("1"), "001")
  expect_equal(icd9DecimalToShort("1.1"), "0011")
  expect_equal(icd9DecimalToShort("1.23"), "00123")
  expect_equal(icd9DecimalToShort("81"), "081")
  expect_equal(icd9DecimalToShort("81.1"), "0811")
  expect_equal(icd9DecimalToShort("81.23"), "08123")
  expect_equal(icd9DecimalToShort("991"), "991")
  expect_equal(icd9DecimalToShort("991.1"), "9911")
  expect_equal(icd9DecimalToShort("991.23"), "99123")
  
  expect_equal(icd9DecimalToShort(c("1", "991.23")), c("001","99123"))
  expect_equal(icd9DecimalToShort(c("1.", "991.23")), c("001","99123"))
  expect_equal(icd9DecimalToShort(c("1", NA, "991.23")), c("001", NA, "99123"))
  expect_equal(icd9DecimalToShort(c("1", "g", "", "991.23")), c("001", NA, NA, "99123"))
  
  expect_error(icd9DecimalToShort(1))
  expect_error(icd9DecimalToShort(22))
  expect_error(icd9DecimalToShort(333))
  expect_error(icd9DecimalToShort(1.9))
  expect_error(icd9DecimalToShort(22.8))
  expect_error(icd9DecimalToShort(333.7))
  expect_error(icd9DecimalToShort(1.98))
  expect_error(icd9DecimalToShort(22.76))
  expect_error(icd9DecimalToShort(333.54))
  
})

test_that("icd9 codes to comorbities", {
  
  testdat <- structure(
    list(visitId = c(207210584L, 207210584L, 207210584L, 
                     207210584L, 207210584L, 207210600L, 207210600L, 
                     207210600L, 207210600L, 207210600L, 207210600L, 
                     207210600L, 207210600L, 207210600L, 207210600L, 
                     207210600L, 207210600L, 207210600L, 207210618L, 207210618L), 
         icd9Code = structure(
           c(17L, 1L, 14L, 10L, 13L, 11L, 8L, 6L, 
             18L, 2L, 7L, 19L, 3L, 5L, 20L, 16L, 12L, 4L, 15L, 9L), 
           .Label = c("04104", "1912", "2449", "2949", "29680", "4254", "4371", "4530", 
                      "5070", "59370", "5990", "71595", "74689", "7757", "85226", 
                      "V153", "77182", "45341", "78097", "V1529"), class = "factor"), 
         poa = c("N", "N", "N", "Y", "Y", "Y", "Y", "Y", "Y", "Y", 
                 "Y", "Y", "Y", "Y", "E", "E", "Y", "Y", "Y", "N")),
    .Names = c("visitId", "icd9Code", "poa"), 
    row.names = 5000000:5000019, 
    class = "data.frame")
  
})

test_that("zero pad decimal", {
  
  expect_equal(icd9ZeroPadDecimal("1"), "001")
  expect_equal(icd9ZeroPadDecimal("01"), "001")
  expect_equal(icd9ZeroPadDecimal(" 01 "), "001")
  expect_equal(icd9ZeroPadDecimal("1.1"), "001.1")
  expect_equal(icd9ZeroPadDecimal("1.99"), "001.99")
  expect_equal(icd9ZeroPadDecimal("22"), "022")
  expect_equal(icd9ZeroPadDecimal(" 22.34      "), "022.34")
  expect_equal(icd9ZeroPadDecimal("V1"), "V1")
  expect_equal(icd9ZeroPadDecimal(" V1 "), "V1")
  expect_equal(icd9ZeroPadDecimal("V1.1"), "V1.1")
  expect_equal(icd9ZeroPadDecimal("V1.99"), "V1.99")
  expect_equal(icd9ZeroPadDecimal("V22"), "V22")
  expect_equal(icd9ZeroPadDecimal(" V22.34      "), "V22.34")
  expect_equal(icd9ZeroPadDecimal("333"), "333")
  expect_equal(icd9ZeroPadDecimal("333.99"), "333.99")
  expect_equal(icd9ZeroPadDecimal("333.1 "), "333.1")
  expect_equal(icd9ZeroPadDecimal(
    c("01","1.99 ", "22.34", "333", "999.00")), 
    c("001","001.99","022.34","333","999.00"))
  expect_equal(icd9ZeroPadDecimal(NA_character_), NA_character_)
  
})

test_that("expand icd9 range definition", {
  expect_equal(sort(icd9ExpandRangeShort("4012","40145")),
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
  expect_error(icd9ExpandRangeShort("002", "1"))
  expect_error(icd9ExpandRangeShort("002", "001"))
  expect_error(icd9ExpandRangeShort("2", "001"))
  expect_error(icd9ExpandRangeShort("4010", "401"))
  
  
  expect_equal( # the range 44100-4419 from the AHRQ found a gap in the code.
    sort(icd9ExpandRangeShort("4410","4412")),
    sort(c("4410", icd9ExpandRangeShort("44100","4412")))
  )
  
  expect_equal(icd9ExpandRangeShort("401","401"), icd9ExpandBaseCode("401", short=T))
  expect_equal(icd9ExpandRangeShort("401","402"), icd9ExpandBaseCode(c("401","402"), short=T))
  expect_equal(icd9ExpandRangeShort("401", "402"),
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
  
  expect_equal(icd9ExpandRangeShort("401","40102"), c("401", "4010", "40100", "40101","40102"))
  expect_equal(icd9ExpandRangeShort("V1000","V1002"), c("V1000","V1001","V1002"))
  
  expect_error(icd9ExpandRangeShort("V10", "   V1 ")) # should fail despite end being 'longer' than start
  expect_error(icd9ExpandRangeShort(c("10","20"), c("11","21"))) # only works with single range
})

test_that("preceding minors", {
  expect_equal(icd9PrecedingMinors("20"),
               c("0", "00", "01", "02", "03", "04", "05", "06", "07", "08", 
                 "09", "1", "10", "11", "12", "13", "14", "15", "16", "17", "18", 
                 "19", "20"))
  expect_equal(sort(icd9PrecedingMinors("19")),
               c("0", "00", "01", "02", "03", "04", "05", "06", "07", "08", 
                 "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", 
                 "19"))
  
  expect_equal(sort(icd9SubsequentMinors("08")),  
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
  expect_equal(("042 " %icd9% "043 ")[1], "042")
  "3420 " %icd9% "3449 "
  
})

test_that("icd9 parts to short form", {
  expect_equal(icd9ZeroPadMajor(1L),"001")
  expect_equal(icd9ZeroPadMajor(10L),"010")
  expect_equal(icd9ZeroPadMajor(999L),"999")
  expect_error(icd9ZeroPadMajor(10.1))
  #expect_equal(icd9ZeroPadMajor("V1"), "V01") # TODO
  expect_equal(icd9ZeroPadMajor(" V10"), "V10")
  expect_equal(icd9ZeroPadMajor(c("10","V25")), c("010","V25"))
  expect_equal(icd9PartsToShort(10L,"20"), "01020")
  expect_equal(icd9PartsToShort("V10",c("0","1")), c("V100", "V101"))
})

test_that("all generated icd9 lookup tables are valid!", {})

test_that("icd9ExtractAlphaNumeric", {
  
  expect_equal(icd9ExtractAlphaNumeric("V12"), matrix(data=c("V","12"), ncol=2))
  expect_equal(icd9ExtractAlphaNumeric(c("V12",34)), t(matrix(data=c("V","12","","34"), ncol=2)))
  
})

