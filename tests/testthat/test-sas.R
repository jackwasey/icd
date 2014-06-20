
test_that("basic SAS VALUE declarations can be read into an R list", {
  
  # general format from my n=1 sample seems to be:
  # VALUE somename numbers, "ranges"-"ranges", ranges-ranges = "mappinname"
  
  
  result <- sasFormatExtract('VALUE JACK 12345 = "TESTTHAT"')
  expect_equal(result, list(JACK = list(TESTTHAT = "12345")))
  
  result <- sasFormatExtract('VALUE JACK 12345, 22222 = "TESTTHAT"')
  expect_equal(result, list(JACK = list(TESTTHAT = c("12345","22222"))))
  
  result <- sasFormatExtract('VALUE TESTTHAT 100-102 = "YES"')
  expect_equal(result, list(TESTTHAT = list("YES" = "100-102")))
  
  
  result <- sasFormatExtract('VALUE $RFRMSOMETHING 100-102 = "HTN" 103,104 = "CHF"')
  expect_equal(result, list("$RFRMSOMETHING" = 
                              list(HTN = "100-102", CHF=c("103","104")))
  )
  
  
  result <- sasFormatExtract(c('VALUE $RFRMSOMETHING 100-102 = "HTN" 103,104 = "CHF";\n',
                               ' VALUE otherstuff 105 = "YES"'))
  expect_equal(result, list("$RFRMSOMETHING" = list(HTN = "100-102", CHF=c("103","104")),
                            otherstuff = list("YES" = '105')))
  
  result <- sasFormatExtract(c('   VALUE $RFRMSOMETHING 100-102 = "HTN" 103,104 = "CHF";',
                               'VALUE  otherstuff   105   =  "  Y E S " '))
  expect_equal(result, list("$RFRMSOMETHING" = list(HTN = "100-102", CHF=c("103","104")),
                            otherstuff = list("YES" = '105')))
  
  # make sure it tolerates lack of whitespace...
  
  
  #print(getwd()) # we're in the testthat directory, at least when running this code with test_file
  #sasTxt <- readLines('inst/extdata//comformat2012-2013.txt')
  
})

#test_that("proc format opening is understood", {})

test_that("groups of SAS assignments can be extracted", {
  
  # this is a single assignment, but of the form of sub-part of multi-assignment, so we should handle it.
  expect_equal(sasParseAssignments('"123" = "YES"'), list(YES="123"))
  expect_equal(sasParseAssignments('"123-456" = "YES"'), list(YES="123-456"))
  expect_equal(sasParseAssignments('123-456 = "YES"'), list(YES="123-456"))
  #   expect_equal( 
  #     sasParseAssignments('"999  " = "ONE" "777  "-"8881 " = "TWO"', stripWhiteSpace=F),
  #     list(ONE="999  ", TWO='777  -8881 '))
  
  expect_equal(sasParseAssignments(
    '"41511"-"41519", "4160 "-"4169 ","4179 " = "PULMCIRC" "5571 ","5579 ", "V434 "= "PERIVASC"'),
    list(
      PULMCIRC=c('41511-41519','4160-4169','4179'),
      PERIVASC=c('5571','5579','V434')
    )
  )
  
})

test_that("AHRQ interpretation at least returns something reasonable", {
  result <- parseAhrqSas(save=F)
  expect_that(result, is_a("list"))
  expect_true(length(result) > 10)
 })

