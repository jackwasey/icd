# Copyright (C) 2014 - 2015  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

context("test SAS interpretation")

test_that("basic SAS VALUE declarations can be read into an R list", {

  # general format from my n=1 sample seems to be:
  # VALUE somename numbers, "ranges"-"ranges", ranges-ranges = "mappinname"


  result <- sasFormatExtract('VALUE JACK 12345 = "TESTTHAT"')
  expect_equal(result, list(JACK = list(TESTTHAT = "12345")))

  result <- sasFormatExtract('VALUE JACK 12345, 22222 = "TESTTHAT"')
  expect_equal(result, list(JACK = list(TESTTHAT = c("12345", "22222"))))

  result <- sasFormatExtract('VALUE TESTTHAT 100-102 = "YES"')
  expect_equal(result, list(TESTTHAT = list("YES" = "100-102")))

  expect_equal(sasFormatExtract('VALUE $RFRMSOMETHING 100-102 = "HTN" 103,104 = "CHF"'),
               list("$RFRMSOMETHING" = list(HTN = "100-102", CHF = c("103", "104")))
  )


  result <- sasFormatExtract(c(
    'VALUE $RFRMSOMETHING 100-102 = "HTN" 103,104 = "CHF";\n',
    ' VALUE otherstuff 105 = "YES"'))
  expect_equal(result, list("$RFRMSOMETHING" = list(HTN = "100-102",
                                                    CHF = c("103", "104")),
                            otherstuff = list("YES" = "105")))

  result <- sasFormatExtract(c(
    '   VALUE $RFRMSOMETHING 100-102 = "HTN" 103,104 = "CHF";',
    'VALUE  otherstuff   105   =  "  Y E S " '))
  expect_equal(result, list("$RFRMSOMETHING" = list(HTN = "100-102",
                                                    CHF = c("103", "104")),
                            otherstuff = list("YES" = "105")))
})

test_that("groups of SAS assignments can be extracted", {

  # this is a single assignment, but of the form of sub-part of
  # multi-assignment, so we should handle it.
  expect_equal(sasParseAssignments('"123" = "YES"'), list(YES = "123"))
  expect_equal(sasParseAssignments('"123-456" = "YES"'), list(YES = "123-456"))
  expect_equal(sasParseAssignments('123-456 = "YES"'), list(YES = "123-456"))
  #   expect_equal(
  #     sasParseAssignments('"999  " = "ONE" "777  "-"8881 " = "TWO"',
  #       stripWhiteSpace=F),
  #     list(ONE="999  ", TWO='777  -8881 '))

  expect_equal(
    sasParseAssignments('"41511"-"41519", "4160 "-"4169 ","4179 " = "PULMCIRC" "5571 ","5579 ", "V434 "= "PERIVASC"'),
    list(PULMCIRC = c("41511-41519", "4160-4169", "4179"),
         PERIVASC = c("5571", "5579", "V434")
    )
  )

})

test_that("read LET string declarations from SAS code", {

  letStrOne <-  "\t%LET DC16=%STR('196','197','198','199');      "
  resList <- sasExtractLetStrings(letStrOne)
  expect_true(is.list(resList))
  expect_true(names(resList) == "DC16")
  expect_equal(resList[[1]], c("196", "197", "198", "199"))

  resList <- sasExtractLetStrings(c(
    "\t%LET DC16=%STR('196','197','198','199');      ",
    "\t%LET DC17=%STR('042','043','044');"))

  expect_equal(resList[["DC16"]], c("196", "197", "198", "199"))
  expect_equal(resList[["DC17"]], c("042", "043", "044"))

  expect_equal(
    sasExtractLetStrings("\t%LET LBL16=%STR(Metastatic Carcinoma);")[["LBL16"]],
    "Metastatic Carcinoma")

  expect_equal(
    sasExtractLetStrings('\t%LET ABC=%STR("123");")[["ABC"]]'),
    list(ABC = "123"))

})
