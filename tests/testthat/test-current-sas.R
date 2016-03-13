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

context("SAS format file data interpretation")

test_that("basic SAS VALUE declarations can be read into an R list", {

  # general format from my n=1 sample seems to be:
  # VALUE somename numbers, "ranges"-"ranges", ranges-ranges = "mappinname"


  result <- sas_format_extract('VALUE JACK 12345 = "TESTTHAT"')
  expect_equal(result, list(JACK = list(TESTTHAT = "12345")))

  result <- sas_format_extract('VALUE JACK 12345, 22222 = "TESTTHAT"')
  expect_equal(result, list(JACK = list(TESTTHAT = c("12345", "22222"))))

  result <- sas_format_extract('VALUE TESTTHAT 100-102 = "YES"')
  expect_equal(result, list(TESTTHAT = list("YES" = "100-102")))

  expect_equal(sas_format_extract('VALUE $RFRMSOMETHING 100-102 = "HTN" 103,104 = "CHF"'),
               list("$RFRMSOMETHING" = list(HTN = "100-102", CHF = c("103", "104")))
  )


  result <- sas_format_extract(c(
    'VALUE $RFRMSOMETHING 100-102 = "HTN" 103,104 = "CHF";\n',
    ' VALUE otherstuff 105 = "YES"'))
  expect_equal(result, list("$RFRMSOMETHING" = list(HTN = "100-102",
                                                    CHF = c("103", "104")),
                            otherstuff = list("YES" = "105")))

  result <- sas_format_extract(c(
    '   VALUE $RFRMSOMETHING 100-102 = "HTN" 103,104 = "CHF";',
    'VALUE  otherstuff   105   =  "  Y E S " '))
  expect_equal(result, list("$RFRMSOMETHING" = list(HTN = "100-102",
                                                    CHF = c("103", "104")),
                            otherstuff = list("YES" = "105")))
})

test_that("groups of SAS assignments can be extracted", {

  # this is a single assignment, but of the form of sub-part of
  # multi-assignment, so we should handle it.
  expect_equal(sas_parse_assignments('"123" = "YES"'), list(YES = "123"))
  expect_equal(sas_parse_assignments('"123-456" = "YES"'), list(YES = "123-456"))
  expect_equal(sas_parse_assignments('123-456 = "YES"'), list(YES = "123-456"))

  expect_equal(
    sas_parse_assignments(
      paste('"41511"-"41519", "4160 "-"4169 ","4179 " = "PULMCIRC"',
            '"5571 ","5579 ", "V434 "= "PERIVASC"')),
    list(PULMCIRC = c("41511-41519", "4160-4169", "4179"),
         PERIVASC = c("5571", "5579", "V434")
    )
  )

})

test_that("read LET string declarations from SAS code", {

  res_list <- sas_extract_let_strings("\t%LET DC16=%STR('196','197','198','199');      ")
  expect_true(is.list(res_list))
  expect_true(names(res_list) == "DC16")
  expect_equal(res_list[[1]], c("196", "197", "198", "199"))

  res_list <- sas_extract_let_strings(c(
    "\t%LET DC16=%STR('196','197','198','199');      ",
    "\t%LET DC17=%STR('042','043','044');"))

  expect_equal(res_list[["DC16"]], c("196", "197", "198", "199"))
  expect_equal(res_list[["DC17"]], c("042", "043", "044"))

  expect_equal(
    sas_extract_let_strings("\t%LET LBL16=%STR(Metastatic Carcinoma);")[["LBL16"]],
    "Metastatic Carcinoma")

  expect_equal(
    sas_extract_let_strings('\t%LET ABC=%STR("123");")[["ABC"]]'),
    list(ABC = "123"))

})
