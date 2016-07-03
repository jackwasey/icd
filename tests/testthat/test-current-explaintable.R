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


test_that("icd_explain_table, condense = F returns same length vector: ", {

  testcodes <- c("362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414")

  expect_equal(dim (icd_explain_table(testcodes, condense = F))[1], length(testcodes))

})

test_that("icd_explain_table reproduces icd_explain.list (element-by-element) with mixed major and minor codes", {

  # The major code in this case is 414 (last element) below
  testcodes <- c("362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414")

  method1 <- unlist(icd:::icd_explain.list(testcodes)) # icd_explain element by element.
  method2 <- icd_explain_table(testcodes, condense = F) %>%
    mutate(testout = ifelse(ismajor, major_desc, long_desc)) %$%  # this replicates how icd_explain picks which description to use.
    testout # extract the testout column for testing

  expect_equal(method1,  method2)

})

test_that("icd_explain_table can reproduce icd_explain's output that uses major first ordering", {

  # This highlights the difference ordering of output between icd_explain and icd_explain_table
  # icd_explain sorts by major codings first, then by order of icd::icd9cm_hierarchy (almost alphabetic)
  # icd::icd9cm_hierarchy data used in icd_explain is almost ordered alphabetically except up to VXX and EXX on row 14742  .
  # To be sure, the next test checks that.

  # Note the test major codeis 414, the last element below
  testcodes <- c("362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414", "584")

  method1 <- icd_explain(testcodes, condense = F)

  # Find row indices in icd9cm_hierarchy that the input testcodes correspond to
  add_source_index_col <- function(df) {
    df$sourceindex <- match( df$shortcode, (icd::icd9cm_hierarchy$code %>% as.vector))
    df
  }

  method2 <- icd_explain_table(testcodes, condense = F) %>%
    mutate(testout = ifelse(ismajor, major_desc, long_desc)) %>%
    add_source_index_col %>%
    arrange(desc(ismajor), sourceindex) %$%  # arrange by the order icd9cm_hierarchy (alphabetic up to VXX and EXX codes)
    testout

  expect_equal(method1,  method2)

})

test_that("icd_explain_table can handle invalid icd9 by filling with NAs.", {

  # Note the major code in this case is 414 (last element) below
  testcodes <- c("362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414", "bogus code")

  method1 <- icd_explain_table(testcodes, condense = T)

  expect_equal(sum(is.na(method1$major_desc)), 1)

  print("Note icd_explain does not warn if about invalid code and return a output vector shorter than input length")

})

test_that("icd_explain_table can handle mixed ICD9 and ICD10", {

  test_icd10 <- "N18.3"
  testcodes <- c("362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414")

  method1 <- icd_explain_table(c(test_icd10, testcodes), condense = F)

})

test_that("icd_explain_table, condense = T, sum of condensed == length of input: ", {

  testcodes <- c("362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414")
  expect_equal( icd_explain_table(testcodes, condense = T) %$% numcondensed %>% sum, length(testcodes))

  testcodes <- c("362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414", "bogus code")
  expect_equal( icd_explain_table(testcodes, condense = T) %$% numcondensed %>% sum, length(testcodes))

})
