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

# could also be done by test-all.R
library("magrittr", quietly = TRUE, warn.conflicts = FALSE)

local({
  do_slow_tests <- getOption("icd.do_slow_tests")
  if (is.null(do_slow_tests) || !do_slow_tests)
    message("Will skip slow tests")
  else
    message("Doing slow tests")
})

set.seed(1441)
n <- 500
np <- round(n / 20) # icd9 codes per patients

random_short_icd9_codes <- as.character(floor(stats::runif(min = 10000, max = 99999, n = n)))
random_sample_ahrq_codes <- sample(unname(c(icd::icd9_map_ahrq, recursive = TRUE)),
                                   replace = TRUE, size = n)
few_icd9_codes <- c("27801", "7208", "25001", "34400", "4011", "4011")

simple_pts <- data.frame(
  visit_id = c(1000, 1000, 1000, 1001, 1001, 1002),
  icd9 = few_icd9_codes,
  poa = factor(c("Y", "N", "Y", "N", "Y", "N"))
)

simple_poa_pts <- data.frame(
  visit_id = c("v1", "v2", "v3", "v4"),
  code = c("39891", "39790", "41791", "4401"), # deprecated tests likely need icd9 here
  poa = c("y", "N", "E", NA_character_), # should tolerate mixed case
  stringsAsFactors = FALSE
)

# multiple codes for POA and not POA, bad POA input. Throw in some invalid ICD9
# codes
complex_poa_pts <- data.frame(
  visit_id = c("v1", "v1", "v1", "v2", "v2", "v3", "v3"),
  icd9 = c("39891", "39891", "39790", "41791", "41791", "41791", "4401"),
  poa = c("Y", "n", NA_character_, "E", NA_character_, "paris", ""),
  stringsAsFactors = FALSE
)

random_test_patients <- data.frame(
  visit_id = sample(seq(1, np), replace = TRUE, size = n),
  icd9 = random_short_icd9_codes,
  poa = as.factor(sample(x = c("Y", "N", "n", "n", "y", "X", "E", "", NA),
                         replace = TRUE, size = n))
)

test_twenty <- structure(
  list(visit_id = c(207210584L, 207210584L, 207210584L,
                    207210584L, 207210584L, 207210600L, 207210600L,
                    207210600L, 207210600L, 207210600L, 207210600L,
                    207210600L, 207210600L, 207210600L, 207210600L,
                    207210600L, 207210600L, 207210600L, 207210618L, 207210618L),
       icd9Code = structure(
         c(17L, 1L, 14L, 10L, 13L, 11L, 8L, 6L,
           18L, 2L, 7L, 19L, 3L, 5L, 20L, 16L, 12L, 4L, 15L, 9L),
         .Label = c("04104", "1912", "2449", "2949", "29680", "4254", "4371",
                    "4530", "5070", "59370", "5990", "71595", "74689", "7757",
                    "85226", "V153", "77182", "45341", "78097", "V1529"),
         class = "factor"),
       poa = c("N", "N", "N", "Y", "Y", "Y", "Y", "Y", "Y", "Y",
               "Y", "Y", "Y", "Y", "E", "E", "Y", "Y", "Y", "N")),
  .Names = c("visit_id", "icd9Code", "poa"),
  row.names = 5000000:5000019,
  class = "data.frame")

# first and last item from each AHRQ comorbidity:
ahrq_end_codes <- c(lapply(icd::icd9_map_ahrq, head, n = 1),
                    lapply(icd::icd9_map_ahrq, tail, n = 1)) %>%
  unname %>%
  unlist %>%
  icd9

ahrq_test_dat <- data.frame(
  visit_id = rep("visit1", times = length(ahrq_end_codes)),
  icd9 = ahrq_end_codes,
  stringsAsFactors = FALSE
) %>% as.icd_long_data

elix_end_codes <- unlist(unname(c(lapply(icd::elixComorbid, head, n = 1),
                                  lapply(icd::elixComorbid, tail, n = 1))))
elix_test_dat <- data.frame(
  visit_id = rep("visit1", times = length(elix_end_codes)),
  icd9 = elix_end_codes,
  stringsAsFactors = FALSE
)

quan_elix_end_codes <- unlist(unname(c(lapply(icd::icd9_map_quan_elix, head, n = 1),
                                       lapply(icd::icd9_map_quan_elix, tail, n = 1))))
quan_elix_test_dat <- data.frame(
  visit_id = rep("visit1", times = length(quan_elix_end_codes)),
  icd9 = quan_elix_end_codes,
  stringsAsFactors = FALSE
)

quan_deyo_end_codes <- unlist(unname(c(lapply(icd::quanDeyoComorbid, head, n = 1),
                                       lapply(icd::quanDeyoComorbid, tail, n = 1))))
quan_deyo_test_dat <- data.frame(
  visit_id = rep("visit1", times = length(quan_deyo_end_codes)),
  icd9 = quan_deyo_end_codes,
  stringsAsFactors = FALSE
)

multi_comorbid <- rbind(
  ahrq_test_dat,
  elix_test_dat,
  quan_elix_test_dat,
  quan_deyo_test_dat
)
multi_comorbid$visit_id <-
  sample(c("v1", "v2", "v3", "v4"), size = nrow(multi_comorbid), replace = TRUE)

othersalmonella <- c("0030", "0031", "00320", "00321", "00322",
                     "00323", "00324", "00329", "0038", "0039")

# one code from each ICD-9 chapter
one_of_each <- c("002.3", "140.25", "245", "285", "290.01", "389.00",
                 "390.00", "518", "525", "581", "631", "700", "720", "759.99",
                 "765", "780.95", "800", "V02.34", "E900.4")

# two items per map, two codes per item, two codes for two visits
two_pts <- data.frame(visit_id = c("v01", "v01", "v02", "v02"),
                      icd9 = c("040", "000", "100", "000"),
                      stringsAsFactors = FALSE)
two_map <- list("malady" = c("100", "2000"),
                "ailment" = c("003", "040"))

pts_invalid_mix <- icd_long_data(
  visit_id = c(1000, 1000, 1001),
  icd9 = icd9(c("27801", "invalides", "25001")),
  poa = factor(c("Y", "N", "Y")),
  stringsAsFactors = FALSE)

###
# Sample datasets for HCC tests
# 4 patients, some with ICDs that do not exist in CC crosswalk
# One of the patients with multiple visit dates, all valid ICDs
hcc_test_simple <- icd_long_data(
  visit_name = c("1", "2", "3", "4", "4"),
  icd_name = c("20084", "1742", "30410", "41514", "95893"),
  date = as.Date(c("2011-01-01", "2011-01-02", "2011-01-03",
    "2011-01-04", "2011-01-04")))

# Only one record
hcc_test_single <- icd_long_data(
  visit_name = c("1"),
  icd_name = c("20084"),
  date = as.Date(c("2011-01-01")))

# Mix of valid and invalid ICD Codes
hcc_test_invalid <- icd_long_data(
  visit_name = c("1", "2", "3", "4", "4"),
  icd_name = c("20084", "174242", "aB30410", "41514", "95893"),
  date = as.Date(c("2011-01-01", "2011-01-02", "2011-01-03",
    "2011-01-04", "2011-01-04")))