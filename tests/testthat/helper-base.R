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

if (!exists("do_slow_tests") || !do_slow_tests) message("Will skip slow tests")
if (!exists("do_online_tests") || !do_online_tests) message("Will skip online tests")

set.seed(1441)
n <- 500
np <- round(n / 20) # icd9 codes per patients

randomShortIcd9 <- as.character(floor(stats::runif(min = 10000, max = 99999, n = n)))
randomSampleAhrq <- sample(unname(c(icd9::icd9_map_ahrq, recursive = TRUE)),
                           replace = TRUE, size = n)
fewIcd9 <- c("27801", "7208", "25001", "34400", "4011", "4011")

patientData <- data.frame(
  visitId = c(1000, 1000, 1000, 1001, 1001, 1002),
  icd9 = fewIcd9,
  poa = factor(c("Y", "N", "Y", "N", "Y", "N"))
)

simplePoaPatients <- data.frame(
  visitId = c("v1", "v2", "v3", "v4"),
  icd9 = c("39891", "39790", "41791", "4401"),
  poa = c("y", "N", "E", NA_character_), # should tolerate mixed case
  stringsAsFactors = FALSE
)

# multiple codes for POA and not POA, bad POA input. Throw in some invalid ICD9
# codes
complexPoaPatients <- data.frame(
  visitId = c("v1", "v1", "v1", "v2", "v2", "v3", "v3"),
  icd9 = c("39891", "39891", "39790", "41791", "41791", "41791", "4401"),
  poa = c("Y", "n", NA_character_, "E", NA_character_, "paris", ""),
  stringsAsFactors = FALSE
)

randomPatients <- data.frame(
  visitId = sample(seq(1, np), replace = TRUE, size=n),
  icd9 = randomShortIcd9,
  poa = as.factor(sample(x=c("Y", "N", "n", "n", "y", "X", "E", "", NA),
                         replace = TRUE, size=n))
)

# random patients with icd9 codes selected from ahrq data
randomPatientsAhrqIcd9 <- randomPatients
randomPatientsAhrqIcd9[["icd9"]] <- randomSampleAhrq

testTwenty <- structure(
  list(visitId = c(207210584L, 207210584L, 207210584L,
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
  .Names = c("visitId", "icd9Code", "poa"),
  row.names = 5000000:5000019,
  class = "data.frame")

# first and last item from each comorbidity:
icd9fl <- c(lapply(icd9::icd9_map_ahrq, head, n = 1),
            lapply(icd9::icd9_map_ahrq, tail, n = 1)) %>%
  unname %>%
  unlist %>%
  icd9

ahrqTestDat <- data.frame(
  visitId = rep("visit1", times = length(icd9fl)),
  icd9 = icd9fl,
  stringsAsFactors = FALSE
) %>% icd_long_data

icd9fl <- unlist(unname(c(lapply(icd9::elixComorbid, head, n = 1),
                          lapply(icd9::elixComorbid, tail, n = 1))))
elixTestDat <- data.frame(
  visitId = rep("visit1", times = length(icd9fl)),
  icd9 = icd9fl,
  stringsAsFactors = FALSE
)
icd9fl <- unlist(unname(c(lapply(icd9::icd9_map_quan_elix, head, n = 1),
                          lapply(icd9::icd9_map_quan_elix, tail, n = 1))))
quanElixTestDat <- data.frame(
  visitId = rep("visit1", times = length(icd9fl)),
  icd9 = icd9fl,
  stringsAsFactors = FALSE
)
icd9fl <- unlist(unname(c(lapply(icd9::quanDeyoComorbid, head, n = 1),
                          lapply(icd9::quanDeyoComorbid, tail, n = 1))))
quanDeyoTestDat <- data.frame(
  visitId = rep("visit1", times = length(icd9fl)),
  icd9 = icd9fl,
  stringsAsFactors = FALSE
)

othersalmonella <- c("0030", "0031", "00320", "00321", "00322",
                     "00323", "00324", "00329", "0038", "0039")

# one code from each ICD-9 chapter
one_of_each <- c("002.3", "140.25", "245", "285", "290.01", "389.00",
                 "390.00", "518", "525", "581", "631", "700", "720", "759.99",
                 "765", "780.95", "800", "V02.34", "E900.4")

# two items per map, two codes per item, two codes for two visits
twoPts <- data.frame(visitId = c("v01", "v01", "v02", "v02"),
                     icd9 = c("040", "000", "100", "000"),
                     stringsAsFactors = FALSE)
twoMap <- list("malady" = c("100", "2000"),
               "ailment" = c("003", "040"))

mixInvalidPts <- icd9(
  icd_long_data(
    data.frame(
      visitId = c(1000, 1000, 1001),
      icd9 = icd9(c("27801", "invalides", "25001")),
      poa = factor(c("Y", "N", "Y")),
      stringsAsFactors = FALSE
    )
  )
)
