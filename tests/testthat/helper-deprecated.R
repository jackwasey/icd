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

ahrq_end_codes <- unclass(ahrq_end_codes)
elix_end_codes <- unclass(elix_end_codes)
quan_elix_end_codes <- unclass(quan_elix_end_codes)
quan_deyo_end_codes <- unclass(quan_deyo_end_codes)

randomShortIcd9 <- as.character(floor(stats::runif(min = 10000, max = 99999, n = n)))
randomSampleAhrq <- sample(unname(c(icd::icd9_map_ahrq, recursive = TRUE)),
                           replace = TRUE, size = n)
fewIcd9 <- c("27801", "7208", "25001", "34400", "4011", "4011")

testTwenty <- test_twenty
names(testTwenty)[1] <- "visitId"

quanElixTestDat <- quan_elix_test_dat
names(quanElixTestDat)[1] <- "visitId"

quanDeyoTestDat <- quan_deyo_test_dat
names(quanDeyoTestDat)[1] <- "visitId"

ahrqTestDat <- data.frame(
  visitId = rep("visit1", times = length(ahrq_end_codes)),
  icd9 = ahrq_end_codes,
  stringsAsFactors = FALSE
)
# to deprecate
elixTestDat <- data.frame(
  visitId = rep("visit1", times = length(elix_end_codes)),
  icd9 = elix_end_codes,
  stringsAsFactors = FALSE
)

quanElixTestDat <- quan_elix_test_dat
names(quanElixTestDat)[1] <- "visitId"

quanDeyoTestDat <- quan_deyo_test_dat
names(quanDeyoTestDat)[1] <- "visitId"

twoPts <- data.frame(visitId = c("v01", "v01", "v02", "v02"),
                     icd9 = c("040", "000", "100", "000"),
                     stringsAsFactors = FALSE)
twoMap <- two_map

patientData <- simple_pts
names(patientData)[1] <- "visitId"

