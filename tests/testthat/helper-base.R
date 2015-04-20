set.seed(1441)
n <- 500
np <- round(n / 20) # icd9 codes per patients

randomShortIcd9 <- as.character(floor(runif(min = 10000, max = 99999, n = n)))
randomSampleAhrq <- sample(unname(c(icd9::ahrqComorbid, recursive = TRUE)),
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
icd9fl <- unlist(unname(c(lapply(icd9::ahrqComorbid, head, n = 1),
                          lapply(icd9::ahrqComorbid, tail, n = 1))))
ahrqTestDat <- data.frame(
  visitId = rep("visit1", times = length(icd9fl)),
  icd9 = icd9fl,
  stringsAsFactors = FALSE
)
icd9fl <- unlist(unname(c(lapply(icd9::elixComorbid, head, n = 1),
                          lapply(icd9::elixComorbid, tail, n = 1))))
elixTestDat <- data.frame(
  visitId = rep("visit1", times = length(icd9fl)),
  icd9 = icd9fl,
  stringsAsFactors = FALSE
)
icd9fl <- unlist(unname(c(lapply(icd9::quanElixComorbid, head, n = 1),
                          lapply(icd9::quanElixComorbid, tail, n = 1))))
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
