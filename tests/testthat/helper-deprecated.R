randomShortIcd9 <- as.character(floor(stats::runif(min = 10000, max = 99999, n = n)))
randomSampleAhrq <- sample(unname(c(icd::icd9_map_ahrq, recursive = TRUE)),
                           replace = TRUE, size = n)
fewIcd9 <- c("27801", "7208", "25001", "34400", "4011", "4011")

testTwenty <- test_twenty
names(testTwenty)[1] <- "visitId"


elixTestDat <- data.frame(
  visitId = rep("visit1", times = length(icd9fl)),
  icd9 = icd9fl,
  stringsAsFactors = FALSE
)

quanElixTestDat <- quan_elix_test_dat
names(quanElixTestDat)[1] <- "visitId"

quanDeyoTestDat <- quan_deyo_test_dat
names(quanDeyoTestDat)[1] <- "visitId"

ahrqTestDat <- data.frame(
  visitId = rep("visit1", times = length(icd9fl)),
  icd9 = icd9fl,
  stringsAsFactors = FALSE
)
# to deprecate
elixTestDat <- data.frame(
  visitId = rep("visit1", times = length(icd9fl)),
  icd9 = icd9fl,
  stringsAsFactors = FALSE
)

# to deprecate
quanElixTestDat <- quan_elix_test_dat
names(quanElixTestDat)[1] <- "visitId"

# to deprecate
quanDeyoTestDat <- quan_deyo_test_dat
names(quanDeyoTestDat)[1] <- "visitId"

# to deprecate
twoPts <- data.frame(visitId = c("v01", "v01", "v02", "v02"),
                     icd9 = c("040", "000", "100", "000"),
                     stringsAsFactors = FALSE)
# to deprecate
twoMap <- two_map


# to deprecate
patientData <- simple_pts
names(patientData)[1] <- "visitId"

