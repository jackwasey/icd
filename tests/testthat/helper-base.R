message("loading helper-base.R")
set.seed(1441)
n <- 500
np <- round(n / 20) # icd9 codes per patients
random_short_icd9_codes <-
  as.character(floor(stats::runif(min = 10000, max = 99999, n = n)))
random_sample_ahrq_codes <-
  sample(unname(c(icd9_map_ahrq, recursive = TRUE)), replace = TRUE, size = n)
few_icd9_codes <- c("27801", "7208", "25001", "34400", "4011", "4011")
empty_pts <- data.frame(
  visit_id = character(0),
  icd9 = character(0),
  icd10 = character(0),
  poa = character(0)
)
empty_ahrq_mat <- matrix(
  nrow = 0, ncol = length(icd9_map_ahrq),
  dimnames = list(character(0), names(icd9_map_ahrq))
)
empty_ahrq_mat_heir <- matrix(
  nrow = 0, ncol = length(icd9_map_ahrq),
  dimnames = list(
    character(0),
    names(icd9_map_ahrq)
  )
)
# according to
# https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp
# diabetes with complications is NOT foldeded into one new category, just
# hypertension.
empty_ahrq_mat_heir <-
  empty_ahrq_mat_heir[, !colnames(empty_ahrq_mat_heir) %in% c("HTNcx")]
simple_pts <- data.frame(
  visit_id = c(1000, 1000, 1000, 1001, 1001, 1002),
  icd9 = few_icd9_codes,
  poa = factor(c("Y", "N", "Y", "N", "Y", "N"))
)
two_pts_fac <- data.frame(
  # don't use visit IDs that collide with ICD codes! So unlikely in real life.
  visit_id = c("visit01", "visit01", "visit02", "visit02"),
  icd9 = c("040", "000", "100", "000"),
  stringsAsFactors = TRUE
)
two_map_fac <- as.list(data.frame(
  "malady" = c("100", "2000"),
  "ailment" = c("003", "040"),
  stringsAsFactors = TRUE
))
simple_poa_pts <- data.frame(
  visit_id = c("visit01", "visit02", "visit03", "visit04"),
  code = c("39891", "39790", "41791", "4401"),
  poa = c("y", "N", "E", NA_character_), # should tolerate mixed case
  stringsAsFactors = FALSE
)
# multiple codes for POA and not POA, bad POA input. Throw in some invalid ICD9
# codes
complex_poa_pts <- data.frame(
  visit_id = c("visit1", "visit1", "visit1",
               "visit2", "visit2",
               "visit3", "visit3"),
  icd9 = c("39891", "39891", "39790", "41791", "41791", "41791", "4401"),
  poa = c("Y", "n", NA_character_, "E", NA_character_, "paris", ""),
  stringsAsFactors = FALSE
)
random_test_patients <- data.frame(
  visit_id = sample(seq(1, np), replace = TRUE, size = n),
  icd9 = random_short_icd9_codes,
  poa = as.factor(sample(
    x = c("Y", "N", "n", "n", "y", "X", "E", "", NA),
    replace = TRUE, size = n
  ))
)
test_twenty <- structure(
  list(
    visit_id = c(
      207210584L, 207210584L, 207210584L,
      207210584L, 207210584L, 207210600L, 207210600L,
      207210600L, 207210600L, 207210600L, 207210600L,
      207210600L, 207210600L, 207210600L, 207210600L,
      207210600L, 207210600L, 207210600L, 207210618L, 207210618L
    ),
    icd9Code = structure(
      c(
        17L, 1L, 14L, 10L, 13L, 11L, 8L, 6L,
        18L, 2L, 7L, 19L, 3L, 5L, 20L, 16L, 12L, 4L, 15L, 9L
      ),
      .Label = c(
        "04104", "1912", "2449", "2949", "29680", "4254", "4371",
        "4530", "5070", "59370", "5990", "71595", "74689", "7757",
        "85226", "V153", "77182", "45341", "78097", "V1529"
      ),
      class = "factor"
    ),
    poa = c(
      "N", "N", "N", "Y", "Y", "Y", "Y", "Y", "Y", "Y",
      "Y", "Y", "Y", "Y", "E", "E", "Y", "Y", "Y", "N"
    )
  ),
  .Names = c("visit_id", "icd9Code", "poa"),
  row.names = 5000000:5000019,
  class = "data.frame"
)
# first and last item from each AHRQ comorbidity:
icd:::icd9(
  unlist(
    unname(
      c(
        lapply(icd9_map_ahrq, head, n = 1),
        lapply(icd9_map_ahrq, tail, n = 1)
      )
    )
  )
) -> ahrq_end_codes
ahrq_test_dat <- as.icd_long_data(
  data.frame(
    visit_id = rep("visit1", times = length(ahrq_end_codes)),
    icd9 = ahrq_end_codes,
    stringsAsFactors = FALSE
  )
)
elix_end_codes <- unlist(
  unname(c(
    lapply(icd9_map_elix, head, n = 1),
    lapply(icd9_map_elix, tail, n = 1)
  ))
)
elix_test_dat <- data.frame(
  visit_id = rep("visit1", times = length(elix_end_codes)),
  icd9 = elix_end_codes,
  stringsAsFactors = FALSE
)
quan_elix_end_codes <- unlist(
  unname(c(
    lapply(icd9_map_quan_elix, head, n = 1),
    lapply(icd9_map_quan_elix, tail, n = 1)
  ))
)
quan_elix_test_dat <- data.frame(
  visit_id = rep("visit1", times = length(quan_elix_end_codes)),
  icd9 = quan_elix_end_codes,
  stringsAsFactors = FALSE
)
quan_deyo_end_codes <- unlist(
  unname(c(
    lapply(icd9_map_quan_deyo, head, n = 1),
    lapply(icd9_map_quan_deyo, tail, n = 1)
  ))
)
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
  sample(c("visit1", "visit2", "visit3", "visit4"),
         size = nrow(multi_comorbid),
         replace = TRUE)
othersalmonella <- c(
  "0030", "0031", "00320", "00321", "00322",
  "00323", "00324", "00329", "0038", "0039"
)
# one code from each ICD-9 chapter
one_of_each <- c(
  "002.3", "140.25", "245", "285", "290.01", "389.00",
  "390.00", "518", "525", "581", "631", "700", "720", "759.99",
  "765", "780.95", "800", "V02.34", "E900.4"
)
one_pt_one_icd9 <- data.frame(
  visit_id = c("a"),
  icd9 = c("042"),
  stringsAsFactors = FALSE
)
one_pt_two_icd9 <- data.frame(
  visit_id = c("a", "a"),
  icd9 = c("042", "042"),
  stringsAsFactors = FALSE
)
# two items per map, two codes per item, two codes for two visits
two_pts <- data.frame(
  visit_id = c("visit01", "visit01", "visit02", "visit02"),
  icd9 = c("040", "000", "100", "000"),
  stringsAsFactors = FALSE
)
two_map <- list(
  "malady" = c("100", "2000"),
  "ailment" = c("003", "040")
)
pts_invalid_mix <- icd_long_data(
  visit_id = c(1000, 1000, 1001),
  icd9 = icd:::icd9(c("27801", "invalides", "25001")),
  poa = factor(c("Y", "N", "Y")),
  stringsAsFactors = FALSE
)
# Sample datasets for HCC tests
# 4 patients, some with ICDs that do not exist in CC crosswalk
# One of the patients with multiple visit dates, all valid ICDs
hcc_test_simple9 <- icd_long_data(
  visit_id = c("1", "2", "3", "4", "4"),
  icd_code = c("20084", "1742", "30410", "41514", "95893"),
  date = as.Date(c(
    "2011-01-01", "2011-01-02", "2011-01-03",
    "2011-01-04", "2011-01-04"
  ))
)
hcc_test_single <- icd_long_data(
  visit = c("1"),
  icd_code = c("20084"),
  date = as.Date(c("2011-01-01"))
)
# Mix of valid and invalid ICD Codes
hcc_test_invalid <- icd_long_data(
  patient = c("1", "2", "3", "4", "4"),
  icd_code = c("20084", "174242", "aB30410", "41514", "95893"),
  date = as.Date(c(
    "2011-01-01", "2011-01-02", "2011-01-03",
    "2011-01-04", "2011-01-04"
  ))
)
hcc_test_simple10 <- hcc_test_simple9
hcc_test_simple10$icd_code <- c("T410X2S", "M87071", "I82531", "I70232", "C434")
hcc_test_simple10$date <- hcc_test_simple10$date + 6 * 365
