library(icd)
library(comorbidity)
# library(comorbidities) # unavailable on CRAN
library(medicalrisk)

# random seed is set in function
ten_million_random_pts <- get_ten_million_icd9_pts()
ten_thousand_pts <- ten_million_random_pts[1:1e4, ]
thousand_pts <- icd:::generate_random_pts(1e3)

# fixes for medicalrisk
thousand_pts_mr <- thousand_pts
names(thousand_pts_mr) <- c("id", "icd9cm")
thousand_pts_mr$icd9cm <- paste("D", thousand_pts_mr$icd9cm, sep = "")
ten_thousand_pts_mr <- thousand_pts
names(ten_thousand_pts_mr) <- c("id", "icd9cm")
ten_thousand_pts_mr$icd9cm <- paste("D", ten_thousand_pts_mr$icd9cm, sep = "")

# icd about 100 times faster even with -O0 and debug enabled!
microbenchmark::microbenchmark(
  icd::comorbid_quan_deyo(thousand_pts, return_df = TRUE),
  comorbidity::comorbidity(x = thousand_pts, id = "visit_id", code = "code", score = "charlson_icd9"),
  comorbidity::comorbidity(x = thousand_pts, id = "visit_id", code = "code", score = "charlson_icd9", parallel = TRUE),
  medicalrisk::generate_comorbidity_df(thousand_pts_mr, icd9mapfn = medicalrisk::icd9cm_charlson_quan),
  times = 5
)

icd_res <- icd::comorbid_quan_deyo(thousand_pts, return_df = TRUE)
comorbidity_res <- comorbidity::comorbidity(x = thousand_pts, id = "visit_id", code = "code", score = "charlson_icd9")
comorbidity_res$visit_id <- as.character(comorbidity_res$visit_id)
comorbidity_res <- icd:::binary_to_logical(comorbidity_res)[1:18]
medicalrisk_res <- medicalrisk::generate_comorbidity_df(thousand_pts_mr, icd9mapfn = medicalrisk::icd9cm_charlson_quan)
icd_res <- unname(icd_res)
comorbidity_res <- unname(comorbidity_res)
medicalrisk_res <- unname(medicalrisk_res)
identical(icd_res, comorbidity_res)
identical(icd_res, medicalrisk_res)
identical(comorbidity_res, medicalrisk_res)
# oh dear - all three are different
colSums(icd_res[-1])
colSums(medicalrisk_res[-1])
colSums(comorbidity_res[-1])

# differences are in chronic pulmonary disease (6) and cancer (14)
icd_res$visit_id[which(icd_res$Pulmonary != comorbidity_res$copd)] # two differences
icd_res$visit_id[which(icd_res$Cancer != comorbidity_res$canc)] # seven different patients

icd_res$visit_id[which(icd_res$Pulmonary != medicalrisk_res$chrnlung)] # two differences
icd_res$visit_id[which(icd_res$Cancer != medicalrisk_res$tumor)] # just one difference

icd_res$visit_id[which(comorbidity_res$copd != medicalrisk_res$chrnlung)] # agreement
icd_res$visit_id[which(comorbidity_res$canc != medicalrisk_res$tumor)] # six differ

# patients "5", "35", "39" appear most
bad_pts <- thousand_pts[thousand_pts$visit_id %in% c("5", "35", "39"), c("visit_id", "code")]

pt_39_codes <- bad_pts[bad_pts$visit_id == "39", "code"]
# look for where chronic lung disease might be hiding
explain(icd:::icd_get_major.icd9(pt_39_codes, short_code = TRUE), warn = FALSE)

# icd about 100 times faster even with -O0 and debug enabled!
microbenchmark::microbenchmark(
  icd::comorbid_quan_deyo(ten_thousand_pts),
  comorbidity::comorbidity(x = ten_thousand_pts, id = "visit_id", code = "code", score = "charlson_icd9"),
  comorbidity::comorbidity(x = ten_thousand_pts, id = "visit_id", code = "code", score = "charlson_icd9", parallel = TRUE),
  medicalrisk::generate_comorbidity_df(ten_thousand_pts_mr, icd9mapfn = medicalrisk::icd9cm_charlson_quan),
  times = 5
)
