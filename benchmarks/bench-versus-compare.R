
compare_versus_others <- function() {
  icd_res <- icd::comorbid_quan_deyo(thousand_pts, return_df = TRUE)
  comorbidity_res <- comorbidity::comorbidity(
    x = thousand_pts, id = "visit_id",
    code = "code", score = "charlson_icd9")
  comorbidity_res$visit_id <- as.character(comorbidity_res$visit_id)
  comorbidity_res <- icd:::binary_to_logical(comorbidity_res)[1:18]
  medicalrisk_res <- medicalrisk::generate_comorbidity_df(
    thousand_pts_mr, icd9mapfn = medicalrisk::icd9cm_charlson_quan)
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
  explain_code(icd:::icd_get_major.icd9(pt_39_codes, short_code = TRUE), warn = FALSE)
}
