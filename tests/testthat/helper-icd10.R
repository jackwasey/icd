# data to support ICD-10 tests

# TODO: add this to @sources: http://www.hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp

icd10_each_quan_elix_cmb <- icd10cm(c(
    "I42.6",
    "I44.39",
    "Z95.3",
    "I27.81",
    "K55.8",
    "I10", # I10.x in Quan et al, but I10 is a lead node in ICD-10-CM
    "I13.11", #htncx
    "G82.50", # paralysis
    "R56.00",
    "J45.998", # pulm
    "E13.11", # DM, Quan is less specific, without wildcard
    "E13.359", # DMcx
    "E00.0",
    "N19", # N19.x in Quan, yet N19 is leaf node in CM
    "B18.9", # liver
    "K26.9", #PUD
    "B22", # actually B22 not defined in ICD-10-CM, B20 covers all HIV, but Quan uses B20-22,24
    "C82.28",
    "C77.9",
    "C00.8",
    "M31.31",
    "D69.41",
    "E66.09",
    "E42",
    "E86.1",
    "D50.0",
    "D50.8",
    "F10.959",
    "F11.988",
    "F29",
    "F31.31"))

icd10_each_ahrq_cmb <- as.icd10cm(c(
  "I50.9", # CHF I42.6 not in AHRQ, but in Quan Elix
  "Z95.3",
  "I27.81",
  "K55.8",
  "I10", # I10.x in Quan et al, but I10 is a lead node in ICD-10-CM
  "O11.9", #htncx, use pregnancy HTN, so I don't also get CHF, renal or DM
  "G82.50", # paralysis
  "R56.00",
  "J45.998", # pulm
  "E13.11", # DM, Quan is less specific, without wildcard
  "E13.359", # DMcx
  "E00.0",
  "N19", # N19.x in Quan, yet N19 is leaf node in CM
  "B18.2", # liver http://www.icd10data.com/ICD10CM/Codes/A00-B99/B15-B19/B18-
  "K26.9", #PUD
  "B20", # HIV/AIDS actually B22 not defined in ICD-10-CM, B20 covers all HIV, but Quan uses B20-22,24
  "C82.28",
  "C77.9",
  "C00.8",
  "M08.979", #arth/rheum M31.31 in Quan but not AHRQ,
  "D69.41",
  "E66.09",
  "E42",
  "E86.1",
  "D50.0",
  "D50.8",
  "F10.959",
  "F11.988",
  "F29",
  "F43.21")) # depression F31.31 not in AHRQ, but appears in Quan Elix
icd10_all_quan_elix <- data.frame(pt_id = 1:31, icd10_code = icd10_each_quan_elix_cmb, stringsAsFactors = FALSE) %>% icd10cm
icd10_all_quan_elix_one_pt <- data.frame(pt_id = rep(1, 31), icd10_code = icd10_each_quan_elix_cmb)

icd10_all_ahrq <- data.frame(pt_id = 1:30, icd10_code = icd10_each_ahrq_cmb, stringsAsFactors = FALSE) %>% icd10cm
icd10_all_ahrq_one_pt <- data.frame(pt_id = rep(1, 30), icd10_code = icd10_each_ahrq_cmb)

icd10_tricky <- icd10cm(c("V97.33XD", "W51.XXXA", "V00.01XD", "Y93.D", "Z99.89", "Y92.146",
                  "S10.87XA", "W55.41XA", "W61.62XD", "Z63.1", "Y93.D", "V91.07XD",
                  "W55.29XA", "V95.43XS", "W61.12XA", "R46.1"))
