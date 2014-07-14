charlsonComorbidNames <- list(
  "01" = "Myocardial Infarction",
  "02" = "Congestive Heart Failure",
  "03" = "Periphral Vascular Disease",
  "04" = "Cerebrovascular Disease",
  "05" = "Dementia",
  "06" = "Chronic Pulmonary Disease",
  "07" = "Connective Tissue Disease-Rheumatic Disease",
  "08" = "Peptic Ulcer Disease",
  "09" = "Mild Liver Disease",
  "10" = "Diabetes without complications",
  "11" = "Diabetes with complications",
  "12" = "Paraplegia and Hemiplegia",
  "13" = "Renal Disease",
  "14" = "Cancer",
  "15" = "Moderate or Severe Liver Disease",
  "16" = "Metastatic Carcinoma",
  "17" = "HIV/AIDS"
)

charlsonComorbidNamesAbbrev <- list(
  "01" = "MI",
  "02" = "CHF",
  "03" = "PVD",
  "04" = "Stroke",
  "05" = "Dementia",
  "06" = "Pulmonary",
  "07" = "Rheumatic",
  "08" = "PUD",
  "09" = "LiverMild",
  "10" = "DM",
  "11" = "DMcx",
  "12" = "Paralysis",
  "13" = "Renal",
  "14" = "Cancer",
  "15" = "LiverSevere",
  "16" = "Mets",
  "17" = "HIV"
)

# 31 in this list, with two for HTN
elixhauserComorbidNamesHtn <- list( # HTN marked together as combined, but DM not.
  "01" = "Congestive heart failure",
  "02" = "Cardiac arrhythmias",
  "03" = "Valvular disease",
  "04" = "Pulmonary circulation disorders",
  "05" = "Peripheral vascular disorders",
  "06a" = "Hypertension, uncomplicated",
  "06b" = "Hypertension, complicated",
  "07" = "Paralysis",
  "08" = "Other neurological disorders",
  "09" = "Chronic pulmonary disease",
  "10" = "Diabetes, uncomplicated",
  "11" = "Diabetes, complicated",
  "12" = "Hypothyroidism",
  "13" = "Renal failure",
  "14" = "Liver disease",
  "15" = "Peptic ulcer disease excluding bleeding",
  "16" = "HIV/AIDS",
  "17" = "Lymphoma",
  "18" = "Metastatic cancer",
  "19" = "Solid tumor without metastasis",
  "20" = "Rheumatoid arthritis/collagen vascular diseases",
  "21" = "Coagulopathy",
  "22" = "Obesity",
  "23" = "Weight loss",
  "24" = "Fluid and electrolye disorders",
  "25" = "Blood loss anemia",
  "26" = "Deficiency anemias",
  "27" = "Alcohol abuse",
  "28" = "Drug abuse",
  "29" = "Psychoses",
  "30" = "Depression"
)

elixhauserComorbidNamesHtnAbbrev <- list( # HTN marked together as combined, but DM not.
  "01" = "CHF",
  "02" = "Arrhythmia",
  "03" = "Valvular",
  "04" = "PHTN",
  "05" = "PVD",
  "06a" = "HTN",
  "06b" = "HTNcx",
  "07" = "Paralysis",
  "08" = "NeuroOther",
  "09" = "Pulmonary",
  "10" = "DM",
  "11" = "DMcx",
  "12" = "Hypothyroid",
  "13" = "Renal",
  "14" = "Liver",
  "15" = "PUD",
  "16" = "HIV",
  "17" = "Lymphoma",
  "18" = "Mets",
  "19" = "Tumor",
  "20" = "Rheumatic",
  "21" = "Coagulopathy",
  "22" = "Obesity",
  "23" = "WeightLoss",
  "24" = "FluidsLytes",
  "25" = "BloodLoss",
  "26" = "Anemia",
  "27" = "Alcohol",
  "28" = "Drugs",
  "29" = "Psychoses",
  "30" = "Depression"
)

# Elixhauser with HTN combined:
elixhauserComorbidNames <- elixhauserComorbidNamesHtn
elixhauserComorbidNames["06b"] <- NULL
elixhauserComorbidNames["06a"] <- "Hypertension, combined"
names(elixhauserComorbidNames)[6] <- "06"
# Abbreviated
elixhauserComorbidNamesAbbrev <- elixhauserComorbidNamesHtnAbbrev
elixhauserComorbidNamesAbbrev["06b"] <- NULL
names(elixhauserComorbidNamesAbbrev)[6] <- "06"

# Quan Elixhauser identical ot original
quanElixhauserComorbidNames <- elixhauserComorbidNames
quanElixhauserComorbidNamesHtn <- elixhauserComorbidNamesHtn
# Abbreviated
quanElixhauserComorbidNamesAbbrev <- elixhauserComorbidNamesAbbrev
quanElixhauserComorbidNamesHtnAbbrev <- elixhauserComorbidNamesHtnAbbrev

# AHRQ very similar to Elixhauser (with HTN combined, arrhythmia dropped, giving 29 total)
ahrqComorbidNames <- elixhauserComorbidNames
ahrqComorbidNames["02"] <- NULL
names(ahrqComorbidNames) <- names(elixhauserComorbidNames)[-30]
# Abbreviated
ahrqComorbidNamesAbbrev <- elixhauserComorbidNamesAbbrev
ahrqComorbidNamesAbbrev["02"] <- NULL
names(ahrqComorbidNamesAbbrev) <- names(elixhauserComorbidNamesAbbrev)[-30]
# with HTN separated out:
ahrqComorbidNamesHtn <- elixhauserComorbidNamesHtn
ahrqComorbidNamesHtn["02"] <- NULL
names(ahrqComorbidNamesHtn) <- names(elixhauserComorbidNamesHtn)[-30]
# Abbreviated
ahrqComorbidNamesHtnAbbrev <- elixhauserComorbidNamesHtnAbbrev
ahrqComorbidNamesHtnAbbrev["02"] <- NULL
names(ahrqComorbidNamesHtnAbbrev) <- names(elixhauserComorbidNamesHtnAbbrev)[-30]

# comparable between Elixhauser and Charlson:
#TODO
