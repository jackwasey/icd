icd_names_charlson <- list(
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

icd_names_charlson_abbrev <- list(
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
# HTN marked together as combined, but DM not.
icd_names_elix_htn <- list(
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

# HTN marked together as combined, but DM not.
icd_names_elix_htn_abbrev <- list(
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
icd_names_elix <- icd_names_elix_htn
icd_names_elix["06b"] <- NULL
icd_names_elix["06a"] <- "Hypertension, combined"
names(icd_names_elix)[6] <- "06"
# Abbreviated
icd_names_elix_abbrev <- icd_names_elix_htn_abbrev
icd_names_elix_abbrev["06b"] <- NULL
names(icd_names_elix_abbrev)[6] <- "06"

# Quan Elixhauser identical ot original
icd_names_quan_elix <- icd_names_elix
icd_names_quan_elix_htn <- icd_names_elix_htn
# Abbreviated
icd_names_quan_elix_abbrev <- icd_names_elix_abbrev
icd_names_quan_elix_htn_abbrev <- icd_names_elix_htn_abbrev

# AHRQ very similar to Elixhauser (with HTN combined, arrhythmia dropped, giving
# 29 total)
icd_names_ahrq <- icd_names_elix
icd_names_ahrq["02"] <- NULL
names(icd_names_ahrq) <- names(icd_names_elix)[-30]
# Abbreviated
icd_names_ahrq_abbrev <- icd_names_elix_abbrev
icd_names_ahrq_abbrev["02"] <- NULL
names(icd_names_ahrq_abbrev) <- names(icd_names_elix_abbrev)[-30]
# with HTN separated out:
icd_names_ahrq_htn <- icd_names_elix_htn
icd_names_ahrq_htn["02"] <- NULL
names(icd_names_ahrq_htn) <- names(icd_names_elix_htn)[-30]
# Abbreviated
icd_names_ahrq_htn_abbrev <- icd_names_elix_htn_abbrev
icd_names_ahrq_htn_abbrev["02"] <- NULL
names(icd_names_ahrq_htn_abbrev) <-
  names(icd_names_elix_htn_abbrev)[-30]

icd_names_cc <- list(
  "1" = "HIV/AIDS",
  "2" = "Septicemia, Sepsis, Systemic Inflammatory Response Syndrome/Shock",
  "5" = "Opportunistic Infections",
  "6" = "Opportunistic Infections",
  "7" = "Metastatic Cancer and Acute Leukemia",
  "8" = "Metastatic Cancer and Acute Leukemia",
  "9" = "Lung and Other Severe Cancers",
  "10" = "Lymphoma and Other Cancers",
  "11" = "Colorectal, Bladder, and Other Cancers",
  "12" = "Breast, Prostate, and Other Cancers and Tumors",
  "15" = "Diabetes with Renal or Peripheral Circulatory Manifestation",
  "16" = "Diabetes with Neurologic or Other Specified Manifestation",
  "17" = "Diabetes with Acute Complications",
  "18" = "Diabetes with Chronic Complications",
  "19" = "Diabetes without Complication",
  "21" = "Protein-Calorie Malnutrition",
  "22" = "Morbid Obesity",
  "23" = "Other Significant Endocrine and Metabolic Disorders",
  "25" = "End-Stage Liver Disease",
  "26" = "Cirrhosis of Liver",
  "27" = "End-Stage Liver Disease",
  "28" = "Cirrhosis of Liver",
  "29" = "Chronic Hepatitis",
  "31" = "Intestinal Obstruction/Perforation",
  "32" = "Pancreatic Disease",
  "33" = "Intestinal Obstruction/Perforation",
  "34" = "Chronic Pancreatitis",
  "35" = "Inflammatory Bowel Disease",
  "37" = "Bone/Joint/Muscle Infections/Necrosis",
  "38" = "Rheumatoid Arthritis and Inflammatory Connective Tissue Disease",
  "39" = "Bone/Joint/Muscle Infections/Necrosis",
  "40" = "Rheumatoid Arthritis and Inflammatory Connective Tissue Disease",
  "44" = "Severe Hematological Disorders",
  "45" = "Disorders of Immunity",
  "46" = "Severe Hematological Disorders",
  "47" = "Disorders of Immunity",
  "48" = "Coagulation Defects and Other Specified Hematological Disorders",
  "51" = "Dementia With Complications",
  "52" = "Dementia Without Complication",
  "54" = "Drug/Alcohol Psychosis",
  "55" = "Drug/Alcohol Dependence",
  "57" = "Schizophrenia",
  "58" = "Major Depressive, Bipolar, and Paranoid Disorders",
  "67" = "Quadriplegia, Other Extensive Paralysis",
  "68" = "Paraplegia",
  "69" = "Spinal Cord Disorders/Injuries",
  "70" = "Quadriplegia",
  "71" = "Paraplegia",
  "72" = "Spinal Cord Disorders/Injuries",
  "73" = "Amyotrophic Lateral Sclerosis and Other Motor Neuron Disease",
  "74" = "Cerebral Palsy",
  "75" = "Polyneuropathy",
  "76" = "Muscular Dystrophy",
  "77" = "Multiple Sclerosis",
  "78" = "Parkinson's and Huntington's Diseases",
  "79" = "Seizure Disorders and Convulsions",
  "80" = "Coma, Brain Compression/Anoxic Damage",
  "81" = "Acute Myocardial Infarction",
  "82" = "Respirator Dependence/Tracheostomy Status",
  "83" = "Respiratory Arrest",
  "84" = "Cardio-Respiratory Failure and Shock",
  "85" = "Congestive Heart Failure",
  "86" = "Acute Myocardial Infarction",
  "87" = "Unstable Angina and Other Acute Ischemic Heart Disease",
  "88" = "Angina Pectoris",
  "92" = "Specified Heart Arrhythmias",
  "95" = "Cerebral Hemorrhage",
  "96" = "Specified Heart Arrhythmias",
  "99" = "Cerebral Hemorrhage",
  "100" = "Ischemic or Unspecified Stroke",
  "101" = "Cerebral Palsy and Other Paralytic Syndromes",
  "103" = "Hemiplegia/Hemiparesis",
  "104" = "Monoplegia, Other Paralytic Syndromes",
  "105" = "Vascular Disease",
  "106" = "Atherosclerosis of the Extremities with Ulceration or Gangrene",
  "107" = "Vascular Disease with Complications",
  "108" = "Vascular Disease",
  "110" = "Cystic Fibrosis",
  "111" = "Chronic Obstructive Pulmonary Disease",
  "112" = "Fibrosis of Lung and Other Chronic Lung Disorders",
  "114" = "Aspiration and Specified Bacterial Pneumonias",
  "115" = "Pneumococcal Pneumonia, Empyema, Lung Abscess",
  "119" = "Proliferative Diabetic Retinopathy and Vitreous Hemorrhage",
  "122" = "Proliferative Diabetic Retinopathy and Vitreous Hemorrhage",
  "124" = "Exudative Macular Degeneration",
  "130" = "Dialysis Status",
  "131" = "Renal Failure",
  "132" = "Nephritis",
  "134" = "Dialysis Status",
  "135" = "Acute Renal Failure",
  "136" = "Chronic Kidney Disease, Stage 5",
  "137" = "Chronic Kidney Disease, Severe (Stage 4)",
  "138" = "Chronic Kidney Disease, Moderate (Stage 3)",
  "139" = "Chronic Kidney Disease, Mild or Unspecified (Stages 1-2 or Unspecified)",
  "140" = "Unspecified Renal Failure",
  "141" = "Nephritis",
  "148" = "Decubitus Ulcer of Skin",
  "149" = "Chronic Ulcer of Skin, Except Decubitus",
  "150" = "Extensive Third-Degree Burns",
  "154" = "Severe Head Injury",
  "155" = "Major Head Injury",
  "157" = "Pressure Ulcer of Skin with Necrosis Through to Muscle, Tendon, or Bone",
  "158" = "Pressure Ulcer of Skin with Full Thickness Skin Loss",
  "159" = "Pressure Ulcer of Skin with Partial Thickness Skin Loss",
  "160" = "Pressure Pre-Ulcer Skin Changes or Unspecified Stage",
  "161" = "Chronic Ulcer of Skin, Except Pressure",
  "162" = "Severe Skin Burn or Condition",
  "164" = "Major Complications of Medical Care and Trauma",
  "166" = "Severe Head Injury",
  "167" = "Major Head Injury",
  "169" = "Vertebral Fractures without Spinal Cord Injury",
  "170" = "Hip Fracture/Dislocation",
  "173" = "Traumatic Amputations and Complications",
  "174" = "Major Organ Transplant Status",
  "176" = "Complications of Specified Implanted Device or Graft",
  "177" = "Amputation Status, Lower Limb/Amputation Complications",
  "186" = "Major Organ Transplant or Replacement Status",
  "188" = "Artificial Openings for Feeding or Elimination",
  "189" = "Amputation Status, Lower Limb/Amputation Complications"
)
