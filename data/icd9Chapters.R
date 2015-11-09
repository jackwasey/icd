# This should not change by year. It is possible to retrieve from web if needed.
icd9Chapters <- list(
  "Infectious And Parasitic Diseases" = c(start = "001", end = "139"),
  "Neoplasms" = c(start = "140", end = "239"),
  "Endocrine, Nutritional And Metabolic Diseases, And Immunity Disorders" =
    c(start = "240", end = "279"),
  "Diseases Of The Blood And Blood-Forming Organs" =
    c(start = "280", end = "289"),
  "Mental Disorders" = c(start = "290", end = "319"),
  "Diseases Of The Nervous System And Sense Organs" =
    c(start = "320", end = "389"),
  "Diseases Of The Circulatory System" = c(start = "390", end = "459"),
  "Diseases Of The Respiratory System" = c(start = "460", end = "519"),
  "Diseases Of The Digestive System" = c(start = "520", end = "579"),
  "Diseases Of The Genitourinary System" = c(start = "580", end = "629"),
  "Complications Of Pregnancy, Childbirth, And The Puerperium" =
    c(start = "630", end = "679"),
  "Diseases Of The Skin And Subcutaneous Tissue" =
    c(start = "680", end = "709"),
  "Diseases Of The Musculoskeletal System And Connective Tissue" =
    c(start = "710", end = "739"),
  "Congenital Anomalies" = c(start = "740", end = "759"),
  "Certain Conditions Originating In The Perinatal Period" =
    c(start = "760", end = "779"),
  "Symptoms, Signs, And Ill-Defined Conditions" = c(start = "780", end = "799"),
  "Injury And Poisoning" = c(start = "800", end = "999"),
  "Supplementary Classification Of Factors Influencing Health Status And Contact With Health Services" =
    c(start = "V01", end = "V99"),
  "Supplementary Classification Of External Causes Of Injury And Poisoning" =
    c(start = "E000", end = "E999")
)

icd9_chapters <- icd9Chapters

icd10_chapters <- list(

  "Certain infectious and parasitic diseases (A00-B99)
  "Neoplasms (C00-D49)
  "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism (D50-D89)
  "Endocrine, nutritional and metabolic diseases (E00-E89)
  Mental, Behavioral and Neurodevelopmental disorders (F01-F99)
  "Diseases of the nervous system (G00-G99)
  "Diseases of the eye and adnexa (H00-H59)
  "Diseases of the ear and mastoid process (H60-H95)
  "Diseases of the circulatory system (I00-I99)
  "Diseases of the respiratory system (J00-J99)
  "Diseases of the digestive system (K00-K95)
  "Diseases of the skin and subcutaneous tissue (L00-L99)
  "Diseases of the musculoskeletal system and connective tissue (M00-M99)
  "Diseases of the genitourinary system (N00-N99)
  "Pregnancy, childbirth and the puerperium (O00-O9A)
  "Certain conditions originating in the perinatal period (P00-P96)
  "Congenital malformations, deformations and chromosomal abnormalities (Q00-Q99)
  "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)
  "Injury, poisoning and certain other consequences of external causes (S00-T88)
  20 External causes of morbidity (V00-Y99)
  21 Factors influencing health status and contact with health services (Z00-Z99)

)
