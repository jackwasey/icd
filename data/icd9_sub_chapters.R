icd9_sub_chapters <- list(
  `Intestinal Infectious Diseases` = c(start = "001", end = "009"),
  Tuberculosis = c(start = "010", end = "018"),
  `Zoonotic Bacterial Diseases` = c(
    start = "020",
    end = "027"
  ),
  `Other Bacterial Diseases` = c(start = "030", end = "041"), `Human Immunodeficiency Virus (Hiv) Infection` = c(
    start = "042",
    end = "042"
  ),
  `Poliomyelitis And Other Non-Arthropod-Borne Viral Diseases And Prion Diseases Of Central Nervous System` = c(
    start = "045",
    end = "049"
  ), `Viral Diseases Generally Accompanied By Exanthem` = c(
    start = "050",
    end = "059"
  ), `Arthropod-Borne Viral Diseases` = c(
    start = "060",
    end = "066"
  ), `Other Diseases Due To Viruses And Chlamydiae` = c(
    start = "070",
    end = "079"
  ), `Rickettsioses And Other Arthropod-Borne Diseases` = c(
    start = "080",
    end = "088"
  ), `Syphilis And Other Venereal Diseases` = c(
    start = "090",
    end = "099"
  ), `Other Spirochetal Diseases` = c(
    start = "100",
    end = "104"
  ), Mycoses = c(start = "110", end = "118"), Helminthiases = c(
    start = "120",
    end = "129"
  ), `Other Infectious And Parasitic Diseases` = c(
    start = "130",
    end = "136"
  ), `Late Effects Of Infectious And Parasitic Diseases` = c(
    start = "137",
    end = "139"
  ), `Malignant Neoplasm Of Lip, Oral Cavity, And Pharynx` = c(
    start = "140",
    end = "149"
  ), `Malignant Neoplasm Of Digestive Organs And Peritoneum` = c(
    start = "150",
    end = "159"
  ), `Malignant Neoplasm Of Respiratory And Intrathoracic Organs` = c(
    start = "160",
    end = "165"
  ), `Malignant Neoplasm Of Bone, Connective Tissue, Skin, And Breast` = c(
    start = "170",
    end = "176"
  ), `Malignant Neoplasm Of Genitourinary Organs` = c(
    start = "179",
    end = "189"
  ), `Malignant Neoplasm Of Other And Unspecified Sites` = c(
    start = "190",
    end = "199"
  ), `Malignant Neoplasm Of Lymphatic And Hematopoietic Tissue` = c(
    start = "200",
    end = "208"
  ), `Neuroendocrine Tumors` = c(start = "209", end = "209"), `Benign Neoplasms` = c(start = "210", end = "229"), `Carcinoma In Situ` = c(
    start = "230",
    end = "234"
  ), `Neoplasms Of Uncertain Behavior` = c(
    start = "235",
    end = "238"
  ), `Neoplasms Of Unspecified Nature` = c(
    start = "239",
    end = "239"
  ), `Disorders Of Thyroid Gland` = c(
    start = "240",
    end = "246"
  ), `Diseases Of Other Endocrine Glands` = c(
    start = "249",
    end = "259"
  ), `Nutritional Deficiencies` = c(start = "260", end = "269"), `Other Metabolic And Immunity Disorders` = c(
    start = "270",
    end = "279"
  ),
  # Manually inserted:
  `Diseases Of The Blood And Blood-Forming Organs` = c(start = "280", end = "289"),
  # supra-sub-chapter Psychoses = c(start = "290", end = "299"),
  `Organic Psychotic Conditions` = c(
    start = "290",
    end = "294"
  ), `Other Psychoses` = c(start = "295", end = "299"), `Neurotic Disorders, Personality Disorders, And Other Nonpsychotic Mental Disorders` = c(
    start = "300",
    end = "316"
  ), `Intellectual Disabilities` = c(
    start = "317",
    end = "319"
  ), `Inflammatory Diseases Of The Central Nervous System` = c(
    start = "320",
    end = "326"
  ), `Organic Sleep Disorders` = c(start = "327", end = "327"), `Hereditary And Degenerative Diseases Of The Central Nervous System` = c(
    start = "330",
    end = "337"
  ), Pain = c(start = "338", end = "338"), `Other Headache Syndromes` = c(
    start = "339",
    end = "339"
  ), `Other Disorders Of The Central Nervous System` = c(
    start = "340",
    end = "349"
  ), `Disorders Of The Peripheral Nervous System` = c(
    start = "350",
    end = "359"
  ), `Disorders Of The Eye And Adnexa` = c(
    start = "360",
    end = "379"
  ), `Diseases Of The Ear And Mastoid Process` = c(
    start = "380",
    end = "389"
  ), `Acute Rheumatic Fever` = c(start = "390", end = "392"), `Chronic Rheumatic Heart Disease` = c(start = "393", end = "398"), `Hypertensive Disease` = c(start = "401", end = "405"), `Ischemic Heart Disease` = c(
    start = "410",
    end = "414"
  ), `Diseases Of Pulmonary Circulation` = c(
    start = "415",
    end = "417"
  ), `Other Forms Of Heart Disease` = c(
    start = "420",
    end = "429"
  ), `Cerebrovascular Disease` = c(start = "430", end = "438"), `Diseases Of Arteries, Arterioles, And Capillaries` = c(
    start = "440",
    end = "449"
  ), `Diseases Of Veins And Lymphatics, And Other Diseases Of Circulatory System` = c(
    start = "451",
    end = "459"
  ), `Acute Respiratory Infections` = c(
    start = "460",
    end = "466"
  ), `Other Diseases Of The Upper Respiratory Tract` = c(
    start = "470",
    end = "478"
  ), `Pneumonia And Influenza` = c(start = "480", end = "488"), `Chronic Obstructive Pulmonary Disease And Allied Conditions` = c(
    start = "490",
    end = "496"
  ), `Pneumoconioses And Other Lung Diseases Due To External Agents` = c(
    start = "500",
    end = "508"
  ), `Other Diseases Of Respiratory System` = c(
    start = "510",
    end = "519"
  ), `Diseases Of Oral Cavity, Salivary Glands, And Jaws` = c(
    start = "520",
    end = "529"
  ), `Diseases Of Esophagus, Stomach, And Duodenum` = c(
    start = "530",
    end = "539"
  ), Appendicitis = c(start = "540", end = "543"), `Hernia Of Abdominal Cavity` = c(
    start = "550",
    end = "553"
  ), `Noninfectious Enteritis And Colitis` = c(
    start = "555",
    end = "558"
  ), `Other Diseases Of Intestines And Peritoneum` = c(
    start = "560",
    end = "569"
  ), `Other Diseases Of Digestive System` = c(
    start = "570",
    end = "579"
  ), `Nephritis, Nephrotic Syndrome, And Nephrosis` = c(
    start = "580",
    end = "589"
  ), `Other Diseases Of Urinary System` = c(
    start = "590",
    end = "599"
  ), `Diseases Of Male Genital Organs` = c(
    start = "600",
    end = "608"
  ), `Disorders Of Breast` = c(start = "610", end = "612"), `Inflammatory Disease Of Female Pelvic Organs` = c(
    start = "614",
    end = "616"
  ), `Other Disorders Of Female Genital Tract` = c(
    start = "617",
    end = "629"
  ), `Ectopic And Molar Pregnancy` = c(
    start = "630",
    end = "633"
  ), `Other Pregnancy With Abortive Outcome` = c(
    start = "634",
    end = "639"
  ), `Complications Mainly Related To Pregnancy` = c(
    start = "640",
    end = "649"
  ), `Normal Delivery, And Other Indications For Care In Pregnancy, Labor, And Delivery` = c(
    start = "650",
    end = "659"
  ), `Complications Occurring Mainly In The Course Of Labor And Delivery` = c(
    start = "660",
    end = "669"
  ), `Complications Of The Puerperium` = c(
    start = "670",
    end = "677"
  ), `Other Maternal And Fetal Complications` = c(
    start = "678",
    end = "679"
  ), `Infections Of Skin And Subcutaneous Tissue` = c(
    start = "680",
    end = "686"
  ), `Other Inflammatory Conditions Of Skin And Subcutaneous Tissue` = c(
    start = "690",
    end = "698"
  ), `Other Diseases Of Skin And Subcutaneous Tissue` = c(
    start = "700",
    end = "709"
  ), `Arthropathies And Related Disorders` = c(
    start = "710",
    end = "719"
  ), Dorsopathies = c(start = "720", end = "724"), `Rheumatism, Excluding The Back` = c(
    start = "725",
    end = "729"
  ), `Osteopathies, Chondropathies, And Acquired Musculoskeletal Deformities` = c(
    start = "730",
    end = "739"
  ),
  # manually inserted:
  `Congenital Anomalies` = c(start = "740", end = "759"),
  `Maternal Causes Of Perinatal Morbidity And Mortality` = c(
    start = "760",
    end = "763"
  ), `Other Conditions Originating In The Perinatal Period` = c(
    start = "764",
    end = "779"
  ), Symptoms = c(start = "780", end = "789"), `Nonspecific Abnormal Findings` = c(
    start = "790",
    end = "796"
  ), `Ill-Defined And Unknown Causes Of Morbidity And Mortality` = c(
    start = "797",
    end = "799"
  ),
  # supra-sub-chapter: Fractures = c(start = "800", end = "829"),
  `Fracture Of Skull` = c(
    start = "800",
    end = "804"
  ), `Fracture Of Neck And Trunk` = c(
    start = "805",
    end = "809"
  ), `Fracture Of Upper Limb` = c(start = "810", end = "819"), `Fracture Of Lower Limb` = c(start = "820", end = "829"),
  Dislocation = c(start = "830", end = "839"), `Sprains And Strains Of Joints And Adjacent Muscles` = c(
    start = "840",
    end = "848"
  ), `Intracranial Injury, Excluding Those With Skull Fracture` = c(
    start = "850",
    end = "854"
  ), `Internal Injury Of Thorax, Abdomen, And Pelvis` = c(
    start = "860",
    end = "869"
  ),
  # supra-sub chapter `Open Wounds` = c(start = "870", end = "897"),
  `Open Wound Of Head, Neck, And Trunk` = c(
    start = "870",
    end = "879"
  ), `Open Wound Of Upper Limb` = c(
    start = "880",
    end = "887"
  ), `Open Wound Of Lower Limb` = c(
    start = "890",
    end = "897"
  ), `Injury To Blood Vessels` = c(
    start = "900",
    end = "904"
  ), `Late Effects Of Injuries, Poisonings, Toxic Effects, And Other External Causes` = c(
    start = "905",
    end = "909"
  ), `Superficial Injury` = c(start = "910", end = "919"), `Contusion With Intact Skin Surface` = c(
    start = "920",
    end = "924"
  ), `Crushing Injury` = c(start = "925", end = "929"), `Effects Of Foreign Body Entering Through Orifice` = c(
    start = "930",
    end = "939"
  ), Burns = c(start = "940", end = "949"), `Injury To Nerves And Spinal Cord` = c(
    start = "950",
    end = "957"
  ), `Certain Traumatic Complications And Unspecified Injuries` = c(
    start = "958",
    end = "959"
  ), `Poisoning By Drugs, Medicinal And Biological Substances` = c(
    start = "960",
    end = "979"
  ), `Toxic Effects Of Substances Chiefly Nonmedicinal As To Source` = c(
    start = "980",
    end = "989"
  ), `Other And Unspecified Effects Of External Causes` = c(
    start = "990",
    end = "995"
  ), `Complications Of Surgical And Medical Care, Not Elsewhere Classified` = c(
    start = "996",
    end = "999"
  ), `Persons With Potential Healthhazards Related To Communicable Diseases` = c(
    start = "V01",
    end = "V06"
  ), `Persons With Need For Isolation, Other Potential Health Hazards And Prophylactic Measures` = c(
    start = "V07",
    end = "V09"
  ), `Persons With Potential Health Hazards Related To Personal And Family History` = c(
    start = "V10",
    end = "V19"
  ), `Persons Encountering Health Services In Circumstances Related To Reproduction And Development` = c(
    start = "V20",
    end = "V29"
  ), `Liveborn Infants According To Type Of Birth` = c(
    start = "V30",
    end = "V39"
  ), `Persons With A Condition Influencing Their Health Status` = c(
    start = "V40",
    end = "V49"
  ), `Persons Encountering Health Services For Specific Procedures And Aftercare` = c(
    start = "V50",
    end = "V59"
  ), `Persons Encountering Health Services In Other Circumstances` = c(
    start = "V60",
    end = "V69"
  ), `Persons Without Reported Diagnosis Encountered During Examination And Investigation  Of Individuals And Populations` = c(
    start = "V70",
    end = "V82"
  ), Genetics = c(start = "V83", end = "V84"), `Body Mass Index` = c(
    start = "V85",
    end = "V85"
  ), `Estrogen Receptor Status` = c(
    start = "V86",
    end = "V86"
  ), `Other Specified Personal Exposures And History Presenting Hazards To Health` = c(
    start = "V87",
    end = "V87"
  ), `Acquired Absence Of Other Organs And Tissue` = c(
    start = "V88",
    end = "V88"
  ), `Other Suspected Conditions Not Found` = c(
    start = "V89",
    end = "V89"
  ), `Retained Foreign Body` = c(
    start = "V90",
    end = "V90"
  ), `Multiple Gestation Placenta Status` = c(
    start = "V91",
    end = "V91"
  ), `External Cause Status` = c(
    start = "E000",
    end = "E000"
  ), Activity = c(start = "E001", end = "E030"),
  # supra-sub-chapter `Transport Accidents` = c(start = "E800", end = "E848"),
  `Railway Accidents` = c(start = "E800", end = "E807"), `Motor Vehicle Traffic Accidents` = c(
    start = "E810",
    end = "E819"
  ), `Motor Vehicle Nontraffic Accidents` = c(
    start = "E820",
    end = "E825"
  ), `Other Road Vehicle Accidents` = c(
    start = "E826",
    end = "E829"
  ), `Water Transport Accidents` = c(
    start = "E830",
    end = "E838"
  ), `Air And Space Transport Accidents` = c(
    start = "E840",
    end = "E845"
  ), `Vehicle Accidents Not Elsewhere Classifiable` = c(
    start = "E846",
    end = "E848"
  ), `Place Of Occurrence` = c(
    start = "E849",
    end = "E849"
  ), `Accidental Poisoning By Drugs, Medicinal Substances, And Biologicals` = c(
    start = "E850",
    end = "E858"
  ), `Accidental Poisoning By Other Solid And Liquid Substances, Gases, And Vapors` = c(
    start = "E860",
    end = "E869"
  ), `Misadventures To Patients During Surgical And Medical Care` = c(
    start = "E870",
    end = "E876"
  ), `Surgical And Medical Procedures As The Cause Of Abnormal Reaction Of Patient Or Later Complication,Without Mention Of Misadventure At The Time Of Procedure` = c(
    start = "E878",
    end = "E879"
  ), `Accidental Falls` = c(start = "E880", end = "E888"), `Accidents Caused By Fire And Flames` = c(
    start = "E890",
    end = "E899"
  ), `Accidents Due To Natural And Environmental Factors` = c(
    start = "E900",
    end = "E909"
  ), `Accidents Caused By Submersion, Suffocation, And Foreign Bodies` = c(
    start = "E910",
    end = "E915"
  ), `Other Accidents` = c(start = "E916", end = "E928"), `Late Effects Of Accidental Injury` = c(
    start = "E929",
    end = "E929"
  ), `Drugs, Medicinal And Biological Substances Causing Adverse Effects In Therapeutic Use` = c(
    start = "E930",
    end = "E949"
  ), `Suicide And Self-Inflicted Injury` = c(
    start = "E950",
    end = "E959"
  ), `Homicide And Injury Purposely Inflicted By Other Persons` = c(
    start = "E960",
    end = "E969"
  ), `Legal Intervention` = c(start = "E970", end = "E978"), Terrorism = c(start = "E979", end = "E979"), `Injury Undetermined Whether Accidentally Or Purposely Inflicted` = c(
    start = "E980",
    end = "E989"
  ), `Injury Resulting From Operations Of War` = c(
    start = "E990",
    end = "E999"
  )
)
