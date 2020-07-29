context("RTF ICD-9")

skip_slow("RTF parsing for ICD-9-CM")
skip_on_cran()
skip_on_ci()

j <- expect_icd9_sub_chap_equal
test_that("some known sub vs chap confusion", {
  # some things shouldn't have been called sub-chapters, just chapters. Known
  # troublemakers:
  expect_icd9_only_chap(
    "Supplementary Classification Of External Causes Of Injury And Poisoning"
  )
  expect_icd9_only_chap(
    "Supplementary Classification Of Factors Influencing Health Status And Contact With Health Services" # nolint
  )
})

test_that("sub_chapter parsing went okay, tricky cases", {
  # "TUBERCULOSIS(010-018)" # nolint
  j("Tuberculosis", "010", "018")
  # or with comma: "Vehicle Accidents, Not Elsewhere Classifiable"
  j("Vehicle Accidents Not Elsewhere Classifiable", "E846", "E848")
  j("Accidental Poisoning By Drugs, Medicinal Substances, And Biologicals", "E850", "E858") # nolint
  j("Accidental Poisoning By Other Solid And Liquid Substances, Gases, And Vapors", "E860", "E869") # nolint
  j("External Cause Status", "E000", "E000")
  j("Injury Resulting From Operations Of War", "E990", "E999")
})

test_that("majors okay", {
  # pick out some troublemakers found in testing, and some edge cases.
  expect_icd9_major_equals("Other respiratory tuberculosis", "012")
  expect_icd9_major_equals("Other poxvirus infections", "059")
  expect_icd9_major_equals("Other disorders of stomach and duodenum", "537")
  expect_icd9_major_equals("Gastrointestinal mucositis (ulcerative)", "538")
  expect_icd9_major_equals("Perinatal disorders of digestive system", "777")
  expect_icd9_major_equals("Iron deficiency anemias", "280")
  expect_icd9_major_equals(
    "Other diseases of blood and blood-forming organs",
    "289"
  )
  expect_icd9_major_equals("Anencephalus and similar anomalies", "740")
  expect_icd9_major_equals("Other and unspecified congenital anomalies", "759")

  # the following is incorrectly specified under vehicle injury in
  # http://www.icd9data.com/2015/Volume1/E000-E999/E846-E849/E849/default.htm
  expect_icd9_major_equals("Place of occurrence", "E849")
})

test_that("some majors are the same as sub-chapters", {
  # majors and sub-chapters cannot overlap, and be the same thing? I think this
  # is right. When we consider 'major' codes, we expect all three-digit (four
  # for E) codes to be present, even if this is also a sub-chapter. And it would
  # be more consistent to have every code being in a chapter, sub-chapter and
  # major, than some being exceptional.
  expect_icd9_major_is_sub_chap("Body mass index", "V85")
  expect_icd9_major_is_sub_chap("Multiple gestation placenta status", "V91")
  expect_icd9_major_is_sub_chap("External cause status", "E000")
})

test_that("Some known problem codes explained, github #126, #124, #123", {
  eee("0381", "Staphylococcal septicemia")
  eee("291", "Alcohol-induced mental disorders")
  eee("361", "Retinal detachments and defects")
  eee(
    "294",
    "Persistent mental disorders due to conditions classified elsewhere"
  )
  eee("6811", "Toe") # Cellulitis and abscess of toe
  eee("7865", "Chest pain")
})

test_that("7806 is correctly explained, github #116", {
  eee(
    "7806",
    "Fever and other physiologic disturbances of temperature regulation"
  )
})

test_that("737 is correctly explained, github #111", {
  eee("737", "Curvature of spine")
  expect_equal(icd::explain_table("737")[["short_desc"]], "Curvature of spine")
  expect_equal(icd::explain_table("737")[["long_desc"]], "Curvature of spine")
})

test_that("345.1 is parsed correctly, github #109", {
  eee("345.1", "Generalized convulsive epilepsy")
})

test_that("414, 4140 and 4141 are parsed correctly, github #99", {
  eee("414", "Other forms of chronic ischemic heart disease")
  eee("4140", "Coronary atherosclerosis")
  eee("4141", "Aneurysm and dissection of heart")
})

test_that("some randomly chosen codes are correct", {
  eee(
    "674.54",
    "Peripartum cardiomyopathy, postpartum condition or complication"
  )
  eee("E992.8", "Injury due to war operations by other marine weapons")
  eee("E870.5", "Accidental cut, puncture, perforation or hemorrhage during aspiration of fluid or tissue, puncture, and catheterization") # nolint
  eee("V53.0", "Devices related to nervous system and special senses")
  eee("V53.3", "Cardiac device")
  eee("996.88", "Complications of transplanted organ, stem cell")
  eee("970.8", "Other specified central nervous system stimulants")
  eee("970.81", "Poisoning by cocaine")
  eee("786", "Symptoms involving respiratory system and other chest symptoms")
  eee("621.4", "Hematometra")
  eee("425.18", "Other hypertrophic cardiomyopathy")
  eee("307.2", "Tics")
  eee("307.23", "Tourette's disorder")
  eee("151", "Malignant neoplasm of stomach")
  eee("1518", "Malignant neoplasm of other specified sites of stomach")
  eee("01896", "Miliary tuberculosis, unspecified, tubercle bacilli not found by bacteriological or histological examination, but tuberculosis confirmed by other methods [inoculation of animals]") # nolint

  # cases which were special in parsing:
  eee("V30", "Single liveborn")
  eee("V300", "Single liveborn, Born in hospital")
  eee(
    "V3001",
    "Single liveborn, born in hospital, delivered by cesarean section"
  )
  eee("V302", "Single liveborn, born outside hospital and not hospitalized")
  eee("345.01", "Generalized nonconvulsive epilepsy, with intractable epilepsy")
})

test_that("ICD-9-CM billable codes package data is recreated", {
  skip_on_cran()
  skip_slow()
  # Do encoding problems on Linux. It is unpredictable at the best of times.
  skip_flat_icd9_all_avail()
  check_billable <- .icd9cm_parse_leaf_descs()
  # make specific quick tests for previously known problems:
  b32 <- .parse_icd9cm_leaf_year("2014")
  expect_true(nrow(b32) == 14567L)
  expect_true(ncol(b32) == 3L)
  expect_identical(
    b32[b32$code == "9999", "short_desc"],
    "Complic med care NEC/NOS"
  )
  expect_identical(
    b32[b32$code == "E0000", "short_desc"],
    "Civilian activity-income"
  )
  expect_identical(
    b32[b32$code == "E9991", "short_desc"],
    "Late effect, terrorism"
  )
  expect_identical(
    b32[b32$code == "V9199", "short_desc"],
    "Mult gest-plac/sac undet"
  )
})

test_that("explain icd9GetChapters simple input", {
  skip_no_icd_data_cache()
  skip_on_no_rtf("2014")
  chaps1 <- .icd9_get_chapters(c("410", "411", "412"), short_code = TRUE)
  expect_equal(nrow(chaps1), 3)
  inf <- try(.icd9_get_chapters("418", short_code = TRUE), silent = TRUE)
  # no such code 418
  expect_error(.icd9_get_chapters("418", short_code = TRUE), info = inf)
  chaps3 <- .icd9_get_chapters("417", short_code = FALSE)
  expect_equal(as_char_no_warn(chaps3$three_digit), "417")
  expect_equal(
    as_char_no_warn(chaps3$major),
    "Other diseases of pulmonary circulation"
  )
  expect_equal(
    as_char_no_warn(chaps3$sub_chapter),
    "Diseases Of Pulmonary Circulation"
  )
  expect_equal(
    as_char_no_warn(chaps3$chapter),
    "Diseases Of The Circulatory System"
  )

  chaps4 <- .icd9_get_chapters("417", short_code = TRUE)
  chaps5 <- .icd9_get_chapters("417.1", short_code = FALSE)
  chaps6 <- .icd9_get_chapters("4171", short_code = TRUE)
  chaps7 <- .icd9_get_chapters("417.1", short_code = FALSE)
  chaps8 <- .icd9_get_chapters("4171", short_code = TRUE)
  expect_equal(chaps3, chaps4)
  expect_equal(chaps3, chaps5)
  expect_equal(chaps3, chaps6)
  expect_equal(chaps3, chaps7)
  expect_equal(chaps3, chaps8)
  chap9 <- .icd9_get_chapters(NA, short_code = FALSE)
  # (note the following fails if a factor has `NA` levels, just R3.4+?) There was a change. see `?factor`
  expect_true(all(is.na(chap9)))
})

# parsing quirks

test_that("2017 quirks", {
  skip_missing_dat("icd9cm2007")
  x <- get_icd9cm2007()
  expect_true("00863" %in% x$code)
})

test_that("2018 quirks ok", {
  skip_missing_dat("icd9cm2008")
  x <- get_icd9cm2008()
  nines <- grep(pattern = "^945[012345]9$", x$code)
  expect_match(
    x[nines, "long_desc"],
    ".*multiple sites of lower limb\\(s\\).*"
  )
})
