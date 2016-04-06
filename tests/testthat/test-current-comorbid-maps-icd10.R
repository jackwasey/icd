# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

context("content of icd-10 to comorbidity maps")

test_that("the icd-10 quan elix comorbidity map data is exactly as produced by the generator", {
  expect_equivalent(icd10_map_quan_elix, icd10_generate_map_quan_elix(save_data = FALSE))
})

test_that("the icd-10 quan deyo comorbidity map data is exactly as produced by the generator", {
  expect_equivalent(icd10_map_quan_deyo, icd10_generate_map_quan_deyo(save_data = FALSE))
})

test_that("the icd-10 elix comorbidity map data is exactly as produced by the generator", {
  expect_equivalent(icd10_map_elix, icd10_generate_map_elix(save_data = FALSE))
})

test_that("the icd-10 ahrq comorbidity map data is exactly as produced by the generator", {
  if (is.null(icd10_fetch_ahrq_sas(offline = TRUE)))
    skip("AHRQ ICD-10 SAS must be downloaded with icd10_fetch_ahrq_sas")
  expect_equivalent(icd10_map_ahrq, icd10_parse_ahrq_sas(save_data = FALSE))
})

test_that("the classes of the ICD-10 maps are correct", {
  maps <- named_list(icd10_map_ahrq, icd10_map_elix, icd10_map_quan_deyo, icd10_map_quan_elix)
  for (m in names(maps)) {
    # for each map, verify it has class map, and that all it's elements are ICD-10 short diag format
    expect_identical(class(maps[[m]]), c("icd_comorbidity_map", "list"), info = paste("map: ", m))
    for (cbd in names(maps[[m]])) {
      y <- maps[[m]][[cbd]]
      expect_true(is.icd10(y), info = paste("map: ", m, " cbd = ", cbd))
      expect_true(is.icd_short_diag(y), info = paste("map: ", m, " cbd = ", cbd))
    }
  }
})

test_that("the class of each element of the quan elix map is correct", {
  for (i in names(icd10_map_quan_elix)) {
    expect_is(icd10_map_quan_elix[[i]], class = c("icd10", "character"), info = i)
    # although we derive the map from ICD-10-CM, it need not be so, and it may be more broad in the future:
    expect_false(inherits(icd10_map_quan_elix[[i]], "icd10cm"), info = i)
    # not a list, it is a character vector
    expect_false(inherits(icd10_map_quan_elix[[i]], "list"), info = i)
    # this is the parent class, not the class of the elements
    expect_false(inherits(icd10_map_quan_elix[[i]], "icd_comorbidity_map"), info = i)
  }
})

test_that("independently created list of Quan Elixhauser codes all appear", {
  quan_elix_independent <- list(
    chf = c("I099", "I110", "I130", "I132", "I255", "I420", "I425", "I426", "I427", "I428", "I429", "I43", "I50", "P290"),
    arrhythmia = c("I441", "I442", "I443", "I456", "I459", "I47", "I48", "I49", "R000", "R001", "R008", "T821", "Z450", "Z950"),
    valvular = c("A520", "I05", "I06", "I07", "I08", "I091", "I098", "I34", "I35",
                 "I36", "I37", "I38", "I39", "Q230", "Q231", "Q232", "Q233", "Z952", "Z953", "Z954"),
    pulmonary_circ = c("I26", "I27", "I280", "I288", "I289"),
    pvd = c("I70", "I71", "I731", "I738", "I739", "I771", "I790", "I792", "K551", "K558", "K559", "Z958", "Z959"),
    # TODO? shoudl htn simple and complex be merged
    htn = c("I10"),
    htncx = c("I11", "I12", "I13", "I15"),
    paralysis = c("G041", "G114", "G801", "G802", "G81", "G82", "G830", "G831", "G832", "G833", "G834", "G839"),
    neuro = c("G10", "G11", "G12", "G13", "G20", "G21", "G22", "G254", "G255", "G312", "G318",
              "G319", "G32", "G35", "G36", "G37", "G40", "G41", "G931", "G934", "R470", "R56"),
    copd = c("I278", "I279", "J40", "J41", "J42", "J43", "J44", "J45", "J46", "J47", "J60",
             "J61", "J62", "J63", "J64", "J65", "J66", "J67", "J684", "J701", "J703"),
    dm_s = c("E100", "E101", "E109", "E110", "E111", "E119", "E120", "E121", "E129", "E130", "E131", "E139", "E140", "E141", "E149"),
    dm_cx = c("E102", "E103", "E104", "E105", "E106", "E107", "E108", "E112", "E113", "E114", "E115", "E116", "E117",
              "E118", "E122", "E123", "E124", "E125", "E126", "E127", "E128", "E132", "E133", "E134", "E135", "E136", "E137", "E138",
              "E142", "E143", "E144", "E145", "E146", "E147", "E148"),
    thyroid = c("E00", "E01", "E02", "E03", "E890"),
    renal = c("I120", "I131", "N18", "N19", "N250", "Z490", "Z491", "Z492", "Z940", "Z992"),
    liver = c("B18", "I85", "I864", "I982", "K70", "K711", "K713", "K714", "K715", "K717", "K72", "K73",
              "K74", "K760", "K762", "K763", "K764", "K765", "K766", "K767", "K768", "K769", "Z944"),
    ulcer = c("K257", "K259", "K267", "K269", "K277", "K279", "K287", "K289"),
    hiv = c("B20", "B21", "B22", "B24"),
    lymphoma = c("C81", "C82", "C83", "C84", "C85", "C88", "C96", "C900", "C902"),
    mets = c("C77", "C78", "C79", "C80"),
    tumor = c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13", "C14",
              "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C30", "C31", "C32", "C33", "C34",
              "C37", "C38", "C39", "C40", "C41", "C43", "C45", "C46", "C47", "C48", "C49", "C50", "C51", "C52", "C53", "C54", "C55",
              "C56", "C57", "C58", "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69", "C70", "C71", "C72", "C73",
              "C74", "C75", "C76", "C97"),
    arthritis = c("L940", "L941", "L943", "M05", "M06", "M08", "M120", "M123", "M30", "M310",
                  "M311", "M312", "M313", "M32", "M33", "M34", "M35", "M45", "M461", "M468", "M469"),
    coag = c("D65", "D66", "D67", "D68", "D691", "D693", "D694", "D695", "D696"),
    obesity = c("E66"),
    weight_loss = c("E40", "E41", "E42", "E43", "E44", "E45", "E46", "R634", "R64"),
    fluid = c("E222", "E86", "E87"),
    blood_loss = c("D500"),
    anemia = c("D508", "D509", "D51", "D52", "D53"),
    etoh = c("F10", "E52", "G621", "I426", "K292", "K700", "K703", "K709", "T51", "Z502", "Z714", "Z721"),
    drugs = c("F11", "F12", "F13", "F14", "F15", "F16", "F18", "F19", "Z715", "Z722"),
    psych = c("F20", "F22", "F23", "F24", "F25", "F28", "F29", "F302", "F312", "F315"),
    depression = c("F204", "F313", "F314", "F315", "F32", "F33", "F341", "F412", "F432"))

  # this list is just parent codes, whereas I store, for ICD-10, the icd-10-cm children also.
  for (i in 1:30) {
    indep <- quan_elix_independent[[i]]
    indep_kids <- quan_elix_independent[[i]] %>% icd_children_defined.icd10cm
    canon <- icd10_map_quan_elix[[i]]
    expect_equal(setdiff(indep, canon), character(),
                 info = paste("checking quan elix canonical in indep: ", i, " - ", names(quan_elix_independent)[i]))
    expect_equal(setdiff(indep_kids, canon), character(),
                 info = paste("checking quan elix canonical in indep_kids: ", i, " - ", names(quan_elix_independent)[i]))
    # yes, there are non-ICD-10-CM codes which are not in the ICD-10-CM children
    # (without including the codes themselves)

    skip("his test reveals that ICD 10 CM codes may not be represented in ICD 10 WHO...")
    expect_equal(setdiff(canon, indep_kids), character(),
                 info = paste("checking quan elix indep in canonical: ", i, " - ",
                              names(quan_elix_independent)[i]))
  }

})

test_that("some hand-picked ICD-10 codes appear in the quan elix map", {
  expect_true("M12019" %in% icd::icd10_map_quan_elix$Rheumatic)
  #TODO more
})

test_that("some hand-picked ICD-10 codes appear in the quan deyo map", {
  expect_true("I214" %in% icd::icd10_map_quan_deyo$MI)
  #TODO more
})
