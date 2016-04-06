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

# some ICD-9 and ICD-10 mappings (others generated from SAS code parsing)

#' Generate Elixhauser comorbidities
#'
#' This function uses the \code{\%i9d\%} operator, so cannot be done as an R
#' file in the \code{data} directory. The data is documented in
#' \code{datadocs.R}.
#' @template parse-template
#' @keywords internal
icd9_generate_map_elix <- function(condense = NULL, save_data = TRUE, path = NULL) {
  if (!missing(condense) || !missing(path))
    warning("'condense' and 'path' are deprecated in icd9_generate_map_elix",
            call. = FALSE)
  icd9_map_elix <- list(
    chf = c("398.91", "402.11", "402.91", "404.11", "404.13", "404.91",
            "404.93", "428.0" %i9da% "428.9"),
    arrhythmia = c("426.1", "426.11", "426.13", "426.2" %i9da% "426.53",
                   "426.6" %i9da% "426.89", "427.0", "427.2", "427.31", "427.60",
                   "427.9", "785", "V45.0", "V53.3"),
    valve = c("93.20" %i9da% "93.24", "394.0" %i9da% "397.1",
              "424.0" %i9da% "424.91", "746.3" %i9da% "746.6", "V42.2", "V43.3"),
    pulm.circ = c("416.0" %i9da% "416.9", " 417.9"),
    pvd = c("440.0" %i9da% "440.9", "441.2", "441.4", "441.7", "441.9",
            "443.1" %i9da% "443.9", "447.1", "557.1", "557.9", "V43.4"),
    htn = c("401.1", "401.9"),
    htncx = c("402.10", "402.90", "404.10", "404.90", "405.11", "405.19",
              "405.91", "405.99"),
    paralysis = c("342.0" %i9da% "342.12", "342.9" %i9da% "344.9"),
    neuro.other = c("331.9", "332.0", "333.4", "333.5", "334.0" %i9da% "335.9",
                    "340", "341.1" %i9da% "341.9", "345.00" %i9da% "345.11",
                    "345.40" %i9da% "345.51", "345.80" %i9da% "345.91", "348.1",
                    "348.3", "780.3", "784.3"),
    chronic.pulm = c("490" %i9da% "492.8", "493.00" %i9da% "493.91", "494",
                     "495.0" %i9da% "505", "506.4"),
    dm.uncomp = c("250.00" %i9da% "250.33"),
    dm.comp = c("250.40" %i9da% "250.73", "250.90" %i9da% "250.93"),
    hypothyroid = c("243" %i9da% "244.2", "244.8", "244.9"),
    renal = c("403.11", "403.91", "404.12", "404.92", "585", "586", "V42.0",
              "V45.1", "V56.0", "V56.8"),
    liver = c("70.32", "70.33", "70.54", "456.0", "456.1", "456.20", "456.21",
              "571.0", "571.2", "571.3", "571.40" %i9da% "571.49", "571.5",
              "571.6", "571.8", "571.9", "572.3", "572.8", "V42.7"),
    pud = c("531.70", "531.90", "532.70", "532.90", "533.70", "533.90",
            "534.70", "534.90", "V12.71"),
    hiv = c("42" %i9da% "44.9"),
    lymphoma = c("200.00" %i9da% "202.38", "202.50" %i9da% "203.01",
                 "203.8" %i9da% "203.81", "238.6", "273.3", "V10.71", "V10.72",
                 "V10.79"),
    mets = c("196.0" %i9da% "199.1"),
    solid.tumor = c("140.0" %i9da% "172.9", "174.0" %i9da% "175.9",
                    "179" %i9da% "195.8", "V10.00" %i9da% "V10.9"),
    rheum = c("701.0", "710.0" %i9da% "710.9", "714.0" %i9da% "714.9",
              "720.0" %i9da% "720.9", "725"),
    coag = c("286.0" %i9da% "286.9", "287.1", "287.3" %i9da% "287.5"),
    obesity = c("278.0"),
    wt.loss = c("260" %i9da% "263.9"),
    lytes = c("276.0" %i9da% "276.9"),
    anemia.loss = c("280.0"),
    anemia.def = c("280.1" %i9da% "281.9", "285.9"),
    etoh = c("291.1", "291.2", "291.5", "291.8", "291.9",
             "303.90" %i9da% "303.93", "305.00" %i9da% "305.03", "V11.3"),
    drugs = c("292.0", "292.82" %i9da% "292.89", "292.9",
              "304.00" %i9da% "304.93", "305.20" %i9da% "305.93"),
    psychoses = c("295.00" %i9da% "298.9", "299.10" %i9da% "299.11"),
    depression = c("300.4", "301.12", "309.0", "309.1", "311")
  )

  icd9_map_elix <- lapply(icd9_map_elix, icd_decimal_to_short.icd9)

  icd9_map_elix <- lapply(
    icd9_map_elix,
    icd_children.icd9, short_code = TRUE, defined = FALSE)

  names(icd9_map_elix) <- icd::icd_names_elix_htn_abbrev
  icd9_map_elix %<>% as.icd_comorbidity_map

  if (save_data)
    save_in_data_dir(icd9_map_elix)
  invisible(icd9_map_elix)
}

#' @rdname icd9_generate_map_elix
#' @keywords internal
icd10_generate_map_elix <- function(save_data = TRUE) {
  icd10_map_elix <- list(
    chf = c("I099", "I110", "I130", "I132", "I255", "I420", "I425", "I426", "I427", "I428", "I429",
            "I43", "I50", "P290"),
    arrhythmia = c("I441", "I442", "I443", "I456", "I459", "I47", "I48", "I49", "R000", "R001",
                   "R008", "T821", "Z450", "Z950"),
    valve = c("A520", "I05", "I06", "I07", "I08", "I091", "I098", "I34", "I35", "I36", "I37", "I38",
              "I39", "Q230", "Q231", "Q232", "Q233", "Z952", "Z953", "Z954"),
    pulm.circ = c("I26", "I27", "I280", "I288", "I289"),
    pvd = c("I70", "I71", "I731", "I738", "I739", "I771", "I790", "I792", "K551", "K558", "K559",
            "Z958", "Z959"),
    htn = c("I10"),
    htncx = c("I11", "I12", "I13", "I15"),
    paralysis = c("G041", "G114", "G801", "G802", "G81", "G82", "G830", "G831", "G832", "G833",
                  "G834", "G839"),
    neuro.other = c("G10", "G11", "G12", "G13", "G20", "G21", "G22", "G254", "G255", "G312", "G318",
                    "G319", "G32", "G35", "G36", "G37", "G40", "G41", "G931", "G934", "R470", "R56"),
    chronic.pulm = c("I278", "I279", "J40", "J41", "J42", "J43", "J44", "J45", "J46", "J47", "J60",
                     "J61", "J62", "J63", "J64", "J65", "J66", "J67", "J684", "J701", "J703"),
    dm.uncomp = c("E100", "E101", "E109", "E110", "E111", "E119", "E120", "E121", "E129", "E130",
                  "E131", "E139", "E140", "E141", "E149"),
    dm.comp = c("E102", "E103", "E104", "E105", "E106", "E107", "E108", "E112", "E113", "E114",
                "E115", "E116", "E117", "E118", "E122", "E123", "E124", "E125", "E126", "E127",
                "E128", "E132", "E133", "E134", "E135", "E136", "E137", "E138", "E142", "E143",
                "E144", "E145", "E146", "E147", "E148"),
    hypothyroid = c("E00", "E01", "E02", "E03", "E890"),
    renal = c("I120", "I131", "N18", "N19", "N250", "Z490", "Z491", "Z492", "Z940", "Z992"),
    liver = c("B18", "I85", "I864", "I982", "K70", "K711", "K713", "K714", "K715", "K717", "K72",
              "K73", "K74", "K760", "K762", "K763", "K764", "K765", "K766", "K767", "K768", "K769",
              "Z944"),
    pud = c("K257", "K259", "K267", "K269", "K277", "K279", "K287", "K289"),
    hiv = c("B20", "B21", "B22", "B24"),
    lymphoma = c("C81", "C82", "C83", "C84", "C85", "C88", "C96", "C900", "C902"),
    mets = c("C77", "C78", "C79", "C80"),
    solid.tumor = c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10",
                    "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21",
                    "C22", "C23", "C24", "C25", "C26", "C30", "C31", "C32", "C33", "C34", "C37",
                    "C38", "C39", "C40", "C41", "C43", "C45", "C46", "C47", "C48", "C49", "C50",
                    "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58", "C60", "C61", "C62",
                    "C63", "C64", "C65", "C66", "C67", "C68", "C69", "C70", "C71", "C72", "C73",
                    "C74", "C75", "C76", "C97"),
    rheum = c("L940", "L941", "L943", "M05", "M06", "M08", "M120", "M123", "M30", "M310", "M311",
              "M312", "M313", "M32", "M33", "M34", "M35", "M45", "M461", "M468", "M469"),
    coag = c("D65", "D66", "D67", "D68", "D691", "D693", "D694", "D695", "D696"),
    obesity = c("E66"),
    wt.loss = c("E40", "E41", "E42", "E43", "E44", "E45", "E46", "R634", "R64"),
    lytes = c("E222", "E86", "E87"),
    anemia.loss = c("D500"),
    anemia.def = c("D508", "D509", "D51", "D52", "D53"),
    etoh = c("F10", "E52", "G621", "I426", "K292", "K700", "K703", "K709", "T51", "Z502", "Z714",
             "Z721"),
    drugs = c("F11", "F12", "F13", "F14", "F15", "F16", "F18", "F19", "Z715", "Z722"),
    psychoses = c("F20", "F22", "F23", "F24", "F25", "F28", "F29", "F302", "F312", "F315"),
    depression = c("F204", "F313", "F314", "F315", "F32", "F33", "F341", "F412", "F432")
  )

  # TODO: either generate children (probably infeasible for ICD-10), or generate
  # comorbid algorithm which matches against the listed parents

  # icd10_map_elix <- lapply(
  #   icd10_map_elix,
  #   icd_children.icd10cm, short_code = TRUE, defined = FALSE)

  names(icd10_map_elix) <- icd::icd_names_elix_htn_abbrev

  icd10_map_elix <- lapply(icd10_map_elix, as.icd_short_diag)
  icd10_map_elix <- lapply(icd10_map_elix, as.icd10)
  icd10_map_elix <- as.icd_comorbidity_map(icd10_map_elix)

  if (save_data)
    save_in_data_dir(icd10_map_elix)
  invisible(icd10_map_elix)
}

#' Generate Quan's revised Elixhauser comorbidities
#'
#' Generate Quan's revised Elixhauser comorbidities, expanded out
#'   to include all possible ICD-9 codes.
#' @template parse-template
#' @keywords internal
icd9_generate_map_quan_elix <- function(condense = NULL, save_data = TRUE, path = NULL) {

  if (!missing(condense) || !missing(path))
    warning("'condense' and 'path' are deprecated in icd9_generate_map_elix, and no longer has any effect.
The map can be condensed using other functions in the package. 'path' is the data dir in working tree.",
            call. = FALSE)
  # TODO: need to deprecate this name so we can switch ICD-9 and ICD-10 (and
  # their variations)
  icd9_map_quan_elix <- list(
    chf = c("398.91", "402.01", "402.11", "402.91", "404.01", "404.03",
            "404.11", "404.13", "404.91", "404.93", "425.4" %i9da% "425.9",
            "428"),
    arrhythmia = c("426.0", "426.13", "426.7", "426.9", "426.10", "426.12",
                   "427.0" %i9da% "427.4", "427.6" %i9da% "427.9", "785.0",
                   "996.01", "996.04", "V45.0", "V53.3"),
    valve = c("93.2", "394" %i9da% "397", "424", "746.3" %i9da% "746.6", "V42.2",
              "V43.3"),
    pulm.circ = c("415.0", "415.1", "416", "417.0", "417.8", "417.9"),
    pvd = c("093.0", "437.3", "440", "441", "443.1" %i9da% "443.9", "447.1",
            "557.1", "557.9", "V43.4"),
    htn = c("401"),
    htncx = c("402" %i9da% "405"),
    paralysis = c("334.1", "342", "343", "344.0" %i9da% "344.6", "344.9"),
    neuro.other = c("331.9", "332.0", "332.1", "333.4", "333.5", "333.92",
                    "334", "335", "336.2", "340", "341", "345", "348.1",
                    "348.3", "780.3", "784.3"),
    chronic.pulm = c("416.8", "416.9", "490" %i9da% "505", "506.4", "508.1",
                     "508.8"),
    dm.uncomp = c("250.0" %i9da% "250.3"),
    dm.comp = c("250.4" %i9da% "250.9"),
    hypothyroid = c("240.9", "243", "244", "246.1", "246.8"),
    renal = c("403.01", "403.11", "403.91", "404.02", "404.03", "404.12",
              "404.13", "404.92", "404.93", "585", "586", "588", "V42.0",
              "V45.1", "V56"),
    liver = c("70.22", "70.23", "70.32", "70.33", "70.44", "70.54", "70.6",
              "70.9", "456.0" %i9da% "456.2", "570", "571",
              "572.2" %i9da% "572.8", "573.3", "573.4", "573.8", "573.9",
              "V42.7"),
    pud = c("531.7", "531.9", "532.7", "532.9", "533.7", "533.9", "534.7",
            "534.9"),
    hiv = c("42" %i9da% "44"),
    lymphoma = c("200" %i9da% "202", "203.0", "238.6"),
    mets = c("196" %i9da% "199"),
    solid.tumor = c("140" %i9da% "172", "174" %i9da% "195"),
    rheum = c("446", "701.0", "710.0" %i9da% "710.4", "710.8", "710.9", "711.2",
              "714", "719.3", "720", "725", "728.5", "728.89", "729.30"),
    coag = c("286", "287.1", "287.3" %i9da% "287.5"),
    obesity = c("278.0"),
    wt.loss = c("260" %i9da% "263", "783.2", "799.4"),
    lytes = c("253.6", "276"),
    anemia.loss = c("280.0"),
    anemia.def = c("280.1" %i9da% "280.9", "281"),
    etoh = c("265.2", "291.1" %i9da% "291.3", "291.5" %i9da% "291.9", "303.0",
             "303.9", "305.0", "357.5", "425.5", "535.3", "571.0" %i9da% "571.3",
             "980", "V11.3"),
    drugs = c("292", "304", "305.2" %i9da% "305.9", "V65.42"),
    psychoses = c("293.8", "295", "296.04", "296.14", "296.44", "296.54", "297",
                  "298"),
    depression = c("296.2", "296.3", "296.5", "300.4", "309", "311")
  )

  icd9_map_quan_elix <- lapply(
    icd9_map_quan_elix,
    function(x) icd_decimal_to_short.icd9(x))

  icd9_map_quan_elix <- lapply(
    icd9_map_quan_elix,
    icd_children.icd9, short_code = TRUE, defined = FALSE)

  names(icd9_map_quan_elix) <- icd::icd_names_quan_elix_htn_abbrev
  icd9_map_quan_elix %<>% as.icd_comorbidity_map

  if (save_data)
    save_in_data_dir(icd9_map_quan_elix)
  invisible(icd9_map_quan_elix)
}

#' generate ICD-10 Quan/Elixhauser mapping
#'
#' @details Started with Quan's SAS code (in \code{data-raw}):
#` \code{grep %STR\(.*[[:digit:]] ICD10_Elixhauser.sas}
#' @template parse-template
#' @keywords internal
icd10_generate_map_quan_elix <- function(save_data = TRUE) {

  quan_elix_raw <- list(
    c("I099", "I110", "I130", "I132", "I255", "I420", "I425", "I426", "I427", "I428", "I429", "I43", "I50", "P290"),
    c("I441", "I442", "I443", "I456", "I459", "I47", "I48", "I49", "R000", "R001", "R008", "T821", "Z450", "Z950"),
    c("A520", "I05", "I06", "I07", "I08", "I091", "I098", "I34", "I35", "I36", "I37", "I38", "I39", "Q230", "Q231",
      "Q232", "Q233", "Z952", "Z953", "Z954"),
    c("I26", "I27", "I280", "I288", "I289"),
    c("I70", "I71", "I731", "I738", "I739", "I771", "I790", "I792", "K551", "K558", "K559", "Z958", "Z959"),
    c("I10"),
    c("I11", "I12", "I13", "I15"),
    c("G041", "G114", "G801", "G802", "G81", "G82", "G830", "G831", "G832", "G833", "G834", "G839"),
    c("G10", "G11", "G12", "G13", "G20", "G21", "G22", "G254", "G255", "G312", "G318", "G319", "G32", "G35", "G36",
      "G37", "G40", "G41", "G931", "G934", "R470", "R56"),
    c("I278", "I279", "J40", "J41", "J42", "J43", "J44", "J45", "J46", "J47", "J60", "J61", "J62", "J63", "J64", "J65",
      "J66", "J67", "J684", "J701", "J703"),
    c("E100", "E101", "E109", "E110", "E111", "E119", "E120", "E121", "E129", "E130", "E131", "E139", "E140", "E141",
      "E149"),
    c("E102", "E103", "E104", "E105", "E106", "E107", "E108", "E112", "E113", "E114", "E115", "E116", "E117", "E118",
      "E122", "E123", "E124", "E125", "E126", "E127", "E128", "E132", "E133", "E134", "E135", "E136", "E137", "E138",
      "E142", "E143", "E144", "E145", "E146", "E147", "E148"),
    c("E00", "E01", "E02", "E03", "E890"),
    c("I120", "I131", "N18", "N19", "N250", "Z490", "Z491", "Z492", "Z940", "Z992"),
    c("B18", "I85", "I864", "I982", "K70", "K711", "K713", "K714", "K715", "K717", "K72", "K73", "K74", "K760", "K762",
      "K763", "K764", "K765", "K766", "K767", "K768", "K769", "Z944"),
    c("K257", "K259", "K267", "K269", "K277", "K279", "K287", "K289"),
    c("B20", "B21", "B22", "B24"),
    c("C81", "C82", "C83", "C84", "C85", "C88", "C96", "C900", "C902"),
    c("C77", "C78", "C79", "C80"),
    c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13", "C14", "C15",
      "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C30", "C31", "C32", "C33", "C34",
      "C37", "C38",
      "C39", "C40", "C41", "C43", "C45", "C46", "C47", "C48", "C49", "C50", "C51", "C52", "C53", "C54", "C55", "C56",
      "C57", "C58", "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69", "C70", "C71", "C72", "C73",
      "C74", "C75", "C76", "C97"),
    c("L940", "L941", "L943", "M05", "M06", "M08", "M120", "M123", "M30", "M310", "M311", "M312", "M313", "M32", "M33",
      "M34", "M35", "M45", "M461", "M468", "M469"),
    c("D65", "D66", "D67", "D68", "D691", "D693", "D694", "D695", "D696"),
    c("E66"),
    c("E40", "E41", "E42", "E43", "E44", "E45", "E46", "R634", "R64"),
    c("E222", "E86", "E87"),
    c("D500"),
    c("D508", "D509", "D51", "D52", "D53"),
    c("F10", "E52", "G621", "I426", "K292", "K700", "K703", "K709", "T51", "Z502", "Z714", "Z721"),
    drugs = c("F11", "F12", "F13", "F14", "F15", "F16", "F18", "F19", "Z715", "Z722"),
    c("F20", "F22", "F23", "F24", "F25", "F28", "F29", "F302", "F312", "F315"),
    c("F204", "F313", "F314", "F315", "F32", "F33", "F341", "F412", "F432")
  )
  # there are 31 items in the list: hypertension is typically combined into one
  # category, whereas diabetes is kept as two categories
  names(quan_elix_raw) <- icd::icd_names_elix_htn_abbrev

  # this expansion will only be for 'defined' codes (currently the most
  # up-to-date canonical CMS ICD-10-CM list). Will ultimately need to generalize
  # this.

  # this function accounts for the fact that some Quan Elixhauser ICD-10 codes
  # are not in fact defined in ICD-10-CM, and currently generation of ICD-10
  # children is limited to 'defined' ones, but only as defined in ICD-10-CM, not
  # ICD-10 in general.
  f <- function(x, verbose = TRUE) {
    if (verbose)
      message("f working on: ", paste(x, collapse = " "))
    kids <- icd_children_defined.icd10cm(x, short_code = TRUE)
    c(kids, x) %>% unique %>% icd_sort.icd10
  }

  icd10_map_quan_elix <- lapply(quan_elix_raw, f)

  # It does appear that there are numerous codes in the Quan Elixhauser scheme
  # which are not present (?anymore) in the ICD-10-CM 2016 list.
  icd10_map_quan_elix <- lapply(icd10_map_quan_elix, as.icd_short_diag)
  icd10_map_quan_elix <- lapply(icd10_map_quan_elix, as.icd10)
  icd10_map_quan_elix %<>% as.icd_comorbidity_map

  if (save_data)
    save_in_data_dir(icd10_map_quan_elix)
  invisible(icd10_map_quan_elix)
}

#' Generate Quan mapping for Charlson categories of ICD-10 codes
#'
#' Based on Quan's SAS lists, transcribed by wmurphyrd
#' @template parse-template
#' @keywords internal
icd10_generate_map_quan_deyo <- function(save_data = TRUE) {
  quan_charl_raw <- list(
    mi = c("I21", "I22", "I252"),
    chf = c("I43", "I50", "I099", "I110", "I130", "I132",
            "I255", "I420", "I425", "I426", "I427", "I428", "I429", "P290"),
    pvd = c("I70", "I71", "I731", "I738", "I739", "I771",
            "I790", "I792", "K551", "K558", "K559", "Z958", "Z959"),
    cvd = c("G45", "G46", "I60", "I61", "I62", "I63", "I64", "I65", "I66", "I67", "I68", "I69", "H340"),
    dementica = c("F00", "F01", "F02", "F03", "G30", "F051", "G311"),
    copd = c("J40", "J41", "J42", "J43", "J44", "J45", "J46", "J47",
             "J60", "J61", "J62", "J63", "J64", "J65", "J66", "J67",
             "I278", "I279", "J684", "J701", "J703"),
    rheum = c("M05", "M32", "M33", "M34", "M06", "M315", "M351", "M353", "M360"),
    ulcer = c("K25", "K26", "K27", "K28"),
    liver_m = c("B18", "K73", "K74", "K700", "K701", "K702", "K703", "K709",
                "K717", "K713", "K714", "K715", "K760", "K762", "K763", "K764", "K768", "K769", "Z944"),
    dm_s = c("E100", "E101", "E106", "E108", "E109", "E110", "E111", "E116", "E118", "E119",
             "E120", "E121", "E126", "E128", "E129",
             "E130", "E131", "E136", "E138", "E139",
             "E140", "E141", "E146", "E148", "E149"),
    dm_cx = c("E102", "E103", "E104", "E105", "E107",
              "E112", "E113", "E114", "E115", "E117",
              "E122", "E123", "E124", "E125", "E127",
              "E132", "E133", "E134", "E135", "E137",
              "E142", "E143", "E144", "E145", "E147"),
    para = c("G81", "G82", "G041", "G114", "G801", "G802",
             "G830", "G831", "G832", "G833", "G834", "G839"),
    renal = c("N18", "N19", "N052", "N053", "N054", "N055", "N056", "N057",
              "N250", "I120", "I131", "N032", "N033", "N034", "N035", "N036", "N037",
              "Z490", "Z491", "Z492", "Z940", "Z992"),
    cancer = c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",
               "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19",
               "C20", "C21", "C22", "C23", "C24", "C25", "C26",
               "C30", "C31", "C32", "C33", "C34", "C37", "C38", "C39",
               "C40", "C41", "C43", "C45", "C46", "C47", "C48", "C49",
               "C50", "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58",
               "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69",
               "C70", "C71", "C72", "C73", "C74", "C75", "C76",
               "C81", "C82", "C83", "C84", "C85", "C88",
               "C90", "C91", "C92", "C93", "C94", "C95", "C96", "C97"),
    liver_s = c("K704", "K711", "K721", "K729", "K765", "K766", "K767", "I850", "I859", "I864", "I982"),
    mets = c("C77", "C78", "C79", "C80"),
    hiv = c("B20", "B21", "B22", "B24")
  )

  names(quan_charl_raw) <- icd::icd_names_charlson_abbrev

  # this expansion will only be for 'defined' codes (currently the most
  # up-to-date canonical CMS ICD-10-CM list). Will ultimately need to generalize
  # this.

  # note that the children is actually an ICD-10-CM function. This needs
  # addressing. I already know that doing string matching is way too slow for
  # millions of rows, so some compromise of an exhaustive list of WHO/ICD-10-CM
  # children will likely be needed. Maybe generating a huge structure is still
  # worth it, even for ICD-10-CM, because I do end up cutting it back down to
  # size based on the input data before comorbidity matching.
  f <- function(x) {
    icd_children_defined.icd10cm(x, short_code = TRUE) %>%
      c(x) %>% unique %>% icd_sort.icd10
  }

  icd10_map_quan_deyo <- lapply(quan_charl_raw, f)

  icd10_map_quan_deyo <- lapply(icd10_map_quan_deyo, as.icd_short_diag)
  icd10_map_quan_deyo <- lapply(icd10_map_quan_deyo, as.icd10)
  icd10_map_quan_deyo %<>% as.icd_comorbidity_map

  # It does appear that there are numerous codes in the Quan Elixhauser scheme
  # which are not present (?anymore) in the ICD-10-CM 2016 list.
  if (save_data)
    save_in_data_dir(icd10_map_quan_deyo)
  invisible(icd10_map_quan_deyo)
}

fetch_uranium_pathology <- function(offline = TRUE) {
  unzip_to_data_raw(
    url = "http://www.ustur.wsu.edu/Case_Studies/Pathology/mdb/Pathology_Office2007.zip",
    file_name = "Pathology_Office2007.accdb",
    offline = offline)
}

#' generate uranium pathology data
#'
#' This is downloaded from \href{https://wsu.edu}{WSU} where it appears to be
#' provided in the public domain. It requires the file to be downloaded from
#' source, or already stored in the \code{data-raw} directory, and it appears
#' that the function \code{odbcConnectAccess2007} is only available in the
#' Windows build of \pkg{RODBC}.
#' @template parse-template
#' @source
#' \url{http://www.ustur.wsu.edu/Case_Studies/Pathology/mdb/Pathology_Office2007.zip}
#'
#' @keywords internal
generate_uranium_pathology <- function(save_data = TRUE, offline = TRUE) {

  requireNamespace("RODBC")
  stopifnot(length(utils::find("odbcConnectAccess2007")) > 0)

  assert_flag(save_data)
  assert_flag(offline)

  stopifnot(!is.null(file_path <- fetch_uranium_pathology(offline = offline)$file_path))

  # odbcConnectAccess2007 is only in the Windows version of RODBC
  channel <- RODBC::odbcConnectAccess2007(file_path)
  uranium_pathology <- RODBC::sqlFetch(channel, "qry_ICD-10")

  uranium_pathology <- uranium_pathology[, c("Case_No", "ICD-10_code")]
  names(uranium_pathology) <- c("case", "icd10")

  uranium_pathology <- uranium_pathology[order(uranium_pathology["case"]), ]

  row.names(uranium_pathology) <- 1:nrow(uranium_pathology)

  uranium_pathology <- as.icd10(as.icd_long_data(uranium_pathology))

  if (save_data)
    save_in_data_dir(uranium_pathology)
  invisible(uranium_pathology)
}

fetch_vermont_dx <- function(offline) {
  unzip_to_data_raw(
    url = "http://healthvermont.gov/research/hospital-utilization/VTINP13.zip",
    file_name = "VTINP13.TXT",
    offline = offline)
}

#' generate \code{vermont_dx} data
#'
#' Process data from \href{healthvermont.gov}{Health Vermont}
#' @template parse-template
#' @keywords internal
generate_vermont_dx <- function(save_data = TRUE, offline = TRUE) {

  assert_flag(save_data)
  assert_flag(offline)
  stopifnot(!is.null(vermont_raw_fp <- fetch_vermont_dx(offline = offline)))

  vermont_dx <- utils::read.csv(vermont_raw_fp$file_path,
                                stringsAsFactors = FALSE,
                                strip.white = TRUE,
                                nrows = 1001)[, c(74, 4, 6, 7, 11, 13:32)]
  vermont_dx <- vermont_dx[1:1000, ]
  age_group <- vermont_dx$intage
  attr(age_group, "class") <- "factor"
  attr(age_group, "levels") <- c("Under 1", "1-17", "18-24",
                                 "25-29", "30-34", "35-39",
                                 "40-44", "45-49", "50-54",
                                 "55-59", "60-64", "65-69",
                                 "70-74", "75 and over",
                                 "Unknown")
  sex <- vermont_dx$sex
  attr(sex, "class") <- "factor"
  attr(sex, "levels") <- c("male", "female", "unknown")
  vermont_dx$intage <- age_group
  vermont_dx$sex <- sex
  # death = 8 (other codes are for various discharge statuses)
  vermont_dx$dstat <- vermont_dx$dstat == 8
  names(vermont_dx)[c(1:5)] <- c("visit_id", "age_group", "sex", "death", "DRG")
  # TODO: or use class functions
  vermont_dx <- as.icd_wide_data(vermont_dx)
  dx_cols <- paste0("DX", 1:20)
  for (dc in dx_cols)
    vermont_dx[[dc]]  <- as.icd9cm(as.icd_short_diag(vermont_dx[[dc]]))

  if (save_data)
    save_in_data_dir(vermont_dx)
  invisible(vermont_dx)
}
