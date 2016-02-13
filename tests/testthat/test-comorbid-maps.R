# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

context("comorbidity maps")

test_that("try to induce c++ segfault bug", {
  expect_error(icd_comorbid.icd9(ahrqTestDat, map = icd::icd9_map_ahrq, short_code = TRUE), NA)
})

test_that("ahrq make sure all the children are listed in the saved data.", {
  skip("this is not true because we don't fill in EVERY (unreal) possible code
       when there is odd specification of the range in the SAS code.")
  for (i in names(icd::icd9_map_ahrq))
    expect_true(setequal(icd_children.icd9(icd::icd9_map_ahrq[[i]], short_code = TRUE, defined = FALSE), icd::icd9_map_ahrq[[i]]),
                info = paste("missing from saved ahrq comorbid (", i, "): ",
                             paste(setdiff(icd_children.icd9(icd::icd9_map_ahrq[[i]], defined = FALSE), icd::icd9_map_ahrq[[i]]),
                                   collapse = ", "
                             )
                )
    )
})

test_that("Elixhauser make sure all the children are listed in the saved data.", {
  for (i in icd::icd9_map_quan_elix)
    expect_equal(icd_children.icd9(i, defined = FALSE, short_code = TRUE), icd_sort.icd9(i))
})

test_that("Quan Charlson make sure all the children are listed in the saved data.", {
  for (i in icd::icd9_map_quan_deyo)
    expect_equal(icd_children.icd9(i, defined = FALSE, short_code = TRUE), sort(i))
})

test_that("Quan Elixhauser make sure all the children are listed in the saved data.", {
  for (i in icd::icd9_map_quan_elix)
    expect_equal(icd_children.icd9(i, defined = FALSE, short_code = TRUE), sort(i))
})

test_that("icd9 comorbidities are created correctly, and logical to binary conversion ok", {
  ptdf <- icd_comorbid.icd9(patientData, map = icd::icd9_map_ahrq, short_code = TRUE,
                            visit_name = "visitId", return_df = TRUE)

  expect_equal(names(ptdf), c("visitId", names(icd::icd9_map_ahrq)))

  expect_true(all(sapply(names(icd::icd9_map_ahrq),
                         function(x)
                           class(ptdf[, x])) == "logical"))
  ptdflogical <- logical_to_binary(ptdf)
  expect_true(all(sapply(names(icd::icd9_map_ahrq),
                         function(x)
                           class(ptdflogical[, x])) == "integer"))
  # do not expect all the rest of patient data to be returned - we
  # aren't responsible for aggregating other fields by visitId!
  expect_equal(dim(ptdf),
               c(length(unique(patientData[["visitId"]])),
                 1 + length(icd::icd9_map_ahrq)))
  expect_true(
    setequal(names(ptdf), c("visitId", names(icd::icd9_map_ahrq))))
  expect_true(
    setequal(names(ptdflogical), c("visitId", names(icd::icd9_map_ahrq))))

  expect_equal(
    logical_to_binary(data.frame(a = c("jack", "hayley"),
                                 b = c(TRUE, FALSE),
                                 f = c(TRUE, TRUE))),
    data.frame(a = c("jack", "hayley"),
               b = c(1, 0),
               f = c(1, 1))
  )
})

test_that("ahrq icd9 mappings generated from the current generation code", {
  # skip_on_cran()  # apparently this runs of wercker but not travis, presumably
  # because travis uses --as-cran or equivalent

  skip_slow_tests()

  # skip this test if either do_online_tests is FALSE, or if the downloaded file
  # is not already in data-raw
  if (is.null(icd9_fetch_ahrq_sas(offline = TRUE)))
    skip_online_tests("data-raw/comformat2012-2013.txt not available, so
                      skipping AHRQ SAS parsing test.")

  # same but from source data. Should be absolutely identical.
  expect_equal(result <- icd9_parse_ahrq_sas(save_data = FALSE), icd9_map_ahrq)
  expect_that(result, is_a("list"))
  expect_true(length(result) == 30)
  expect_equivalent(icd_get_invalid.icd_comorbidity_map(icd::icd9_map_ahrq), list())

})

test_that("Quan Charlson icd9 mappings are all
            generated from the current generation code", {
              # skip("generating code from SAS is now not distributed in package. Move this test to pre-build test dir. TODO")

              if (is.null(icd9_fetch_quan_deyo_sas(offline = TRUE, allow_missing = TRUE)))
                skip_online_tests("data-raw/ICD9_E_Charlson.sas not available.")

              expect_identical(icd9_map_quan_deyo, icd9_parse_quan_deyo_sas(save_data = FALSE))
              expect_equivalent(
                icd_get_invalid.icd_comorbidity_map(icd9_map_quan_deyo, short_code = TRUE),
                list())
            })

test_that("Quan Elixhauser icd9 mappings are all
            generated from the current generation code", {
              expect_identical(icd9_map_quan_elix, icd9_generate_map_quan_elix(save_data = FALSE))
              expect_equivalent(
                icd_get_invalid.icd_comorbidity_map(icd9_map_quan_elix, short_code = TRUE),
                list())
            })

test_that("Elixhauser icd9 mappings are all
            generated from the current generation code", {
              expect_identical(icd9_map_elix, icd9_generate_map_elix(save_data = FALSE))
              expect_equivalent(icd_get_invalid.icd_comorbidity_map(icd9_map_elix, short_code = TRUE), list())
            })

test_that("Elixhauser icd10 mappings are all
            generated from the current generation code", {
              expect_identical(icd10_map_elix, icd10_generate_map_elix(save_data = FALSE))
              expect_equivalent(icd_get_invalid.icd_comorbidity_map(icd10_map_elix, short_code = TRUE), list())
            })

test_that("can condense the big lists of comorbidities without errors", {
  # this is a useful but slow (8s on my PC) test because the data weren't
  # generated by just expanding base ranges (which is how the condense works in
  # reverse)

  skip_on_cran()
  skip_slow_tests()

  for (onlyReal in c(TRUE, FALSE)) {
    if (onlyReal) {
      expect_warning(ahrq <- lapply(icd::icd9_map_ahrq, icd_condense.icd9, short_code = TRUE, defined = onlyReal))
      expect_warning(quanDeyo <- lapply(icd::icd9_map_quan_deyo, icd_condense.icd9, short_code = TRUE, defined = onlyReal))
      expect_warning(quanElix <- lapply(icd::icd9_map_quan_elix, icd_condense.icd9, short_code = TRUE, defined = onlyReal))
      expect_warning(elix <- lapply(icd::icd9_map_elix, icd_condense.icd9, defined = onlyReal))
    }
    else {
      expect_warning(ahrq <- lapply(icd::icd9_map_ahrq, icd_condense.icd9, short_code = TRUE, defined = onlyReal), NA)
      expect_warning(quanDeyo <- lapply(icd::icd9_map_quan_deyo, icd_condense.icd9, short_code = TRUE, defined = onlyReal), NA)
      expect_warning(quanElix <- lapply(icd::icd9_map_quan_elix, icd_condense.icd9, short_code = TRUE, defined = onlyReal), NA)
      expect_warning(elix <- lapply(icd::icd9_map_elix, icd_condense.icd9, short_code = TRUE, defined = onlyReal), NA)
    }

    expect_is(ahrq, class = "list")
    expect_is(elix, class = "list")
    expect_is(quanDeyo, class = "list")
    expect_is(quanElix, class = "list")
    # the comorbidity mappings save in \code{data} should not be condensed.
    expect_false(isTRUE(all.equal(ahrq, icd::icd9_map_ahrq)))
    expect_false(isTRUE(all.equal(quanDeyo, icd::icd9_map_quan_deyo)))
    expect_false(isTRUE(all.equal(quanElix, icd::icd9_map_quan_elix)))
    expect_false(isTRUE(all.equal(elix, icd::icd9_map_elix)))
  }
})

test_that("icd9cm_hierarchy as saved in data can be recreated as expected", {
  skip_slow_tests()
  skip_flat_icd9_avail_all()

  cmh_headings <- c("code", "short_desc", "long_desc", "threedigit",
                    "major", "subchapter", "chapter")
  cmh <- icd9cm_generate_chapters_hierarchy(save_data = FALSE, verbose = FALSE)
  for (h in cmh_headings)
    expect_equal(cmh[[h]], icd::icd9cm_hierarchy[[h]], info = h)
})

test_that("Charlson Deyo doesn't double count disease with two severities", {
  expect_false(any(icd9_map_quan_deyo$LiverMild %in% icd9_map_quan_deyo$LiverSevere))
  expect_false(any(icd9_map_quan_deyo$Cancer %in% icd9_map_quan_deyo$Mets))
  expect_false(any(icd9_map_quan_deyo$DM %in% icd9_map_quan_deyo$DMcx))
})

test_that("Elixhauser doesn't double count disease with multiple severities", {
  expect_false(any(icd::icd9_map_quan_elix[["DM"]] %in%
                     icd::icd9_map_quan_elix[["DMcx"]] ))
  expect_false(any(icd::icd9_map_quan_elix[["Tumor"]] %in%
                     icd::icd9_map_quan_elix[["Mets"]] ))
  expect_false(any(elixComorbid[["DM"]] %in%
                     elixComorbid[["DMcx"]] ))
  expect_false(any(elixComorbid[["Tumor"]] %in%
                     elixComorbid[["Mets"]] ))
  expect_false(any(icd::icd9_map_ahrq[["DM"]] %in% icd::icd9_map_ahrq[["DMcx"]] ))
  expect_false(any(icd::icd9_map_ahrq[["Tumor"]] %in% icd::icd9_map_ahrq[["Mets"]] ))
})

# next couple of tests demonstrate that the interpreted data is correctly
# transcribed in cases where the data is structured differently, and also
# affirms that 'child' codes are included in the RData mappings in the package.
# E.g. if the mapping specifies "044", we do expect 111 total codes to be in the
# mapping 0440 04400 04401 etc. Ahrq
test_that("ICD-9 codes from SAS source AHRQ exist", {
  # specific codes that have had parsing problems in the past:
  expect_true("3970" %in% icd::icd9_map_ahrq$Valvular)
  expect_true("39706" %in% icd::icd9_map_ahrq$Valvular)
  expect_true("3971" %in% icd::icd9_map_ahrq$Valvular)
  expect_true("3979" %in% icd::icd9_map_ahrq$Valvular)
  # SAS source is "7463 "-"7466 "
  expect_true("7463" %in% icd::icd9_map_ahrq$Valvular)
  expect_true("7466" %in% icd::icd9_map_ahrq$Valvular)
  expect_true("74645" %in% icd::icd9_map_ahrq$Valvular)
  # "3420 "-"3449 ",
  # "43820"-"43853",
  # "78072"         = "PARA"      /* Paralysis */
  expect_true("43820" %in% icd::icd9_map_ahrq$Paralysis)
  expect_true("43822" %in% icd::icd9_map_ahrq$Paralysis)
  expect_true("43850" %in% icd::icd9_map_ahrq$Paralysis)
  expect_true("43852" %in% icd::icd9_map_ahrq$Paralysis)
  expect_true("43853" %in% icd::icd9_map_ahrq$Paralysis)
  # although 4385 implies an overly broad range, all its children are in the requested range, so it should appear.
  expect_true("4383" %in% icd::icd9_map_ahrq$Paralysis)
  expect_true("4384" %in% icd::icd9_map_ahrq$Paralysis)
  expect_true("4385" %in% icd::icd9_map_ahrq$Paralysis)
  expect_false("438" %in% icd::icd9_map_ahrq$Paralysis)
  expect_false("4386" %in% icd::icd9_map_ahrq$Paralysis)
  # neuro other problem codes
  #   "3411 "-"3419 ",
  #   "34500"-"34511",
  #   "3452 "-"3453 ",
  #   "34540"-"34591",
  #   "34700"-"34701",
  #   "34710"-"34711",
  expect_true("3337" %in% icd::icd9_map_ahrq$NeuroOther) # single value
  expect_true("33371" %in% icd::icd9_map_ahrq$NeuroOther) # single value sub-code - zero not defined in 2015
  expect_true("494" %in% icd::icd9_map_ahrq$Pulmonary) # top-level at start of range
  expect_true("4940" %in% icd::icd9_map_ahrq$Pulmonary) # value within range
  expect_true("49400" %in% icd::icd9_map_ahrq$Pulmonary) # sub-value within range
  expect_true("3450" %in% icd::icd9_map_ahrq$NeuroOther)
  expect_true("34500" %in% icd::icd9_map_ahrq$NeuroOther)
  expect_true("3451" %in% icd::icd9_map_ahrq$NeuroOther)
  expect_true("34511" %in% icd::icd9_map_ahrq$NeuroOther)
  expect_true("34519" %in% icd::icd9_map_ahrq$NeuroOther)
  expect_true("3452" %in% icd::icd9_map_ahrq$NeuroOther)
  expect_true("34529" %in% icd::icd9_map_ahrq$NeuroOther)
  expect_true("3453" %in% icd::icd9_map_ahrq$NeuroOther)
  expect_true("34539" %in% icd::icd9_map_ahrq$NeuroOther)
  expect_true("3459" %in% icd::icd9_map_ahrq$NeuroOther)
  expect_true("34599" %in% icd::icd9_map_ahrq$NeuroOther) # by implication
  #   "490  "-"4928 ",
  #   "49300"-"49392", # this is all of asthma
  #   "494  "-"4941 ", # bronchiectasis is just 494, 4940 and 4941
  #   "4950 "-"505  ",
  #   "5064 "         = "CHRNLUNG"  /* Chronic pulmonary disease */
  expect_true("492" %in% icd::icd9_map_ahrq$Pulmonary) # implied, and more below
  expect_true("4929" %in% icd::icd9_map_ahrq$Pulmonary)
  expect_true("4920" %in% icd::icd9_map_ahrq$Pulmonary)
  expect_true("4928" %in% icd::icd9_map_ahrq$Pulmonary)
  expect_true("493" %in% icd::icd9_map_ahrq$Pulmonary)
  expect_true("49392" %in% icd::icd9_map_ahrq$Pulmonary)
  expect_true("49300" %in% icd::icd9_map_ahrq$Pulmonary)
  expect_true("49322" %in% icd::icd9_map_ahrq$Pulmonary) # implied intermediate
  expect_true("494" %in% icd::icd9_map_ahrq$Pulmonary)
  expect_true("4940" %in% icd::icd9_map_ahrq$Pulmonary)
  expect_true("4941" %in% icd::icd9_map_ahrq$Pulmonary)
  expect_true("49499" %in% icd::icd9_map_ahrq$Pulmonary) # implied
  #   "25000"-"25033",
  #   "64800"-"64804",
  #   "24900"-"24931" = "DM"        /* Diabetes w/o chronic complications*/
  expect_false("249" %in% icd::icd9_map_ahrq$DM)
  expect_false("2494" %in% icd::icd9_map_ahrq$DM)
  expect_false("24941" %in% icd::icd9_map_ahrq$DM)
  expect_true("24900" %in% icd::icd9_map_ahrq$DM)
  expect_true("24931" %in% icd::icd9_map_ahrq$DM)
  expect_true("24939" %in% icd::icd9_map_ahrq$DM)
  expect_true("2493" %in% icd::icd9_map_ahrq$DM)
  expect_false("2504" %in% icd::icd9_map_ahrq$DM)
  expect_false("25043" %in% icd::icd9_map_ahrq$DM)
  expect_false("250" %in% icd::icd9_map_ahrq$DM)
  expect_true("25000" %in% icd::icd9_map_ahrq$DM)
  expect_true("25029" %in% icd::icd9_map_ahrq$DM) # implied
  expect_true("25033" %in% icd::icd9_map_ahrq$DM)
  expect_true("2503" %in% icd::icd9_map_ahrq$DM) # implied
  expect_true("25039" %in% icd::icd9_map_ahrq$DM) # implied
  #   "25040"-"25093",
  #   "7751 ",
  #   "24940"-"24991" = "DMCX"      /* Diabetes w/ chronic complications */
  expect_false("250" %in% icd::icd9_map_ahrq$DMcx)
  expect_false("2503" %in% icd::icd9_map_ahrq$DMcx)
  expect_true("2509" %in% icd::icd9_map_ahrq$DMcx) # implied
  expect_true("25093" %in% icd::icd9_map_ahrq$DMcx)
  expect_true("25099" %in% icd::icd9_map_ahrq$DMcx) # implied
  expect_false("249" %in% icd::icd9_map_ahrq$DMcx)
  expect_true("2499" %in% icd::icd9_map_ahrq$DMcx)
  expect_true("2498" %in% icd::icd9_map_ahrq$DMcx)
  expect_true("24999" %in% icd::icd9_map_ahrq$DMcx)
  expect_true("24991" %in% icd::icd9_map_ahrq$DMcx)
  #   "243  "-"2442 ",
  #   "2448 ",
  #   "2449 "         = "HYPOTHY"   /* Hypothyroidism */
  expect_false("244" %in% icd::icd9_map_ahrq$Hypothyroid) # some children are not included
  expect_false("2443" %in% icd::icd9_map_ahrq$Hypothyroid) # explicitly excluded by Quan
  expect_false("24430" %in% icd::icd9_map_ahrq$Hypothyroid) # implied exclusion
  expect_true("2442" %in% icd::icd9_map_ahrq$Hypothyroid)
  expect_true("243" %in% icd::icd9_map_ahrq$Hypothyroid) # top level billable code
  expect_true("2430" %in% icd::icd9_map_ahrq$Hypothyroid) # implied, doesn't exist
  expect_true("24300" %in% icd::icd9_map_ahrq$Hypothyroid) # implied
  expect_true("2448" %in% icd::icd9_map_ahrq$Hypothyroid)
  expect_true("2449" %in% icd::icd9_map_ahrq$Hypothyroid)
  expect_true("24480" %in% icd::icd9_map_ahrq$Hypothyroid)
  expect_true("24499" %in% icd::icd9_map_ahrq$Hypothyroid)
  #      "V560 "-"V5632",
  expect_true("V560" %in% icd::icd9_map_ahrq$Renal)
  expect_true("V563" %in% icd::icd9_map_ahrq$Renal)
  expect_true("V5632" %in% icd::icd9_map_ahrq$Renal)
  expect_true("V568" %in% icd::icd9_map_ahrq$Renal)
  expect_false("V56" %in% icd::icd9_map_ahrq$Renal)
  #   "20000"-"20238",
  #   "20250"-"20301",
  #   "2386 ",
  #   "2733 ",
  #   "20302"-"20382" = "LYMPH"     /* Lymphoma */
  expect_true("200" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("2000" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("20000" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("201" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("20100" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("20199" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_false("202" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_false("2024" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_false("20240" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_false("20248" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_false("20249" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("2025" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("20250" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("20258" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("20259" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("20298" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("20299" %in% icd::icd9_map_ahrq$Lymphoma)
  # 2030 and 203 are parents: problem because this range is split for some reason
  expect_true("2031" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("20310" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("20300" %in% icd::icd9_map_ahrq$Lymphoma)
  expect_true("20301" %in% icd::icd9_map_ahrq$Lymphoma)
  # "1960 "-"1991 ",
  expect_true("196" %in% icd::icd9_map_ahrq$Mets)
  expect_true("1960" %in% icd::icd9_map_ahrq$Mets)
  expect_true("19600" %in% icd::icd9_map_ahrq$Mets)
  expect_true("1969" %in% icd::icd9_map_ahrq$Mets)
  expect_true("19699" %in% icd::icd9_map_ahrq$Mets)
  expect_true("197" %in% icd::icd9_map_ahrq$Mets)
  expect_true("1970" %in% icd::icd9_map_ahrq$Mets)
  expect_true("19700" %in% icd::icd9_map_ahrq$Mets)
  expect_true("19799" %in% icd::icd9_map_ahrq$Mets)
  expect_true("198" %in% icd::icd9_map_ahrq$Mets)
  expect_true("1980" %in% icd::icd9_map_ahrq$Mets)
  expect_true("19800" %in% icd::icd9_map_ahrq$Mets)
  expect_true("19899" %in% icd::icd9_map_ahrq$Mets)
  expect_true("1990" %in% icd::icd9_map_ahrq$Mets)
  expect_true("19900" %in% icd::icd9_map_ahrq$Mets)
  expect_true("19909" %in% icd::icd9_map_ahrq$Mets)
  expect_true("1991" %in% icd::icd9_map_ahrq$Mets)
  expect_true("19910" %in% icd::icd9_map_ahrq$Mets)
  expect_true("19919" %in% icd::icd9_map_ahrq$Mets)
  expect_false("199" %in% icd::icd9_map_ahrq$Mets)
  expect_false("1992" %in% icd::icd9_map_ahrq$Mets)
  expect_false("19920" %in% icd::icd9_map_ahrq$Mets)
  expect_false("19929" %in% icd::icd9_map_ahrq$Mets)
  expect_false("1993" %in% icd::icd9_map_ahrq$Mets) # implied
  expect_false("19930" %in% icd::icd9_map_ahrq$Mets) # implied
  expect_false("19999" %in% icd::icd9_map_ahrq$Mets) # implied
  #   "179  "-"1958 ",
  #   "20900"-"20924",
  #   "20925"-"2093 ",
  #   "20930"-"20936",
  expect_true("195" %in% icd::icd9_map_ahrq$Tumor) # all children, so implied
  expect_true("1950" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("1958" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("19589" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("1959" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("19599" %in% icd::icd9_map_ahrq$Tumor)
  expect_false("209" %in% icd::icd9_map_ahrq$Tumor)
  expect_false("2094" %in% icd::icd9_map_ahrq$Tumor)
  expect_false("20940" %in% icd::icd9_map_ahrq$Tumor)
  expect_false("2099" %in% icd::icd9_map_ahrq$Tumor)
  expect_false("20999" %in% icd::icd9_map_ahrq$Tumor)
  expect_false("2097" %in% icd::icd9_map_ahrq$Tumor)
  expect_false("20979" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("20936" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("2093" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("20930" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("20939" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("2090" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("2091" %in% icd::icd9_map_ahrq$Tumor)
  # is range split between definitions? ideally this would be included, but it
  # is a corner case e.g. 2092
  expect_true("20900" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("20910" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("20920" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("20907" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("20917" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("20927" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("20909" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("20919" %in% icd::icd9_map_ahrq$Tumor)
  expect_true("20929" %in% icd::icd9_map_ahrq$Tumor)
  #   "2871 ",
  #   "2873 "-"2875 ", # coag
  expect_true("2871" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_true("28710" %in% icd::icd9_map_ahrq$Coagulopathy) # doesn't exist but really should work simply
  expect_true("28719" %in% icd::icd9_map_ahrq$Coagulopathy) # doesn't exist but really should work simply
  expect_false("287" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_false("2872" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_false("28720" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_false("28729" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_true("2873" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_true("28730" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_true("28739" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_true("2874" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_true("28741" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_true("28749" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_true("2875" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_true("28759" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_false("2876" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_false("28760" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_false("28769" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_false("2878" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_false("2879" %in% icd::icd9_map_ahrq$Coagulopathy)
  expect_false("28799" %in% icd::icd9_map_ahrq$Coagulopathy)
  #   "2910 "-"2913 ",
  #   "2915 ",
  #   "2918 ",
  #   "29181",
  #   "29182",
  #   "29189",
  #   "2919 ",
  #   "30300"-"30393",
  #   "30500"-"30503" = "ALCOHOL"   /* Alcohol abuse */
  expect_true("2910" %in% icd::icd9_map_ahrq$Alcohol)
  expect_true("2913" %in% icd::icd9_map_ahrq$Alcohol)
  expect_true("2915" %in% icd::icd9_map_ahrq$Alcohol)
  expect_true("2918" %in% icd::icd9_map_ahrq$Alcohol)
  expect_true("29181" %in% icd::icd9_map_ahrq$Alcohol)
  expect_true("29182" %in% icd::icd9_map_ahrq$Alcohol)
  expect_true("29189" %in% icd::icd9_map_ahrq$Alcohol)
  expect_true("2919" %in% icd::icd9_map_ahrq$Alcohol)
  expect_false("291" %in% icd::icd9_map_ahrq$Alcohol)
  expect_false("2914" %in% icd::icd9_map_ahrq$Alcohol)
  expect_false("29140" %in% icd::icd9_map_ahrq$Alcohol)
  expect_false("29149" %in% icd::icd9_map_ahrq$Alcohol)
  #   "2920 ",
  #   "29282"-"29289",
  #   "2929 ",
  #   "30400"-"30493",
  #   "30520"-"30593",
  #   "64830"-"64834" = "DRUG"      /* Drug abuse */
  expect_true("304" %in% icd::icd9_map_ahrq$Drugs)
  expect_true("3040" %in% icd::icd9_map_ahrq$Drugs)
  expect_true("30400" %in% icd::icd9_map_ahrq$Drugs)
  expect_true("3049" %in% icd::icd9_map_ahrq$Drugs)
  expect_true("30493" %in% icd::icd9_map_ahrq$Drugs)
  expect_false("305" %in% icd::icd9_map_ahrq$Drugs)
  expect_false("3050" %in% icd::icd9_map_ahrq$Drugs)
  expect_false("30500" %in% icd::icd9_map_ahrq$Drugs)
  expect_false("3051" %in% icd::icd9_map_ahrq$Drugs)
  expect_false("30510" %in% icd::icd9_map_ahrq$Drugs)
  expect_true("3052" %in% icd::icd9_map_ahrq$Drugs)
  expect_true("30520" %in% icd::icd9_map_ahrq$Drugs)
  expect_true("30523" %in% icd::icd9_map_ahrq$Drugs)
  expect_true("3059" %in% icd::icd9_map_ahrq$Drugs)
  expect_true("30593" %in% icd::icd9_map_ahrq$Drugs)

})

test_that("ICD-9 codes from SAS source for Quan Deyo Charlson exist", {
  # Quan Deyo Charlson
  # top level single value
  expect_true("410" %in% quanDeyoComorbid$MI)
  # this is not included (410 and 412 defined)
  expect_false("411" %in% quanDeyoComorbid$MI)
  # this is not included (410 and 412 defined)
  expect_false("41199" %in% quanDeyoComorbid$MI)
  # midlevel value, not from range
  expect_true("4100" %in% quanDeyoComorbid$MI)
  # lower-level value, not from range
  expect_true("41001" %in% quanDeyoComorbid$MI)
  # midlevel definition
  expect_true("2504" %in% quanDeyoComorbid$DMcx)
  # midlevel definition lower-level code
  expect_true("25041" %in% quanDeyoComorbid$DMcx)

})

# the following two tests cover the mappings in which there was no source SAS
# data, but the numbers were transcribed manually. This is therefore testing a
# little of the transcription, and also the elobration of codes definied in
# ranges
test_that("sample of ICD-9 codes from manually specified Quan Elix mapping exist", {
  expect_true("2500" %in% icd::icd9_map_quan_elix$DM)
  expect_true("2501" %in% icd::icd9_map_quan_elix$DM)
  expect_true("25011" %in% icd::icd9_map_quan_elix$DM)
  expect_true("276" %in% icd::icd9_map_quan_elix$FluidsLytes)
  expect_true("2761" %in% icd::icd9_map_quan_elix$FluidsLytes)
  expect_true("27612" %in% icd::icd9_map_quan_elix$FluidsLytes)
  # top level should not be included automatically
  expect_false("710" %in% icd::icd9_map_quan_elix$FluidsLytes)
})

test_that("sample of ICD-9 codes from manually specified Elixhauser mapping exist", {
  expect_true("09320" %in% elixComorbid$Valvular)
  expect_true("3971" %in% elixComorbid$Valvular)
  expect_true("V560" %in% elixComorbid$Renal)
  expect_true("V1090" %in% elixComorbid$Tumor) # child at end of a V range
})

test_that("github #34 - short and long custom map give different results", {
  mydf <- data.frame(visitId = c("a", "b", "b", "c"),
                     icd9 = c("1", "010", "10", "20"))

  mymaps <- list(jack = c("1", "2", "3"), alf = c("010", "20"))
  mymapd <- lapply(mymaps, icd_short_to_decimal.icd9)

  expect_identical(
    icd_comorbid.icd9(mydf, map = mymaps, short_code = TRUE),
    icd_comorbid.icd9(mydf, map = mymapd, short_code = FALSE)
  )
})

test_that("no NA values in the co-morbidity lists", {
  expect_false(anyNA(unlist(unname(icd::icd9_map_ahrq))))
  expect_false(anyNA(unlist(unname(icd::icd9_map_quan_deyo))))
  expect_false(anyNA(unlist(unname(icd::icd9_map_quan_elix))))
  expect_false(anyNA(unlist(unname(icd::icd9_map_elix))))
})

test_that("no duplicate values in the co-morbidity lists", {
  expect_false(any(as.logical(lapply(icd::icd9_map_ahrq, anyDuplicated))))
  expect_false(any(as.logical(lapply(icd::icd9_map_quan_deyo, anyDuplicated))))
  expect_false(any(as.logical(lapply(icd::icd9_map_quan_elix, anyDuplicated))))
  expect_false(any(as.logical(lapply(icd::icd9_map_elix, anyDuplicated))))
})

test_that("built-in icd9 to comorbidity mappings are all valid", {
  expect_true(icd_is_valid.icd_comorbidity_map(icd::icd9_map_ahrq, short_code = TRUE))
  expect_true(icd_is_valid.icd_comorbidity_map(icd::icd9_map_quan_elix, short_code = TRUE))
  expect_true(icd_is_valid.icd_comorbidity_map(icd::icd9_map_quan_elix, short_code = TRUE))
  expect_true(icd_is_valid.icd_comorbidity_map(icd::icd9_map_elix, short_code = TRUE))
})

test_that("disordered visit ids", {
  pts <- data.frame(visitId = c("2", "1", "2", "3", "3"),
                    icd9 = c("39891", "40110", "09322", "41514", "39891"))
  icd_comorbid.icd9(pts, icd::icd9_map_ahrq, short_code = TRUE)
})

test_that("diff comorbid works", {
  # TODO: S3 classes for this
  # list, but not list of character vectors
  expect_error(icd_diff_comorbid(bad_input, bad_input))

  # no warning or error for good data
  # TODO: should be testing correct dispatch here, too, since map is a different class.
  expect_warning(
    utils::capture.output(
      res <- icd_diff_comorbid(icd::icd9_map_ahrq, elixComorbid, show = FALSE)
    ),
    NA)

  expect_true(all(names(res) %in% c(
    "CHF", "Valvular", "PHTN", "PVD", "HTN", "HTNcx", "Paralysis",
    "NeuroOther", "Pulmonary", "DM", "DMcx", "Hypothyroid", "Renal",
    "Liver", "PUD", "HIV", "Lymphoma", "Mets", "Tumor", "Rheumatic",
    "Coagulopathy", "Obesity", "WeightLoss", "FluidsLytes", "BloodLoss",
    "Anemia", "Alcohol", "Drugs", "Psychoses", "Depression")))
  # one side diff
  expect_identical(res$Drugs[["only.y"]], character(0))
  # match
  expect_identical(res$Depression[[2]], character(0))
  expect_identical(res$Depression[[3]], character(0))

  # both, also with elements in either side set diff
  expect_equal(res$PUD$both, c("53170", "53270", "53370", "53470"))

  expect_warning(
    expect_output(
      resq <- icd_diff_comorbid(icd9_map_quan_elix, icd9_map_elix, show = TRUE),
      regexp = "Comorbidity Psychoses"
    ), NA)
})

twoPtsFac <- data.frame(visitId = c("v01", "v01", "v02", "v02"),
                        icd9 = c("040", "000", "100", "000"),
                        stringsAsFactors = TRUE)
twoMapFac <- as.list(data.frame("malady" = c("100", "2000"),
                                "ailment" = c("003", "040"),
                                stringsAsFactors = TRUE))

test_that("comorbid quick test", {
  testres <- icd_comorbid.icd9(twoPts, twoMap, return_df = TRUE)
  trueres <- data.frame("visitId" = c("v01", "v02"),
                        "malady" = c(FALSE, TRUE),
                        "ailment" = c(TRUE, FALSE),
                        stringsAsFactors = FALSE)
  expect_equal(testres, trueres)

  testmat <- icd_comorbid.icd9(twoPts, twoMap, return_df = FALSE)
  truemat <- matrix(c(FALSE, TRUE, TRUE, FALSE), nrow = 2,
                    dimnames = list(c("v01", "v02"), c("malady", "ailment")))
  expect_equal(testmat, truemat)

  testresfac <- icd_comorbid.icd9(twoPtsFac, twoMapFac, return_df = TRUE)
  trueresfac <- data.frame("visitId" = c("v01", "v02"),
                           "malady" = c(FALSE, TRUE),
                           "ailment" = c(TRUE, FALSE),
                           stringsAsFactors = TRUE)
  expect_equal(testresfac, trueresfac)
  expect_equal(icd_comorbid.icd9(twoPtsFac, twoMapFac), truemat)

})

test_that("control params don't affect result of comorbid calc", {
  pts <- generate_random_pts(101, 13)
  pts$visitId <- asCharacterNoWarn(pts$visitId)
  pts$icd9 <- as.factor(pts$icd9)
  upts <- length(unique(pts$visitId))
  ac <-  lapply(icd::icd9_map_ahrq, function(x) {
    f <- factor(x, levels(pts[["icd9"]]))
    f[!is.na(f)]
  })
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 1, chunkSize = 32),
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 3, chunkSize = 32)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 2, chunkSize = 32),
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 5, chunkSize = 32)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 3, chunkSize = 1),
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 3, chunkSize = 32)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 4, chunkSize = upts - 1),
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 4, chunkSize = upts)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 4, chunkSize = upts - 1),
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 4, chunkSize = upts + 1)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 4, chunkSize = upts + 1),
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 4, chunkSize = upts)
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 3, chunkSize = upts - 2, ompChunkSize = 1), # nolint
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 3, chunkSize = upts + 2, ompChunkSize = 1) # nolint
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 3, chunkSize = upts - 2, ompChunkSize = 11), # nolint
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 3, chunkSize = upts + 2, ompChunkSize = 11) # nolint
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 3, chunkSize = upts, ompChunkSize = 1), # nolint
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 3, chunkSize = upts, ompChunkSize = 11) # nolint
  )
  expect_identical(
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field"),
    icd9ComorbidShortCpp(pts, ac, visitId = "visitId", icd9Field = "icd9Field", threads = 3, chunkSize = 3, ompChunkSize = 5) # nolint
  )
})

test_that("failing example", {
  mydf <- data.frame(visitId = c("a", "b", "c"),
                     icd9 = c("441", "412.93", "044.9"))
  cmb <- icd_comorbid_quan_deyo.icd9(mydf, short_code = FALSE, hierarchy = TRUE)
  expect_false("names" %in% names(attributes(cmb)))
  icd_charlson(mydf, isShort = FALSE) # TODO: fix S3 classes ehre
  icd_charlson(mydf, isShort = FALSE, return.df = TRUE)
  icd_charlson_from_comorbid(cmb)
})

test_that("disordered visitIds works by default", {
  set.seed(1441)
  dat <- transform(testTwenty, visitId = sample(visitId))
  tres <- icd_comorbid.icd9(dat, icd::icd9_map_ahrq, icd9Field = "icd9Code")
  cres <- icd_comorbid.icd9(testTwenty, icd::icd9_map_ahrq, icd9Field = "icd9Code")
  expect_equal(dim(tres), dim(cres))
  expect_equal(sum(tres), sum(cres))
  expect_true(setequal(rownames(tres), rownames(cres)))
  expect_equal(colnames(tres), colnames(cres))

})

test_that("comorbidities created from source data frame coded as factors", {
  v2 <- icd_wide_to_long(vermont_dx) # TODO: correct S3 method?
  v2$visit_id <- as.factor(v2$visit_id)
  v2$icdCode <- as.factor(v2$icdCode)

  res <- icd_comorbid_ahrq.icd9(v2)
  res_nofactor <- vermont_dx %>% icd_wide_to_long %>% icd_comorbid_ahrq.icd9
  expect_identical(res, res_nofactor)
})

test_that("all AHRQ ICD-9 comorbidities are also in the ICD-10 maps, in same order", {
  # TODO: similar for elix, deyo etc.
  expect_equal_no_icd(names(icd9_map_ahrq), names(icd10_map_ahrq))
})
