context("comorbidities")

test_that("ahrq make sure all the children
            are listed in the saved data.", {
              for (i in ahrqComorbid) expect_equal(icd9ChildrenShort(i, onlyReal = FALSE), sort(i))
            })

test_that("Elixhauser make sure all the children
            are listed in the saved data.", {
              for (i in elixComorbid) expect_equal(icd9ChildrenShort(i, onlyReal = FALSE), sort(i))
            })

test_that("Quan Charlson make sure all the children
            are listed in the saved data.", {
              for (i in quanDeyoComorbid) expect_equal(icd9ChildrenShort(i, onlyReal = FALSE), sort(i))
            })

test_that("Quan Elixhauser make sure all the children
            are listed in the saved data.", {
              for (i in quanElixComorbid) expect_equal(icd9ChildrenShort(i, onlyReal = FALSE), sort(i))
            })

test_that("icd9 comorbidities are created correctly,
            and logical to binary conversion ok", {
              ptdf <- icd9Comorbid(icd9df = patientData, isShort = TRUE,
                                   icd9Mapping = ahrqComorbid,
                                   visitId = "visitId")

              expect_equal(names(ptdf), c("visitId", names(ahrqComorbid)))

              expect_true(all(sapply(names(ahrqComorbid),
                                     function(x)
                                       class(ptdf[, x])) == "logical"))
              ptdflogical <- logicalToBinary(ptdf)
              expect_true(all(sapply(names(ahrqComorbid),
                                     function(x)
                                       class(ptdflogical[, x])) == "integer"))
              # do not expect all the rest of patient data to be returned - we
              # aren't responsible for aggregating other fields by visitId!
              expect_equal(dim(ptdf),
                           c(length(unique(patientData[["visitId"]])),
                             1 + length(ahrqComorbid)))
              expect_true(
                all(names(ptdf) %in% c("visitId", names(ahrqComorbid))))
              expect_true(
                all(names(ptdflogical) %in% c("visitId", names(ahrqComorbid))))

              expect_equal(
                logicalToBinary(data.frame(a = c("jack", "hayley"),
                                           b = c(TRUE, FALSE),
                                           f = c(TRUE, TRUE))),
                data.frame(a = c("jack", "hayley"),
                           b = c(1, 0),
                           f = c(1, 1))
              )
            })

test_that("ahrq icd9 mappings generated from the current generation code", {
  # same but from source data. Should be absolutely identical.
  expect_equal(ahrqComorbid,
               parseAhrqSas(condense = FALSE,
                            save = FALSE, returnAll = FALSE))
  # same but from source data. Should be absolutely identical.
  expect_identical(ahrqComorbidAll,
                   parseAhrqSas(condense = FALSE,
                                save = FALSE, returnAll = TRUE))
  expect_equivalent(icd9GetInvalidMappingShort(ahrqComorbid), list())
})
test_that("Quan Charlson icd9 mappings are all
            generated from the current generation code", {
              expect_identical(quanDeyoComorbid,
                               parseQuanDeyoSas(condense = FALSE, save = FALSE))
              expect_equivalent(icd9GetInvalidMappingShort(quanDeyoComorbid), list())
            })
test_that("Quan Elixhauser icd9 mappings are all
            generated from the current generation code", {
              expect_identical(quanElixComorbid,
                               parseQuanElix(condense = FALSE, save = FALSE))
              expect_equivalent(icd9GetInvalidMappingShort(quanElixComorbid), list())
            })
test_that("Elixhauser icd9 mappings are all
            generated from the current generation code", {

              expect_identical(elixComorbid,
                               parseElix(condense = FALSE, save = FALSE))
              expect_equivalent(icd9GetInvalidMappingShort(elixComorbid), list())
            })

test_that("ahrq comorbidity mapping is applied correctly,
            all comorbidities in one patient, no abbrev, hier", {
              res <- icd9ComorbidAhrq(ahrqTestDat, isShort = TRUE,
                                      abbrevNames = FALSE,
                                      applyHierarchy = TRUE)
              expect_equal(dim(res), c(1,30))
              expect_true(all(ahrqComorbidNames %in% names(res)))
              # should not have dm and dmcx, etc
              expect_false(all(as.logical(res[1, unlist(ahrqComorbidNames)])))
              expect_false(res[1, "Diabetes, uncomplicated"])
              expect_false(res[1, "Solid tumor without metastasis"])
            })

test_that("elix comorbidity mapping is applied correctly,
            all comorbidities in one patient, no abbrev, hier", {
              res <- icd9ComorbidElix(elixTestDat, isShort = TRUE,
                                      abbrevNames = FALSE,
                                      applyHierarchy = TRUE)
              expect_equal(dim(res), c(1,31))
              expect_true(all(elixComorbidNames %in% names(res)))
              # should not have dm and dmcx, etc
              expect_false(all(as.logical(res[1, unlist(elixComorbidNames)])))
              expect_false(res[1, "Diabetes, uncomplicated"])
              expect_false(res[1, "Solid tumor without metastasis"])
            })

test_that("elix comorbidity mapping is applied correctly,
            all comorbidities in one patient, abbrev, hier", {
              res <- icd9ComorbidElix(elixTestDat,
                                      isShort = TRUE,
                                      abbrevNames = TRUE,
                                      applyHierarchy = TRUE)
              expect_equal(dim(res), c(1,31))
              expect_true(all(elixComorbidNamesAbbrev %in% names(res)))
              # should not have dm and dmcx, etc
              expect_false(
                all(as.logical(res[1, unlist(elixComorbidNamesAbbrev)])))
              expect_false(res[1, "DM"])
              expect_false(res[1, "Tumor"])
            })

test_that("elix comorbidity mapping is applied correctly,
            all comorbidities in one patient, no abbrev, no hier", {
              res <- icd9ComorbidElix(elixTestDat, isShort = TRUE,
                                      abbrevNames = FALSE,
                                      applyHierarchy = FALSE)
              expect_equal(dim(res), c(1,32)) #longer because 2x htn
              expect_true(all(elixComorbidNamesHtn %in% names(res)))
              # not applying hierarchy, so dm and dmcx can both be true
              expect_true(all(as.logical(res[1, unlist(elixComorbidNamesHtn)])))
            })

test_that("elix comorbidity mapping is applied correctly,
            all comorbidities in one patient, abbrev, no hier", {
              res <- icd9ComorbidElix(elixTestDat, isShort = TRUE,
                                      abbrevNames = TRUE,
                                      applyHierarchy = FALSE)
              expect_equal(dim(res), c(1,32))
              expect_true(all(elixComorbidNamesHtnAbbrev %in% names(res)))
              expect_true(
                all(as.logical(res[1, unlist(elixComorbidNamesHtnAbbrev)])))

            })

test_that("qelix comorbidity mapping is applied correctly,
            all comorbidities in one patient, no abbrev, hier", {
              res <- icd9ComorbidQuanElix(quanElixTestDat,
                                          isShort = TRUE,
                                          abbrevNames = FALSE,
                                          applyHierarchy = TRUE)
              expect_equal(dim(res), c(1,31))
              expect_true(all(quanElixComorbidNames %in% names(res)))
              # should not have dm and dmcx, etc
              expect_false(
                all(as.logical(res[1, unlist(quanElixComorbidNames)])))
              expect_false(res[1, "Diabetes, uncomplicated"])
              expect_false(res[1, "Solid tumor without metastasis"])
            })

test_that("qelix comorbidity mapping is applied correctly,
            all comorbidities in one patient, abbrev, hier", {
              res <- icd9ComorbidQuanElix(quanElixTestDat,
                                          isShort = TRUE,
                                          abbrevNames = TRUE,
                                          applyHierarchy = TRUE)
              expect_equal(dim(res), c(1,31))
              expect_true(all(quanElixComorbidNamesAbbrev %in% names(res)))
              # should not have dm and dmcx, etc
              expect_false(
                all(as.logical(res[1, unlist(quanElixComorbidNamesAbbrev)])))
              expect_false(res[1, "DM"])
              expect_false(res[1, "Tumor"])
            })

test_that("qelix comorbidity mapping is applied correctly,
            all comorbidities in one patient, no abbrev, no hier", {
              res <- icd9ComorbidQuanElix(quanElixTestDat,
                                          isShort = TRUE,
                                          abbrevNames = FALSE,
                                          applyHierarchy = FALSE)
              #longer because 2x htn
              expect_equal(dim(res), c(1,32))
              # not applying hierarchy, so dm and dmcx can both be true
              expect_true(all(quanElixComorbidNamesHtn %in% names(res)))
              expect_true(
                all(as.logical(res[1, unlist(quanElixComorbidNamesHtn)])))
            })

test_that("qelix comorbidity mapping is applied correctly,
            all comorbidities in one patient, abbrev, no hier", {
              res <- icd9ComorbidQuanElix(quanElixTestDat,
                                          isShort = TRUE,
                                          abbrevNames = TRUE,
                                          applyHierarchy = FALSE)
              expect_equal(dim(res), c(1,32))
              expect_true(all(quanElixComorbidNamesHtnAbbrev %in% names(res)))
              expect_true(
                all(as.logical(res[1, unlist(quanElixComorbidNamesHtnAbbrev)])))

            })

test_that("can condense the big lists of comorbidities without errors", {
  # this is a useful test because the data weren't generated by just expanding
  # base ranges (which is how the condense works in reverse)
  for (onlyReal in c(TRUE, FALSE)) {
    ahrq <- lapply(ahrqComorbid, icd9CondenseToMajorShort, onlyReal)
    quanDeyo <- lapply(quanDeyoComorbid, icd9CondenseToMajorShort, onlyReal)
    quanElix <- lapply(quanElixComorbid,
                       icd9CondenseToMajorShort, onlyReal)
    elix <- lapply(elixComorbid, icd9CondenseToMajorShort, onlyReal)
    expect_is(ahrq, class = "list")
    expect_is(elix, class = "list")
    expect_is(quanDeyo, class = "list")
    expect_is(quanElix, class = "list")
    # the comorbidity mappings save in \code{data} should not be condensed.
    expect_that(ahrq,
                testthat::not(testthat::equals(ahrqComorbid)))
    expect_that(elix,
                testthat::not(testthat::equals(elixComorbid)))
    expect_that(quanDeyo,
                testthat::not(testthat::equals(quanDeyoComorbid)))
    expect_that(quanElix,
                testthat::not(testthat::equals(quanElixComorbid)))
  }
})


test_that("get Charlson/Deyo comorbidities for a single patient", {
  mydf <- data.frame(visitId = c("a"),
                     icd9 = c("044.9"),
                     stringsAsFactors = FALSE)
  expect_equal(
    icd9ComorbidQuanDeyo(icd9df = mydf, isShort = FALSE),
    structure(
      list(
        visitId = "a",
        MI = FALSE, CHF = FALSE, PVD = FALSE, Stroke = FALSE, Dementia = FALSE,
        Pulmonary = FALSE, Rheumatic = FALSE, PUD = FALSE, LiverMild = FALSE,
        DM = FALSE, DMcx = FALSE, Paralysis = FALSE, Renal = FALSE,
        Cancer = FALSE, LiverSevere = FALSE, Mets = FALSE, HIV = TRUE),
      .Names = c("visitId",
                 "MI", "CHF", "PVD", "Stroke", "Dementia", "Pulmonary",
                 "Rheumatic", "PUD", "LiverMild", "DM", "DMcx", "Paralysis",
                 "Renal", "Cancer", "LiverSevere", "Mets", "HIV"),
      row.names = 1L,
      class = "data.frame")
  )

  mydf <- data.frame(visitId = c("a", "a"),
                     icd9 = c("044.9", "044.9"),
                     stringsAsFactors = FALSE)
  expect_that(icd9ComorbidQuanDeyo(mydf, isShort = FALSE),
              testthat::not(throws_error()))

  mydf <- data.frame(visitId = c("a", "a"), icd9 = c("441", "412.93"))
  expect_that(icd9ComorbidQuanDeyo(mydf, isShort = FALSE),
              testthat::not(throws_error()))

})

test_that("icd9Hierarchy as saved in data can be recreated", {
  skip("this is a very slow test - 10 mins for one assertion")
  expect_equal(icd9GetChaptersHierarchy(save = FALSE),
               icd9::icd9Hierarchy)
})

# the following test is dependent on availability and consistency of
# http://www.icd9data.com because there is no machine readable CDC or CMS file
# with this data.
test_that("icd9Chapters, etc. as saved in data can be recreated", {
  res <- parseIcd9Chapters(year = "2014", save = FALSE)
  expect_equal(res$icd9Chapters, icd9::icd9Chapters)
  expect_equal(res$icd9ChaptersSub, icd9::icd9ChaptersSub)
  expect_equal(res$icd9ChaptersMajor, icd9::icd9ChaptersMajor)
})


test_that("ahrq comorbidity mapping is applied correctly,
            all comorbidities in one patient, abbrev, hier", {
              res <- icd9ComorbidAhrq(ahrqTestDat, isShort = TRUE,
                                      abbrevNames = TRUE,
                                      applyHierarchy = TRUE)
              expect_equal(dim(res), c(1,30))
              expect_true(all(ahrqComorbidNamesAbbrev %in% names(res)))
              # should not have dm and dmcx, etc
              expect_false(
                all(as.logical(res[1, unlist(ahrqComorbidNamesAbbrev)])))
              expect_false(res[1, "DM"])
              expect_false(res[1, "Tumor"])
            })

test_that("ahrq comorbidity mapping is applied correctly,
          all comorbidities in one patient, no abbrev, no hier", {
            res <- icd9ComorbidAhrq(ahrqTestDat, isShort = TRUE,
                                    abbrevNames = FALSE,
                                    applyHierarchy = FALSE)
            #longer because 2x htn
            expect_equal(dim(res), c(1, 31))
            # not applying hierarchy, so dm and dmcx can both be true
            expect_true(all(ahrqComorbidNamesHtn %in% names(res)))
            expect_true(all(as.logical(res[1, unlist(ahrqComorbidNamesHtn)])))
          })

test_that("ahrq comorbidity mapping is applied correctly,
          all comorbidities in one patient, abbrev, no hier", {
            res <- icd9ComorbidAhrq(ahrqTestDat, isShort = TRUE,
                                    abbrevNames = TRUE,
                                    applyHierarchy = FALSE)
            expect_equal(dim(res), c(1,31))
            expect_true(all(ahrqComorbidNamesHtnAbbrev %in% names(res)))
            expect_true(
              all(as.logical(res[1, unlist(ahrqComorbidNamesHtnAbbrev)])))

          })



test_that("condense an ICD-9 code set to minimal group", {
  skip("TODO:  this test breaks because %i9s% now includes the last major, even if not all its child.")
  expect_equal(sort(icd9CondenseToMajorShort("98799" %i9s% "98901",
                                        onlyReal = FALSE)),
               sort(c("98799", "988", "98900", "98901")))
  expect_equal(sort(icd9CondenseToMajorShort("98799" %i9s% "98901",
                                        onlyReal = TRUE,
                                        dropNonReal = TRUE)),
               "988")
  expect_equal(
    sort(icd9CondenseToMajorShort("98799" %i9s% "98901",
                             onlyReal = TRUE,
                             dropNonReal = FALSE)),
    sort(
      # this list is the full range with these missing:
      #  c("9880", "9881", "9882", "9888", "9889")

      c("988", "98799", "988", "98800", "98801", "98802", "98803",
        "98804", "98805", "98806", "98807", "98808", "98809", "98810",
        "98811", "98812", "98813", "98814", "98815", "98816", "98817",
        "98818", "98819", "98820", "98821", "98822", "98823", "98824",
        "98825", "98826", "98827", "98828", "98829", "9883", "98830",
        "98831", "98832", "98833", "98834", "98835", "98836", "98837",
        "98838", "98839", "9884", "98840", "98841", "98842", "98843",
        "98844", "98845", "98846", "98847", "98848", "98849", "9885",
        "98850", "98851", "98852", "98853", "98854", "98855", "98856",
        "98857", "98858", "98859", "9886", "98860", "98861", "98862",
        "98863", "98864", "98865", "98866", "98867", "98868", "98869",
        "9887", "98870", "98871", "98872", "98873", "98874", "98875",
        "98876", "98877", "98878", "98879", "98880", "98881", "98882",
        "98883", "98884", "98885", "98886", "98887", "98888", "98889",
        "98890", "98891", "98892", "98893", "98894", "98895", "98896",
        "98897", "98898", "98899", "98900", "98901")
    ))
  # TODO: more tests
})


# test_that("AHRQ interpretation at least returns something reasonable", {
#   result <- parseAhrqSas(sasPath = system.file("extdata",
#     "comformat2012-2013.txt", package="icd9"), save = FALSE)
#   expect_that(result, is_a("list"))
#   expect_true(length(result) > 10)
# })

test_that("HTN subgroups all worked", {
  # pick one subcategory
  expect_true(all(ahrqComorbidAll$HTNPREG %in% ahrqComorbid[["HTNcx"]]))

  # and we didn't drop any:
  expect_true(all(ahrqComorbidAll$HTNCX %in% ahrqComorbid[["HTNcx"]]))
  expect_true(all(ahrqComorbidAll$CHF %in% ahrqComorbid[["CHF"]]))
  expect_true(all(ahrqComorbidAll$RENLFAIL %in% ahrqComorbid[["Renal"]]))

})

test_that("Charlson Deyo doesn't double count disease with two severities", {
  expect_false(any(quanDeyoComorbid[["Mild Liver Disease"]] %in%
                     quanDeyoComorbid[["Moderate or Severe Liver Disease"]] ))
  expect_false(any(quanDeyoComorbid[["Cancer"]] %in%
                     quanDeyoComorbid[["Metastatic Carcinoma"]] ))
  expect_false(any(quanDeyoComorbid[["Diabetes without complications"]] %in%
                     quanDeyoComorbid[["Diabetes with complications"]] ))
})

test_that("Elixhauser doesn't double count disease with multiple severities", {
  expect_false(any(quanElixComorbid[["dm.uncomp"]] %in%
                     quanElixComorbid[["dm.comp"]] ))
  expect_false(any(quanElixComorbid[["solid.tumor"]] %in%
                     quanElixComorbid[["mets"]] ))
  expect_false(any(elixComorbid[["dm.uncomp"]] %in%
                     elixComorbid[["dm.comp"]] ))
  expect_false(any(elixComorbid[["solid.tumor"]] %in%
                     elixComorbid[["mets"]] ))
  expect_false(any(ahrqComorbid[["DM"]] %in% ahrqComorbid[["DMCX"]] ))
  expect_false(any(ahrqComorbid[["TUMOR"]] %in% ahrqComorbid[["METS"]] ))
})


test_that("filter POA - not a data frame", {
  expect_error(icd9FilterPoaNo(list(pollo = "loco")))
  expect_error(icd9FilterPoaNotYes(visitId=c("1","2"),
                                   icd9 = c("1","2"),
                                   poa = c("Y","N")))
})

test_that("filter POA - no poa field", {
  expect_error(icd9FilterPoaYes(simplePoaPatients[1:2]))
})

test_that("filter POA - generic func - invalid poa type", {
  expect_error(icd9FilterPoa(icd9df = simplePoaPatients,
                             poaField = "poa", poa = "not an option"))
  expect_error(icd9FilterPoa(icd9df = simplePoaPatients,
                             poaField = "poa", poa = ""))
  expect_error(icd9FilterPoa(icd9df = simplePoaPatients,
                             poaField = "poa", poa = NA))
})

test_that("filter POA - wrong name poa field", {
  pd <- simplePoaPatients
  names(pd) <- c("visitId", "icd9", "achilleus")
  expect_error(icd9FilterPoaYes(pd, poaField = "poa"))
  expect_error(icd9FilterPoaYes(pd, poaField = "odysseus"))
  expect_error(icd9FilterPoaYes(pd))
})

test_that("filter POA - poa is factor", {
  # POA flag is an obvious case for using factors. Not sure if it saves much
  # memory, and it certainly risks screwing up the analysis with obscure and
  # difficult to debug errors. ICD-9 code is also factor fodder, and likely to
  # be highly repeated over millions of patients, but I've resisted its charms
  # thus far.

  # just within this closure
  simplePoaPatients$poa <- factor(simplePoaPatients$poa)
  names(simplePoaPatients)[3] <- "poa"
  # just within this closure
  complexPoaPatients$poa <- factor(complexPoaPatients$poa)
  names(complexPoaPatients)[3] <- "poa"

  # row names are preserved here: probably not important, but a little annoying
  expect_identical(icd9FilterPoaYes(simplePoaPatients),
                   simplePoaPatients[1, 1:2])
  expect_identical(icd9FilterPoaNotYes(simplePoaPatients),
                   simplePoaPatients[-1, 1:2])
  expect_identical(icd9FilterPoaNo(simplePoaPatients),
                   simplePoaPatients[2, 1:2])
  expect_identical(icd9FilterPoaNotNo(simplePoaPatients),
                   simplePoaPatients[-2, 1:2])

  expect_identical(icd9FilterPoaYes(complexPoaPatients),
                   complexPoaPatients[1, 1:2])
  expect_identical(icd9FilterPoaNotYes(complexPoaPatients),
                   complexPoaPatients[-1, 1:2])
  expect_identical(icd9FilterPoaNo(complexPoaPatients),
                   complexPoaPatients[2, 1:2])
  expect_identical(icd9FilterPoaNotNo(complexPoaPatients),
                   complexPoaPatients[-2, 1:2])
})

test_that("filter POA - poa is vector", {
  expect_identical(icd9FilterPoaYes(simplePoaPatients),
                   simplePoaPatients[1, 1:2])
  expect_identical(icd9FilterPoaNotYes(simplePoaPatients),
                   simplePoaPatients[-1, 1:2])
  expect_identical(icd9FilterPoaNo(simplePoaPatients),
                   simplePoaPatients[2, 1:2])
  expect_identical(icd9FilterPoaNotNo(simplePoaPatients),
                   simplePoaPatients[-2, 1:2])

  expect_identical(icd9FilterPoaYes(complexPoaPatients),
                   complexPoaPatients[1, 1:2])
  expect_identical(icd9FilterPoaNotYes(complexPoaPatients),
                   complexPoaPatients[-1, 1:2])
  expect_identical(icd9FilterPoaNo(complexPoaPatients),
                   complexPoaPatients[2, 1:2])
  expect_identical(icd9FilterPoaNotNo(complexPoaPatients),
                   complexPoaPatients[-2, 1:2])

})

test_that("filter POA - poa upper and lower case", {
  smpl <- simplePoaPatients
  smpl[["poa"]] <- c("Y", "n", "e", NA)
  expect_identical(icd9FilterPoaNo(smpl), icd9FilterPoaNo(simplePoaPatients))
})

test_that("filter POA - just Y and N should be complementary", {
  # take any data frame to start out:
  dfrm <- testTwenty;
  dfrm <- dfrm[dfrm[["poa"]] %in% c("Y", "N", "y", "n"),]
  expect_identical(icd9FilterPoaNo(dfrm),  icd9FilterPoaNotYes(dfrm))
  expect_identical(icd9FilterPoaYes(dfrm), icd9FilterPoaNotNo(dfrm))
})


test_that("ICD-9 codes from the original sources do appear in my data", {
  # these tests demonstrate that the interpreted data is correctly transcribed
  # in cases where the data is structured differently, and also affirms that
  # 'child' codes are included in the RData mappings in the package. E.g. if the
  # mapping specifies "044", we do expect 111 total codes to be in the mapping
  # 0440 04400 04401 etc. Ahrq
  expect_true("3337" %in% ahrqComorbid$NeuroOther) # single value
  expect_true("33370" %in% ahrqComorbid$NeuroOther) # single value sub-code
  expect_true("494" %in% ahrqComorbid$Pulmonary) # top-level at start of range
  expect_true("4940" %in% ahrqComorbid$Pulmonary) # value within range
  expect_true("49400" %in% ahrqComorbid$Pulmonary) # sub-value within range

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

test_that("sample of ICD-9 codes from manually specified mappings do appear", {
  # the following tests cover the mappings in which there was no source SAS
  # data, but the numbers were transcribed manually. This is therefore testing a
  # little of the transcription, and also the elobration of codes definied in
  # ranges
  expect_true("2500" %in% quanElixComorbid$DM)
  expect_true("2501" %in% quanElixComorbid$DM)
  expect_true("25011" %in% quanElixComorbid$DM)
  expect_true("276" %in% quanElixComorbid$FluidsLytes)
  expect_true("2761" %in% quanElixComorbid$FluidsLytes)
  expect_true("27612" %in% quanElixComorbid$FluidsLytes)
  # top level should not be included automatically
  expect_false("710" %in% quanElixComorbid$FluidsLytes)
  expect_true("09320" %in% elixComorbid$Valvular)
  expect_true("3971" %in% elixComorbid$Valvular)
  expect_true("V560" %in% elixComorbid$Renal)
  expect_true("V1090" %in% elixComorbid$Tumor) # child at end of a V range
})

test_that("github #34 - short and long custom map give different results", {
  mydf <- data.frame(visitId = c("a","b","b","c"),
                     icd9 = c("1","010","10","20"))

  mymaps <- list(jack = c("1", "2", "3"), alf = c("010", "20"))
  mymapd <- lapply(mymaps, icd9ShortToDecimal)

  expect_identical(
    icd9Comorbid(mydf, icd9Mapping = mymaps, isShort = TRUE),
    icd9Comorbid(mydf, icd9Mapping = mymapd, isShort = FALSE))

})

test_that("no NA values in the co-morbidity lists", {
  expect_false(any(is.na(unlist(unname(ahrqComorbid)))))
  expect_false(any(is.na(unlist(unname(ahrqComorbidAll)))))
  expect_false(any(is.na(unlist(unname(quanDeyoComorbid)))))
  expect_false(any(is.na(unlist(unname(quanElixComorbid)))))
  expect_false(any(is.na(unlist(unname(elixComorbid)))))
})

test_that("built-in icd9 to comorbidity mappings are all valid", {
  expect_true(icd9ValidMappingShort(ahrqComorbid))
  expect_true(icd9ValidMappingShort(quanDeyoComorbid))
  expect_true(icd9ValidMappingShort(quanElixComorbid))
  expect_true(icd9ValidMappingShort(elixComorbid))
})
