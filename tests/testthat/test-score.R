context("Charlson and comorbidity counting")

test_that("github issue #44 from wmurphyrd", {
  mydf <- data.frame(visit_id = c("a", "b", "c", "a", "b", "d"),
                     icd9 = c("441", "412.93", "042", "250.0", "250.0", "250.0"),
                     stringsAsFactors = TRUE)
  expect_warning(res <- charlson(mydf, return_df = TRUE), NA)
  expect_equal(res$Charlson, c(2, 2, 6, 1))
})

test_that("github issue #46 from wmurphyd", {
  mydf <- data.frame(visit_id = "a", icd9 = "250.0")
  comorbids <- icd9_comorbid_quan_deyo(mydf, short_code = FALSE, return_df = TRUE)
  set.seed(123)
  # Fill a QuanDeyo comorbidity data frame with random data
  use_ncol_cmb <- ncol(comorbids) - 1
  comorbids <- rbind(comorbids, data.frame(
    visit_id = letters[2:10],
    matrix(runif(use_ncol_cmb * 9) > 0.7,
           ncol = 17, dimnames = list(character(0), names(comorbids[2:18]))
    )
  )
  )
  c2.inv <- cbind(t(comorbids[2, 2:18]),
                  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 6, 6))
  expect_equivalent(
    charlson_from_comorbid(comorbids, hierarchy = TRUE)[2],
    sum(apply(c2.inv, 1, prod))
  )
})

test_that("only matrix or data.frame accepted", {
  expect_error(charlson(c(1, 2)))
  expect_error(charlson(c(1, 2), visit_id = "roam", return_df = TRUE,
                        stringsAsFactors = TRUE))
  expect_error(charlson(list(1, 2)))
  expect_error(charlson(list(1, 2), visit_id = "roam", return_df = TRUE,
                        stringsAsFactors = TRUE))
})

test_that("Charlson score", {
  mydf <- data.frame(visit_id = c("a", "b", "c"),
                     icd9 = c("441", "412.93", "042"),
                     stringsAsFactors = TRUE)
  expect_equal(
    charlson_from_comorbid(
      icd9_comorbid_quan_deyo(mydf, short_code = FALSE, hierarchy = TRUE, return_df = TRUE)
    ),
    charlson(mydf, short_code = FALSE, return_df = FALSE)
  )
  expect_equivalent(charlson(mydf,
                             return_df = TRUE,
                             stringsAsFactors = TRUE,
                             short_code = FALSE),
                    structure(list(visit_id = structure(1:3,
                                                        .Label = c("a", "b", "c"),
                                                        class = "factor"),
                                   Charlson = c(1, 1, 6)),
                              .Names = c("visit_id", "Charlson"),
                              row.names = c(NA, -3L),
                              class = "data.frame")
  )
  mydff <- data.frame(visit_id = c("a", "b", "c"),
                      icd9 = c("441", "412.93", "042"),
                      stringsAsFactors = FALSE)
  expect_identical(charlson(mydff,
                            return_df = TRUE,
                            stringsAsFactors = FALSE,
                            short_code = FALSE),
                   structure(list(visit_id = c("a", "b", "c"),
                                  Charlson = c(1, 1, 6)),
                             .Names = c("visit_id", "Charlson"),
                             row.names = c(NA, -3L),
                             class = "data.frame")
  )
  expect_identical(charlson(mydff,
                            return_df = TRUE,
                            stringsAsFactors = TRUE,
                            short_code = FALSE),
                   structure(list(visit_id = factor(c("a", "b", "c")),
                                  Charlson = c(1, 1, 6)),
                             .Names = c("visit_id", "Charlson"),
                             row.names = c(NA, -3L),
                             class = "data.frame")
  )
  mydfff <- mydff
  names(mydfff)[1] <- "v"
  expect_equivalent(charlson(mydfff,
                             return_df = TRUE,
                             stringsAsFactors = FALSE,
                             short_code = FALSE),
                    structure(list(v = c("a", "b", "c"),
                                   Charlson = c(1, 1, 6)),
                              .Names = c("v", "Charlson"),
                              row.names = c(NA, -3L),
                              class = "data.frame")
  )
  mydffff <- cbind(mydfff, data.frame(v2 = mydfff$v, stringsAsFactors = FALSE))
  mydffff$v <- NULL
  expect_identical(get_icd_name(mydffff, NULL), "icd9")
  expect_equivalent(charlson(mydffff, visit_name = "v2",
                             return_df = TRUE,
                             stringsAsFactors = FALSE,
                             short_code = FALSE),
                    structure(list(v2 = c("a", "b", "c"),
                                   Charlson = c(1, 1, 6)),
                              .Names = c("v2", "Charlson"),
                              row.names = c(NA, -3L),
                              class = "data.frame")
  )
})

test_that("Charlson - errors?", {
  baddf <- data.frame(visit_id = c("d", "d"),
                      icd9 = c("2500", "25042"),
                      stringsAsFactors = TRUE)
  cmb <- icd9_comorbid_quan_deyo(baddf, hierarchy = FALSE, short_code = TRUE)
  expect_error(charlson_from_comorbid(cmb, hierarchy = FALSE))
  baddf <- data.frame(visit_id = c("d", "d"),
                      icd9 = c("57224", "57345"),
                      stringsAsFactors = TRUE)
  cmb <- icd9_comorbid_quan_deyo(baddf, hierarchy = FALSE, short_code = TRUE)
  expect_error(charlson_from_comorbid(cmb, hierarchy = FALSE))
})

test_that("count icd9 codes", {
  mydf <- data.frame(visit_name = c("r", "r", "s"),
                     icd9 = c("441", "412.93", "042"))
  expect_equal(count_codes(mydf, return_df = TRUE),
               data.frame(visit_name = c("r", "s"),
                          icd_count = c(2, 1))
  )
  expect_equal(count_codes(mydf), c(2, 1))
  cmb <- icd9_comorbid_quan_deyo(mydf, short_code = FALSE, return_df = TRUE)
  expect_equivalent(count_comorbid(cmb), count_codes(mydf))
  wide <- data.frame(visit_id = c("r", "s", "t"),
                     icd9_1 = c("0011", "441", "456"),
                     icd9_2 = c(NA, "442", NA),
                     icd9_3 = c(NA, NA, "510"))
  expect_equal(count_codes_wide(wide),
               c("r" = 1, "s" = 2, "t" = 2))
  widezero <- data.frame(visit_id = c("j"),
                         icd9_a = NA,
                         icd9_b = NA)
  expect_equal(count_codes_wide(widezero),
               c("j" = 0))
  widezero2 <- data.frame(visit_id = c("j"),
                          icd9_a = NA)
  expect_equal(count_codes_wide(widezero2),
               c("j" = 0))
  widezero3 <- data.frame(visit_id = c("j", "j"),
                          icd9_a = c(NA, NA))
  expect_equal(count_codes_wide(widezero3, aggr = TRUE),
               c("j" = 0))
  widezero4 <- data.frame(visit_id = c("j", "j"),
                          icd9_a = c(NA, NA),
                          icd9_b = c(NA, NA))
  expect_equal(count_codes_wide(widezero4, aggr = TRUE),
               c("j" = 0))
  widezero3b <- data.frame(visit_id = c("j", "j"),
                           icd9_a = c(NA, NA))
  expect_equal(count_codes_wide(widezero3b, aggr = FALSE),
               c("j" = 0, "j" = 0))
  widezero4b <- data.frame(visit_id = c("j", "j"),
                           icd9_a = c(NA, NA),
                           icd9_b = c(NA, NA))
  expect_equal(count_codes_wide(widezero4b, aggr = FALSE),
               c("j" = 0, "j" = 0))
  widezero5 <- data.frame(visit_id = c("j", "k"),
                          icd9_a = c(NA, NA))
  expect_equal(count_codes_wide(widezero5),
               c("j" = 0, "k" = 0))
  widezero6 <- data.frame(visit_id = c("j", "k"),
                          icd9_a = c(NA, NA),
                          icd9_b = c(NA, NA))
  expect_equal(count_codes_wide(widezero6),
               c("j" = 0, "k" = 0))
})

test_that("count wide directly (old func) same as reshape count", {
  widedf <- data.frame(visit_id = c("a", "b", "c"),
                       icd9_01 = c("441", "4424", "441"),
                       icd9_02 = c(NA, "443", NA))
  # we don't get names back for the vector for 'long'
  expect_equivalent(count_codes_wide(widedf),
                    count_codes(wide_to_long(widedf)))
})

test_that("van_walraven_from_comorbid score calculation", {
  comorbids <- icd9_comorbid_quan_elix(
    mydf <- data.frame(visit_id = "a", icd9 = "250.0"),
    return_df = TRUE)
  set.seed(123)
  # Fill a QuanElix comorbidity data frame with random data
  use_ncol_cmb <- ncol(comorbids) - 1
  comorbids <- rbind(
    comorbids,
    data.frame(visit_id = letters[2:10],
               matrix(stats::runif(use_ncol_cmb * 9) > 0.7,
                      ncol = use_ncol_cmb,
                      dimnames = list(character(0), names(comorbids[2:31])))))
  c2.inv <- cbind(t(comorbids[2, -1]),
                  c(7, 5, -1, 4, 2, 0, 7, 6, 3, 0, 0, 0, 5, 11, 0, 0,
                    9, 12, 4, 0, 3, -4, 6, 5, -2, -2, 0, -7, 0, -3))
  expect_equivalent(
    van_walraven_from_comorbid(comorbids, hierarchy = TRUE)[2],
    sum(apply(c2.inv, 1, prod))
  )
})

test_that("van_walraven comodbidity index and score", {
  mydf <- data.frame(
    id = factor(c(rep(1, 20), rep(2, 20), rep(3, 18))),
    value =
      c("324.1", "285.9", "599.70", "038.9", "278.00", "38.97",
        "V88.01", "112.0", "427.89", "790.4", "401.9", "53.51", "584.9",
        "415.12", "995.91", "996.69", "83.39", "V46.2", "V58.61", "276.69",
        "515", "V14.6", "784.0", "V85.1", "427.31", "V85.44", "300",
        "86.28", "569.81", "041.49", "486", "45.62", "V15.82", "496",
        "261", "280.9", "275.2", "96.59", "V49.86", "V10.42", "276.8",
        "710.4", "311", "041.12", "276.0", "790.92", "518.84", "552.21",
        "V85.41", "278.01", "V15.82", "96.72", "070.70", "285.29", "276.3",
        "V66.7", "272.4", "790.92"))
  expect_equivalent(
    van_walraven(mydf, visit_name = "id", icd_name = "value"),
    van_walraven_from_comorbid(
      icd9_comorbid_quan_elix(mydf, visit_name = "id", icd_name = "value")))
  expect_equivalent(
    van_walraven(mydf, visit_name = "id", icd_name = "value", return_df = TRUE),
    data.frame(id = factor(c(1, 2, 3)),
               vanWalraven = c(10, 12, -2)))
  expect_equal(
    van_walraven(mydf, icd_name = "value"),
    structure(c(10, 12, -2), names = c("1", "2", "3"))
  )
})

test_that("github issue #64 - quan revised charleson scores", {
  mydf <- data.frame(visit_id = "a", icd9 = "250.0")
  comorbids <- comorbid_quan_deyo(mydf, short_code = FALSE, return_df = TRUE)

  ## test against a known score for a single comorbidity

  # direct scoring
  expect_equivalent(
    charlson_from_comorbid(comorbids, scoring_system = "original"), 1)
  expect_equivalent(
    charlson_from_comorbid(comorbids, scoring_system = "charlson"), 1)
  expect_equivalent(
    charlson_from_comorbid(comorbids, scoring_system = "quan"), 0)

  # pass through from top level function
  expect_equivalent(charlson(mydf, scoring_system = "o"), 1)
  expect_equivalent(charlson(mydf, scoring_system = "c"), 1)
  expect_equivalent(charlson(mydf, scoring_system = "q"), 0)

  ## test against randomly generated comorbidities in various conditions
  set.seed(456)
  # Fill a QuanDeyo comorbidity data frame with random data
  use_ncol_cmb <- ncol(comorbids) - 1
  comorbids <- rbind(comorbids, data.frame(
    visit_id = letters[2:10],
    matrix(runif(use_ncol_cmb * 9) > 0.7,
           ncol = 17, dimnames = list(character(0),
                                      names(comorbids[2:18])))))
  comorbids[, "DM"] <- comorbids[, "DM"] & !comorbids[, "DMcx"]
  comorbids[, "LiverMild"] <-
    comorbids[, "LiverMild"] & !comorbids[, "LiverSevere"]
  comorbids[, "Cancer"] <- comorbids[, "Cancer"] & !comorbids[, "Mets"]

  original_weights <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 6, 6)
  quan_weights     <- c(0, 2, 0, 0, 2, 1, 1, 0, 2, 0, 1, 2, 1, 2, 4, 6, 4)

  #omitting scoring_system argument should use original scores
  expect_equivalent(
    charlson_from_comorbid(comorbids, hierarchy = TRUE)[2],
    sum(apply(cbind(t(comorbids[2, 2:18]), original_weights), 1, prod))
  )

  #specify original scores
  expect_equivalent(
    charlson_from_comorbid(
      comorbids, hierarchy = TRUE, scoring_system = "original")[3],
    sum(apply(cbind(t(comorbids[3, 2:18]), original_weights), 1, prod))
  )
  expect_equivalent(
    charlson_from_comorbid(
      comorbids, hierarchy = TRUE, scoring_system = "charlson")[3],
    sum(apply(cbind(t(comorbids[3, 2:18]), original_weights), 1, prod))
  )

  #specify quan scores
  expect_equivalent(
    charlson_from_comorbid(
      comorbids, hierarchy = TRUE, scoring_system = "quan")[4],
    sum(apply(cbind(t(comorbids[4, 2:18]), quan_weights), 1, prod))
  )

  #partial matching of scoring_system argument
  expect_equivalent(
    charlson_from_comorbid(
      comorbids, hierarchy = TRUE, scoring_system = "o")[5],
    sum(apply(cbind(t(comorbids[5, 2:18]), original_weights), 1, prod))
  )
  expect_equivalent(
    charlson_from_comorbid(
      comorbids, hierarchy = TRUE, scoring_system = "q")[6],
    sum(apply(cbind(t(comorbids[6, 2:18]), quan_weights), 1, prod))
  )

  #invalid scoring_system argument
  expect_error(charlson_from_comorbid(comorbids,
                                      hierarchy = TRUE, scoring_system = "z"))

})
