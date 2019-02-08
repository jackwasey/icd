context("util")

test_that("trim with empty inputs", {
  expect_equal(trim(character(0)), character(0))
  expect_equal(trim(""), "")
})

test_that("trim with acceptable inputs", {
  expect_equal(trim(NA_character_), NA_character_)
  # not necesarily desirable, but this states the expectation.
  expect_equal(trim(NA_integer_), NA_character_)
  expect_equal(trim("jack"), "jack")
  expect_equal(trim("jack "), "jack")
  expect_equal(trim(" jack "), "jack")
  expect_equal(trim(" jack"), "jack")
  expect_equal(trim("jack\t"), "jack")
  expect_equal(trim("\tjack\t"), "jack")
  expect_equal(trim("\tjack"), "jack")
  expect_equal(trim("ja ck"), "ja ck")
  expect_equal(trim("ja ck "), "ja ck")
  expect_equal(trim(" ja ck "), "ja ck")
  expect_equal(trim(" ja ck"), "ja ck")
  expect_equal(trim(c(" ja ck\t", "wasey ")),
               c("ja ck", "wasey"))
  # trim and trimCpp should do the same thing, for non-NA
  expect_identical(trim(c("123", " a s d f ")),
                   trimCpp(c("123", " a s d f ")))
  # strim (trim a single string)
  expect_equal(strim(NA_character_), NA_character_)
  expect_equal(strim("jack"), "jack")
  expect_equal(strim("jack "), "jack")
  expect_equal(strim(" jack "), "jack")
  expect_equal(strim(" jack"), "jack")
  expect_equal(strim("jack\t"), "jack")
  expect_equal(strim("\tjack\t"), "jack")
  expect_equal(strim("\tjack"), "jack")
  expect_equal(strim("ja ck"), "ja ck")
  expect_equal(strim("ja ck "), "ja ck")
  expect_equal(strim(" ja ck "), "ja ck")
  expect_equal(strim(" ja ck"), "ja ck")
  # is this what I want?
  expect_error(strim(c("jack", " an other ")))
  # strim and strimCpp should do the same thing, for non-NA input
  expect_identical(strim(" a s d f "), strimCpp(" a s d f "))
})

test_that("get visit name from a matrix should fail", {
  expect_error(get_visit_name(matrix(c(1, 2, 3, 4), nrow = 2)))
})

test_that("get visit name works for this failure case", {
  nms <- c("id", "newborn", "age", "dx01", "dx02", "dx03", "dx04", "dx05",
           "dx06", "dx07", "dx08", "dx09", "dx10", "dx11", "dx12", "dx13",
           "dx14", "dx15", "pc01", "pc02", "pc03", "pc04", "pc05", "pc06",
           "pc07", "pc08", "dx_adm", "emergency")
  df <- as.data.frame(matrix(nrow = 2, ncol = length(nms)))
  names(df) <- nms
  expect_equal(get_visit_name(df), "id")
})

test_that("logical to binary for a matrix works", {
  expect_identical(logical_to_binary(matrix(c(T, F, T, F), nrow = 2)),
                   matrix(c(1L, 0L, 1L, 0L), nrow = 2))
})

context("heuristics for determining the field names of an input data frame")

test_that("well structured data frame, ICD & visit guessed", {
  expect_equal(get_visit_name(simple_poa_pts), "visit_id")
  expect_equal(get_icd_name(simple_poa_pts), "code")
  expect_equal(get_visit_name(complex_poa_pts), "visit_id")
  expect_equal(get_icd_name(complex_poa_pts), "icd9")
  expect_equal(get_visit_name(test_twenty), "visit_id")
  expect_equal(get_icd_name(test_twenty), "icd9Code")
})

test_that("ambiguous icd_name gives warning", {
  skip("we now accept wide data, but we could still warn if there is a mix")
  two_code_pt <-  data.frame(
    visit_id = "V111111",
    icd9 = "441",
    icd10 = "A11",
    poa = "N"
  )
  expect_warning(get_icd_name(two_code_pt))
})

test_that("icd name heuristic works for various", {
  guesses <- c("icd.?(9|10)", "icd.?(9|10).?Code", "icd",
               "diagnos", "diag.?code", "diag", "i(9|10)", "code")
  x <- simple_pts
  for (n in c("icd-10", "icd9", "diag", "diag1",
              "DIAGCODE", "i9code", "code", "diagnosis")) {
    names(x)[2] <- n
    expect_equal(get_icd_name(x), n)
  }
})

test_that("error if an unnamed data frame is used to guess field", {
  x <- unname(simple_pts)
  expect_error(get_visit_name(x))
  expect_error(get_icd_name(x))
})

test_that("icd9 field guessed from data if name fails, any order", {
  x <- simple_poa_pts
  x10 <- x
  x10[[2]] <- icd10_each_ahrq_cmb[1:4]
  names(x)[2] <- "not_helpful"
  names(x10)[2] <- "not_helpful"
  expect_equal(get_icd_name(x), "not_helpful")
  perms <- matrix(ncol = 3, byrow = TRUE, data = c(
    1, 2, 3,
    1, 3, 2,
    2, 1, 3,
    2, 3, 1,
    3, 1, 2,
    3, 2, 1)
  )
  for (i in nrow(perms)) {
    expect_equal(get_icd_name(x[perms[i, ]]), "not_helpful",
                 info = paste(perms[i, ], collapse = ", "))
    expect_equal(get_icd_name(x10[perms[i, ]]), "not_helpful",
                 info = paste(perms[i, ], collapse = ", "))
  }
})

test_that("get_icd_name: icd field not present at all", {
  x <- simple_pts
  x[2] <- NULL
  expect_equal(get_icd_name(x), names(simple_pts)[1])
  names(x)[2] <- "icd"
  expect_error(get_icd_name(x))
  x[[2]] <- as.icd9(x[[2]])
  expect_error(get_icd_name(x))
})
