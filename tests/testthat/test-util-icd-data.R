context("util")
test_that("string pair match extraction", {
  expect_equal(
    .str_pair_match(pattern = "(a*)b(c*)", string = "abc"), c(a = "c")
  )
  expect_equal(
    .str_pair_match(
      pattern = "([^mackarel]*)(spain)",
      string = "togospain"
    ),
    c(togo = "spain")
  )
  expect_equal(
    .str_pair_match(
      pattern = "([^mackarel]*)(spain)",
      string = c("togospain", "djiboutispain")
    ),
    c(togo = "spain", djibouti = "spain")
  )
  expect_equal(
    .str_pair_match(
      pattern = "(a*)b(c*)",
      string = c("abc", "aabcc")
    ),
    c(a = "c", aa = "cc")
  )
})

test_that(".str_pair_match error if more than two outputs", {
  expect_error(.str_pair_match(string = "hadoop", pattern = "(ha)(do)(op)"))
  # no error if explicit
  .str_pair_match(string = "hadoop", pattern = "(ha)(do)(op)", pos = c(1, 2))
})


test_that("chapter to desc range works for icd9", {
  expect_identical(
    .chapter_to_desc_range.icd9("jack (110-120)"),
    list(Jack = c(start = "110", end = "120"))
  )
  expect_identical(
    .chapter_to_desc_range.icd9("jack (V10-V20)"),
    list(Jack = c(start = "V10", end = "V20"))
  )
  expect_identical(
    .chapter_to_desc_range.icd9("jack (E990-E991)"),
    list(Jack = c(start = "E990", end = "E991"))
  )
  expect_identical(
    .chapter_to_desc_range.icd9("jack (110)"),
    list(Jack = c(start = "110", end = "110"))
  )
  expect_identical(
    .chapter_to_desc_range.icd9("jack (V10)"),
    list(Jack = c(start = "V10", end = "V10"))
  )
  expect_identical(
    .chapter_to_desc_range.icd9("jack (E990)"),
    list(Jack = c(start = "E990", end = "E990"))
  )
})

test_that("chapter to desc range works for icd10", {
  expect_identical(
    .chapter_to_desc_range.icd10("jack (A10-A20)"),
    list(Jack = c(start = "A10", end = "A20"))
  )
  expect_identical(
    .chapter_to_desc_range.icd10("jack (V10-V20)"),
    list(Jack = c(start = "V10", end = "V20"))
  )
  expect_identical(
    .chapter_to_desc_range.icd10("jack (E99)"),
    list(Jack = c(start = "E99", end = "E99"))
  )
})

test_that("title case works", {
  expect_identical(.to_title_case("jack"), "Jack")
  expect_identical(.to_title_case("jack wasey"), "Jack Wasey")
  expect_identical(.to_title_case("a [HIV] b"), "A [HIV] B")
  expect_identical(
    .to_title_case("Arthropod-borne Viral Fevers And Viral Hemorrhagic Fevers"),
    "Arthropod-Borne Viral Fevers And Viral Hemorrhagic Fevers"
  )
  expect_identical(.to_title_case("ill-defined"), "Ill-Defined")
  expect_identical(
    .to_title_case("body mass index [bmi]"), "Body Mass Index [Bmi]"
  )
  expect_identical(
    .to_title_case("body mass index [BMI]"), "Body Mass Index [BMI]"
  )
})
