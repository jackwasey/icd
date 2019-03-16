context("sub-types of ICD-9 code")

vs <- c("V1", "V99", " V05", "v19x", " v200 ")
es <- c("E00", "E9999", " E905", "e901", " e888 ")
ns <- c("0.1", "1.2", " 2", "33", " 444XX", "555.5", "66666 ", "789.01")

test_that("find codes", {
  expect_true(icd9_is_v(" V10 "))
  expect_true(icd9_is_e(" E800"))
  expect_true(icd9_is_n(" 10.1"))
  expect_true(is_major.icd9("100"))
  expect_true(is_major.icd9("E900"))
  expect_false(is_major.icd9("V90.3"))
  expect_false(is_major.icd9("E900.3"))
  expect_false(is_major.icd9("V903"))
  expect_false(is_major.icd9("E9003"))
  # todo, what about "E800." or "100." ?
  expect_false(icd9_is_v(" E900.3 "))
  expect_false(icd9_is_e(" 80.2"))
  expect_false(icd9_is_n(" V10.1"))
})

test_that("'is' works for multiple values", {
  expect_true(all(icd9_is_v(vs)))
  expect_true(all(icd9_is_e(es)))
  expect_true(all(icd9_is_n(ns)))
  expect_false(all(icd9_is_v(ns)))
  expect_false(all(icd9_is_e(vs)))
  expect_false(all(icd9_is_n(es)))

  expect_false(any(icd9_is_n(c(vs, es))))
})

test_that("'is' works for factors", {
  expect_true(all(icd9_is_v(factor(vs))))
  expect_true(all(icd9_is_e(factor(es))))
  expect_true(all(icd9_is_n(factor(ns))))
  expect_false(all(icd9_is_v(factor(ns))))
  expect_false(all(icd9_is_e(factor(vs))))
  expect_false(all(icd9_is_n(factor(es))))
})

test_that("'is' mixed values, factors and vectors", {
  v <- c("E100.1", "V234", "12", "V34.2", "61523", "10.2", "E9991", " V45XX ")
  expect_equal(
    icd9_is_n(v),
    c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_equal(
    icd9_is_v(v),
    c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)
  )
  expect_equal(
    icd9_is_e(v),
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  )
})

test_that("ICD-10 majors identified", {
  expect_true(is_major.icd10("A01"))
  # for now, leave "A1" as undefined.
  expect_true(is_major.icd10("Z99"))

  expect_true(is_major("V10"))
  expect_true(is_major.icd9("V10"))
  expect_true(is_major.icd10("V10"))

  expect_false(is_major.icd10("100"))
  expect_false(is_major.icd10("E999"))
  expect_false(is_major.icd10("V100"))
  expect_false(is_major.icd10("10"))
  expect_false(is_major.icd10("1"))
})
