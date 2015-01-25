context("tests for type of ICD-9 code")

vs <- c("V1", "V99", " V05", "v19", " v200 ")
es <- c("E00", "E9999", " E905", "e901", " e888 ")
ns <- c("0.1", "1.2", "2", "33", "444", "555.5", "66666", "789.01")

test_that("find codes", {
  expect_true(icd9IsV(" V10 "))
  expect_true(icd9IsE(" E800"))
  expect_true(icd9IsN(" 10.1"))
  expect_true(all(icd9IsV(vs)))
  expect_true(all(icd9IsE(es)))
  expect_true(all(icd9IsN(ns)))
})

test_that("slow versions for comparison", {
  expect_equal(icd9IsV(vs), icd9IsV_cpp_slow(vs))
  expect_equal(icd9IsV(vs), icd9IsV_cpp_slower(vs))
  expect_equal(icd9IsV(es), icd9IsV_cpp_slow(es))
  expect_equal(icd9IsV(es), icd9IsV_cpp_slower(es))
  expect_equal(icd9IsV(ns), icd9IsV_cpp_slow(ns))
  expect_equal(icd9IsV(ns), icd9IsV_cpp_slower(ns))
})

test_that("slow and maybe obsolete", {
  expect_true(all(icd9IsVE(c(vs, es))))
  expect_equal(icd9IsVE(c(vs, es)), icd9IsVE_R(c(vs, es)))
  expect_equal(icd9IsVE(c(vs, es)), icd9IsVE_cpp_slow(c(vs, es)))

  expect_error(icd9IsASingleVE(c("V12", "E800")))
  expect_true(icd9IsASingleVE(" V12"))
  expect_true(icd9IsASingleVE("E123 "))
  expect_false(icd9IsASingleVE(""))
  expect_false(icd9IsASingleVE("441.4"))
  expect_false(icd9IsASingleVE("not a lot of good symbols"))
})
