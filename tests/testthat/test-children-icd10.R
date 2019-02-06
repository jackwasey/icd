context("generate defined child codes for ICD-10-CM")

expect_icd10cm_child_is_self <- function(...) {
  dots <- unlist(list(...))
  for (i in dots) {
    eval(bquote(expect_identical(children(icd:::icd10cm(.(i))),
                                 as.icd10cm(as.short_diag(.(i))))))
    children(icd10(i))
    eval(bquote(expect_warning(warn_res <- children(as.icd10cm(.(i))),
                               regexp = NA)))
    eval(bquote(expect_is(warn_res, "icd10")))
    eval(bquote(expect_is(warn_res, "icd10cm")))
    eval(bquote(expect_warning(warn_res <- children(.(i)), regexp = NA)))
    eval(bquote(expect_identical(warn_res,
                                 as.icd10(as.short_diag(.(i))))))
    eval(bquote(expect_identical(children.icd10cm(.(i)),
                                 as.icd10cm(as.short_diag(.(i))))))
    eval(bquote(expect_identical(children.icd10cm(as.icd10(.(i))),
                                 as.icd10cm(as.short_diag(.(i))))))
    eval(bquote(expect_identical(children.icd10cm(as.icd10cm(.(i))),
                                 as.icd10cm(as.short_diag(.(i))))))

    # at present, the children are only even icd10cm, but we should not enforce
    # this:
    children.icd10(icd10(i)) # should not warn
    eval(bquote(expect_true(is.icd10(warn_res))))
    eval(bquote(expect_equivalent(unclass(.(warn_res)), .(i))))

    children(icd10(i)) # should not warn
    eval(bquote(expect_true(is.icd10(warn_res))))
    eval(bquote(expect_equivalent(unclass(.(warn_res)), .(i))))
  }
}

test_that("errors found in development", {
  expect_error(regexp = NA, children_defined.icd10cm("C17"))
})

test_that("children of a leaf node returns itself", {
  expect_icd10cm_child_is_self("O9A119", "O9A53", "S0000XA", "T3299", "P150",
                               "P159", "Z9981", "Z9989", "Z950", "C7A098",
                               "C7A8")
  set.seed(1441)
  rand_icd10cm <- generate_random_short_icd10cm_bill(50)
  expect_icd10cm_child_is_self(rand_icd10cm)
})

test_that("zero length ICD-10-CM children", {
  expect_empty_icd10cm_kids <- function(x, has_warning = TRUE) {
    if (has_warning)
      eval(bquote(expect_warning(res <- children_defined.icd10cm(x))))
    else
      eval(bquote(expect_warning(res <- children_defined.icd10cm(x),
                                 regexp = NA)))
    eval(bquote(expect_equivalent(res, as.icd10cm(character(0)))))
  }
  expect_empty_icd10cm_kids("%!^#&<>?,./")
  expect_empty_icd10cm_kids("")
  expect_empty_icd10cm_kids(c("%!^#&<>?,./", ""))
  expect_empty_icd10cm_kids(c("", ""))
  expect_empty_icd10cm_kids(character(0), has_warning = FALSE)
  expect_warning(children_defined(icd:::icd10cm(character(0))),
                 icd:::icd10cm(character(0)),
                 regexp = NA)
})

test_that("icd10cm children with one of several missing should not segfault", {
  expect_identical(
    children.icd10cm(c("I792", "K551")),
    children.icd10cm("K551")
  )
  expect_identical(
    children.icd10cm(c("I790", "I792")),
    children.icd10cm(c("I790"))
  )
})

test_that("class of children same as input class", {
  expect_identical(class(as.icd9("666.32")),
                   class(children(as.icd9("666.32"))))
  expect_identical(class(as.icd9cm("666.32")),
                   class(children(as.icd9cm("666.32"))))
  expect_identical(class(as.icd10("T27.6XXD")),
                   class(children(as.icd10("T27.6XXD"))))
  expect_identical(class(as.icd10cm("T27.6XXD")),
                   class(children(as.icd10cm("T27.6XXD"))))
})

context("WHO ICD-10 children")

test_that("basic", {
  skip_if_not_installed("icd.data", 1.1)
  skip_missing_icd10who(ver = "2016")
  children(as.icd10who("A01"))
})
