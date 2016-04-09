context("S3 class functions")

test_that("classes are created and identified correctly", {
  expect_true(is.icd9(icd9("V10")))
  expect_true(is.icd9cm(icd9cm("V10")))
  expect_true(is.icd10(icd10("V10")))
  expect_true(is.icd10cm(icd10cm("V10")))
})

test_that("subclasses still have parent class", {
  expect_true(is.icd9(icd9cm("V10")))
  expect_true(is.icd10(icd10cm("V10")))
})

test_that("setting conflicting icd data class gives error", {
  expect_error(as.icd10(icd9("V10")))
  expect_error(as.icd10cm(icd9("V10")))
  expect_error(as.icd10(icd9cm("V10")))
  expect_error(as.icd10cm(icd9cm("V10")))
  expect_error(as.icd9(icd10("V10")))
  expect_error(as.icd9(icd10cm("V10")))
  expect_error(as.icd9cm(icd10("V10")))
  expect_error(as.icd9cm(icd10cm("V10")))
})

test_that("well ordered class lists are created", {
  expect_icd_classes_ordered(icd9(""))
  expect_icd_classes_ordered(icd9cm(""))
  expect_icd_classes_ordered(icd10(""))
  expect_icd_classes_ordered(icd10cm(""))

  skip("this is time consuming, and we should probably tolerate mixed order anyway")
  expect_icd_classes_ordered(icd_short_diag(""))
  expect_icd_classes_ordered(as.icd_decimal_diag(""))
})

test_that("well ordered class lists short/decimal ICD combos are created", {
  skip("this is time consuming, and we should probably tolerate mixed order anyway")
  expect_icd_classes_ordered(icd_short_diag(icd9("V102")))
  expect_icd_classes_ordered(as.icd_decimal_diag(icd9cm("410.00")))
  expect_icd_classes_ordered(icd_short_diag(icd10("A100")))
  expect_icd_classes_ordered(as.icd_decimal_diag(icd10cm("B23.1")))

  expect_icd_classes_ordered(as.icd_decimal_diag(icd9("V10.2")))
  expect_icd_classes_ordered(icd_short_diag(icd9cm("41000")))
  expect_icd_classes_ordered(as.icd_decimal_diag(icd10("A10.0")))
  expect_icd_classes_ordered(icd_short_diag(icd10cm("B231")))
})

test_that("warn if changing ICD decimal to short or vice versa", {
  skip("not sure I want this behaviour")
  expect_warning(as.icd_short_diag(as.icd_decimal_diag("10.1"), warn = TRUE))
  expect_warning(as.icd_decimal_diag(as.icd_short_diag("2222"), warn = TRUE))
  expect_warning(as.icd_short_diag(as.icd_decimal_diag(icd9cm("10.1")), warn = TRUE))
})

test_that("is short or decimal code", {
  # todo, somehow clarify that this is not the same is icd_is_valid...
  expect_true(is.icd_short_diag(as.icd_short_diag("1234")))
  expect_true(is.icd_decimal_diag(as.icd_decimal_diag("12.34")))
  expect_true(is.icd_short_diag(as.icd_short_diag("1234")))
  expect_true(is.icd_short_diag(as.icd_short_diag("1234")))
  expect_true(is.icd_decimal_diag(as.icd_decimal_diag("12.34")))
  expect_true(is.icd_decimal_diag(as.icd_decimal_diag("12.34")))
})

test_that("no warning or error for combining same types", {
  expect_warning(c(as.icd9cm(""), as.icd9cm("")), regexp = NA)
  expect_warning(c(as.icd9(""), as.icd9("")), regexp = NA)
  expect_warning(c(as.icd10(""), as.icd10("")), regexp = NA)
  expect_warning(c(as.icd10cm(""), as.icd10cm("")), regexp = NA)
})

test_that("warn if combining mixed ICD sub-version types", {
  skip("hold off this for now")
  c(as.icd9cm(""), as.icd9(""))
  c(as.icd10cm(""), as.icd10(""))
})

test_that("error if combining mixed ICD version types, e.g. ICD-9 vs ICD-10", {
  skip("this is nice to have, but adds weight to 'c'")
  expect_error(c(as.icd9cm(""), as.icd10("")))
  expect_error(c(as.icd10cm(""), as.icd9("")))
})

test_that("combining identity", {
  expect_identical(c(as.icd9cm("")), as.icd9cm(""))
  expect_identical(c(as.icd9cm("")), as.icd9cm(""))
  expect_identical(c(as.icd10("")), as.icd10(""))
  expect_identical(c(as.icd10cm("")), as.icd10cm(""))
})

test_that("attributes set and queried", {
  no_short_code <- z <- y <- x <- icd9("")
  attr(x, "icd_short_diag") <- TRUE
  attr(y, "icd_short_diag") <- TRUE
  attr(z, "icd_short_diag") <- FALSE
  expect_true(is.icd_short_diag(x))
  expect_null(is.icd_short_diag(no_short_code))
})

test_that("ICD version supertype set", {
  expect_true(is.icd9(icd9cm("")))
  expect_true(is.icd10(icd10cm("")))
})

x <- icd::icd9_map_quan_elix

test_that("constructing a comorbidity map works", {
  expect_equal(icd_comorbidity_map(x), x)
  expect_equal(as.icd_comorbidity_map(x), x)
  expect_equivalent(as.list(x), x)
  expect_equivalent(icd_comorbidity_map(as.list(x)), x)
})

test_that("constructing a comorbidity map with unnamed list, etc. fails", {
  expect_error(as.icd_comorbidity_map(unname(unclass(x))))
  # and data frames should definitely fail
  expect_error(as.icd_comorbidity_map(icd::vermont_dx))
  expect_error(as.icd_comorbidity_map(icd::uranium_pathology))
})

test_that("subsetting a comorbidity map gives the right class", {

  wonky_map <- icd_comorbidity_map(list(a = as.icd9cm("100"), b = as.icd9cm("V22")))
  attr(wonky_map$a, "icd_short_diag") <- TRUE
  attr(wonky_map$b, "icd_short_diag") <- TRUE

  expect_is(wonky_map, "icd_comorbidity_map")
  expect_null(is.icd_short_diag(wonky_map))

  expect_true(!inherits(wonky_map[[1]], "icd_comorbidity_map"))
  expect_true(is.icd_short_diag(wonky_map[[1]]))
  expect_is(wonky_map[[1]], "icd9cm")
  expect_is(wonky_map[[1]], "icd9")
  expect_is(wonky_map[[1]], "character")

  expect_true(!inherits(wonky_map$a, "icd_comorbidity_map"))
  expect_true(is.icd_short_diag(wonky_map$a))
  expect_is(wonky_map$a, "icd9cm")
  expect_is(wonky_map$a, "icd9")
  expect_is(wonky_map$a, "character")

  expect_true(!inherits(wonky_map[[2]], "icd_comorbidity_map"))
  expect_true(is.icd_short_diag(wonky_map[[2]]))
  expect_is(wonky_map[[2]], "icd9cm")
  expect_is(wonky_map[[2]], "icd9")
  expect_is(wonky_map[[2]], "character")
})

test_that("constructing wide data works", {
  expect_equal(as.icd_wide_data(icd::vermont_dx), icd::vermont_dx)
  expect_equivalent(as.icd_wide_data(icd::vermont_dx), icd::vermont_dx)
  expect_equivalent(as.icd_wide_data(as.data.frame(icd::vermont_dx)), icd::vermont_dx)
})

test_that("constructing long data works", {
  expect_equal(as.icd_long_data(icd::uranium_pathology), icd::uranium_pathology)
  expect_equivalent(as.icd_long_data(icd::uranium_pathology), icd::uranium_pathology)
  expect_equivalent(as.icd_long_data(as.data.frame(icd::uranium_pathology)), icd::uranium_pathology)
})

test_that("is long or wide data?", {
  expect_true(is.icd_wide_data(as.icd_wide_data(icd::vermont_dx)))
  expect_true(is.icd_long_data(as.icd_long_data(icd::uranium_pathology)))
  expect_true(is.icd_wide_data(icd::vermont_dx))
  expect_true(is.icd_long_data(icd::uranium_pathology))
  expect_is(as.icd_wide_data(icd::vermont_dx), "icd_wide_data")
  expect_is(as.icd_long_data(icd::uranium_pathology), "icd_long_data")
})

test_that("constructing wide or long format for non-data frame gives error", {
  expect_error(as.icd_wide_data(e))
  expect_error(as.icd_wide_data(letters))
})

test_that("subsetting data frame works", {
  expect_equal(unclass(icd::vermont_dx[1, 6]), as.icd_short_diag("27801"))
  expect_equal(unclass(icd::vermont_dx[1, "DX1"]), as.icd_short_diag("27801"))
  expect_is(icd::vermont_dx[1, "DX1"], c("icd9cm", "icd9", "character"))
  expect_equivalent(unclass(icd::vermont_dx[[1, 6]]), "27801")
  expect_is(icd::vermont_dx[[1, "DX1"]], c("icd9cm", "icd9", "character"))
  expect_true(is.icd9(icd::vermont_dx[[1, "DX9"]]))
  expect_true(is.icd9cm(icd::vermont_dx[[1, "DX12"]]))
  # columns
  expect_is(icd::vermont_dx[6], c("icd9cm", "icd9", "data.frame")) # not necessarily wide anymore...
  expect_is(icd::vermont_dx[[6]], c("icd9cm", "icd9", "character"))
})

test_that("data frame subsetting doesn't incorrectly set class on columns", {
  expect_true(is.numeric(pts_invalid_mix[c(TRUE, TRUE, TRUE), "visit_id"]))
  expect_false(inherits(pts_invalid_mix[c(TRUE, TRUE, TRUE), "visit_id"], "icd9"))
  expect_equal(sapply(pts_invalid_mix[c(TRUE, TRUE, TRUE), ], class),
               structure(list(visit_id = "numeric", icd9 = c("icd9", "character"
               ), poa = "factor"), .Names = c("visit_id", "icd9", "poa"))
  )
})

test_that("printing a comorbidity map works very simply", {
  expect_warning(
    capture.output(
      print.icd_comorbidity_map(icd::icd9_map_quan_elix)
    ), regexp = NA)
})

test_that("is comorbidity map?", {
  icd9_map_ahrq %>% unclass %>% icd_comorbidity_map -> x
  expect_true(is.icd_comorbidity_map(icd9_map_ahrq))
  expect_true(is.icd_comorbidity_map(x))
  expect_is(icd9_map_ahrq, "icd_comorbidity_map")
  expect_is(x, "icd_comorbidity_map")
})

context("class conflicts")

test_that("no conflict for standard classes", {
  expect_false(icd_classes_conflict(icd9("100.1")))
  expect_false(icd_classes_conflict(icd9("100.1") %>% as.icd_decimal_diag))
  expect_false(icd_classes_conflict(icd9("1001") %>% as.icd_short_diag))
  expect_false(icd_classes_conflict(icd9cm("100.1")))
  expect_false(icd_classes_conflict(icd9cm("100.1") %>% as.icd_decimal_diag))
  expect_false(icd_classes_conflict(icd9cm("1001") %>% as.icd_short_diag))
  expect_false(icd_classes_conflict(icd10("A00.0")))
  expect_false(icd_classes_conflict(icd10("A00.0") %>% as.icd_decimal_diag))
  expect_false(icd_classes_conflict(icd10("A000") %>% as.icd_short_diag))
  expect_false(icd_classes_conflict(icd10cm("A00.0")))
  expect_false(icd_classes_conflict(icd10cm("A00.0") %>% as.icd_decimal_diag))
  expect_false(icd_classes_conflict(icd10cm("A000") %>% as.icd_short_diag))
})

test_that("no conflict for built-in data", {
  expect_false(icd_classes_conflict(vermont_dx))
  expect_false(icd_classes_conflict(uranium_pathology))
  expect_false(icd_classes_conflict(icd9_map_elix))
  expect_false(icd_classes_conflict(icd9_map_elix[2]))
  expect_false(icd_classes_conflict(icd9_map_elix[[2]]))
  expect_false(icd_classes_conflict(icd10_map_quan_deyo[5]))
  expect_false(icd_classes_conflict(icd10_map_quan_deyo[[5]]))
})

test_that("conflicting ICD type classes can be found", {
  skip("working on this")
  expect_true(icd_classes_conflict(structure("V10", class = c("icd9cm", "icd10", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd9", "icd10", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd9cm", "icd10cm", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd10cm", "icd9", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd10cm", "icd9cm", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd10", "icd9", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd10", "icd9cm", "character"))))
  expect_true(icd_classes_conflict(structure(list("V10", "A20"), class = c("icd9cm", "icd10", "list"))))
})

test_that("conflicting short vs decimal class asssignment", {
  skip("working on this")
  expect_true(icd_classes_conflict(structure("V10", class = c("icd_short_diag", "icd_decimal_diag"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd_decimal_diag", "icd_short_diag"))))
})

test_that("long vs wide data conflict identified", {
  skip("WIP")
  v_bad <- vermont_dx
  class(v_bad) <- c(class(v_bad), "icd_long_data")
  u_bad <- uranium_pathology
  class(u_bad) <- c(class(u_bad), "icd_wide_data")

  expect_true(icd_classes_conflict(v_bad))
  expect_true(icd_classes_conflict(u_bad))
})


context("class updates")

test_that("update data frame class for simple cases", {
  skip("obsolete")

  expect_updated_class <- function(fun_name) {

    x <- data.frame(visit_id = c(1, 2, 3),
                    code = do.call(fun_name, list(c("V10", "V10", "V10"))),
                    stringsAsFactors = FALSE)

    eval(bquote(expect_true(inherits(update_data_frame_class(.(x)), fun_name))))
  }

  for (cl in c("icd9", "icd9cm", "icd10", "icd10cm")) {
    message(cl)
    expect_updated_class(cl)
  }
})

test_that("fail to update data frame class with conflicting cols", {
  skip("obsolete")
  x <- data.frame(visit_id = c(1, 2, 3),
                  code.icd9 = icd9(c("100", "V10", "V10")),
                  code.icd10 = icd10(c("A01", "V10", "V10")),
                  stringsAsFactors = FALSE)
  expect_error(update_data_frame_class(x, must_work = TRUE))
  expect_identical(update_data_frame_class(x, must_work = FALSE), x)
})

test_that("can create NA valued ICD code types", {
  expect_true(is.na(as.icd9(NA)))
  expect_true(is.na(as.icd9cm(NA)))
  expect_true(is.na(as.icd10(NA)))
  expect_true(is.na(as.icd10cm(NA)))
})
