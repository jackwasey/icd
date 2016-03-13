context("S3 class functions")

test_that("classes are created and identified correctly", {
  expect_true(is.icd9(icd9("V10")))
  expect_true(is.icd9cm(icd9cm("V10")))
  expect_true(is.icd10(icd10("V10")))
  expect_true(is.icd10cm(icd10cm("V10")))
  expect_true(is.icd10who(icd10who("V10")))
})

test_that("subclasses still have parent class", {
  expect_true(is.icd9(icd9cm("V10")))
  expect_true(is.icd10(icd10cm("V10")))
  expect_true(is.icd10(icd10who("V10")))
})

test_that("setting conflicting icd data class gives error", {
  expect_error(icd10(icd9("V10")))
  expect_error(icd10cm(icd9("V10")))
  expect_error(icd10who(icd9("V10")))
  expect_error(icd10(icd9cm("V10")))
  expect_error(icd10cm(icd9cm("V10")))
  expect_error(icd10who(icd9cm("V10")))
  expect_error(icd9(icd10("V10")))
  expect_error(icd9(icd10cm("V10")))
  expect_error(icd9(icd10who("V10")))
  expect_error(icd9cm(icd10("V10")))
  expect_error(icd9cm(icd10cm("V10")))
  expect_error(icd9cm(icd10who("V10")))
})

test_that("well ordered class lists are created", {
  expect_icd_classes_ordered(icd9(""))
  expect_icd_classes_ordered(icd9cm(""))
  expect_icd_classes_ordered(icd10(""))
  expect_icd_classes_ordered(icd10cm(""))
  expect_icd_classes_ordered(icd10who(""))

  expect_icd_classes_ordered(icd_short_code(""))
  expect_icd_classes_ordered(icd_decimal_code(""))
})

test_that("well ordered class lists short/decimal ICD combos are created", {
  expect_icd_classes_ordered(icd_short_code(icd9("V102")))
  expect_icd_classes_ordered(icd_decimal_code(icd9cm("410.00")))
  expect_icd_classes_ordered(icd_short_code(icd10("A100")))
  expect_icd_classes_ordered(icd_decimal_code(icd10cm("B23.1")))
  expect_icd_classes_ordered(icd_short_code(icd10who("C33")))

  expect_icd_classes_ordered(icd_decimal_code(icd9("V10.2")))
  expect_icd_classes_ordered(icd_short_code(icd9cm("41000")))
  expect_icd_classes_ordered(icd_decimal_code(icd10("A10.0")))
  expect_icd_classes_ordered(icd_short_code(icd10cm("B231")))
  expect_icd_classes_ordered(icd_decimal_code(icd10who("C33")))
})

test_that("warn if changing ICD decimal to short or vice versa", {
  expect_warning(icd_short_code(icd_decimal_code("10.1")))
  expect_warning(icd_decimal_code(icd_short_code("2222")))
  expect_warning(icd_short_code(icd_decimal_code(icd9cm("10.1"))))
  expect_warning(icd_decimal_code(icd_short_code(icd10who("D22"))))
})

test_that("is short or decimal code", {
  # todo, somehow clarify that this is not the same is icd_is_valid...
  expect_true(is.icd_short_code(icd_short_code("1234")))
  expect_true(is.icd_decimal_code(icd_decimal_code("12.34")))
  expect_is(icd_short_code("1234"), "icd_short_code")
  expect_is(icd_decimal_code("12.34"), "icd_decimal_code")
})

test_that("no warning for combinding same types", {
  expect_warning(c(icd9cm(""), icd9cm("")), NA)
  expect_warning(c(icd9(""), icd9("")), NA)
  expect_warning(c(icd10(""), icd10("")), NA)
  expect_warning(c(icd10cm(""), icd10cm("")), NA)
  expect_warning(c(icd10who(""), icd10who("")), NA)
})

test_that("warn if combining mixed ICD sub-version types", {
  skip("hold off this for now")
  expect_warning(c(icd9cm(""), icd9("")))
  expect_warning(c(icd10cm(""), icd10("")))
  expect_warning(c(icd10who(""), icd10("")))
  expect_warning(c(icd10who(""), icd10cm("")))
})

test_that("error if combining mixed ICD version types, e.g. ICD-9 vs ICD-10", {
  expect_error(c(icd9cm(""), icd10("")))
  expect_error(c(icd10cm(""), icd9("")))
  expect_error(c(icd10who(""), icd9cm("")))
  expect_error(c(icd10who(""), icd9("")))
})

test_that("combining identity", {
  expect_identical(c(icd9cm("")), icd9cm(""))
  expect_identical(c(icd9cm("")), icd9cm(""))
  expect_identical(c(icd10("")), icd10(""))
  expect_identical(c(icd10cm("")), icd10cm(""))
  expect_identical(c(icd10who("")), icd10who(""))
  expect_identical(c(icd_short_code("")), icd_short_code(""))
  expect_identical(c(icd_decimal_code("")), icd_decimal_code(""))
})

test_that("ICD version supertype set", {
  expect_is(icd9cm(""), "icd9")
  expect_is(icd10cm(""), "icd10")
  expect_is(icd10who(""), "icd10")
})

x <- icd::icd9_map_quan_elix

test_that("constructing a comorbidity map works", {
  expect_equal(icd_comorbidity_map(x), x)
  expect_equivalent(as.list(x), x)
  expect_equivalent(icd_comorbidity_map(as.list(x)), x)
})

test_that("constructing a comorbidity map with unnamed list, etc. fails", {
  expect_error(icd_comorbidity_map(unname(unclass(x))))
  # and data frames should definitely fail
  expect_error(icd_comorbidity_map(icd::vermont_dx))
  expect_error(icd_comorbidity_map(icd::uranium_pathology))
})

test_that("subsetting a comorbidity map gives the right class", {

  wonky_map <- icd_comorbidity_map(icd_short_code(icd9cm(list(a = icd9cm("100"), b = icd9("V22")))))

  expect_is(wonky_map, "icd_comorbidity_map")
  expect_is(wonky_map, "icd_short_code")
  expect_is(wonky_map, "icd9cm")
  expect_is(wonky_map, "icd9")

  expect_true(!inherits(wonky_map[[1]], "icd_comorbidity_map"))
  expect_is(wonky_map[[1]], "icd_short_code")
  expect_is(wonky_map[[1]], "icd9cm")
  expect_is(wonky_map[[1]], "icd9")
  expect_is(wonky_map[[1]], "character")

  expect_true(!inherits(wonky_map[[2]], "icd_comorbidity_map"))
  expect_is(wonky_map[[2]], "icd_short_code")
  expect_is(wonky_map[[2]], "icd9cm")
  expect_is(wonky_map[[2]], "icd9")
  expect_is(wonky_map[[2]], "character")
})

test_that("constructing wide data works", {
  expect_equal(icd_wide_data(icd::vermont_dx), icd::vermont_dx)
  expect_equivalent(icd_wide_data(icd::vermont_dx), icd::vermont_dx)
  expect_equivalent(icd_wide_data(as.data.frame(icd::vermont_dx)), icd::vermont_dx)
})

test_that("constructing long data works", {
  expect_equal(icd_long_data(icd::uranium_pathology), icd::uranium_pathology)
  expect_equivalent(icd_long_data(icd::uranium_pathology), icd::uranium_pathology)
  expect_equivalent(icd_long_data(as.data.frame(icd::uranium_pathology)), icd::uranium_pathology)
})

test_that("is long or wide data?", {
  expect_true(is.icd_wide_data(icd_wide_data(icd::vermont_dx)))
  expect_true(is.icd_long_data(icd_long_data(icd::uranium_pathology)))
  expect_true(is.icd_wide_data(icd::vermont_dx))
  expect_true(is.icd_long_data(icd::uranium_pathology))
  expect_is(icd_wide_data(icd::vermont_dx), "icd_wide_data")
  expect_is(icd_long_data(icd::uranium_pathology), "icd_long_data")
})

test_that("constructing wide or long format for non-data frame gives error", {
  expect_error(icd_wide_data(e))
  expect_error(icd_wide_data(letters))
})

test_that("subsetting data frame works", {
  expect_equal(unclass(icd::vermont_dx[1, 6]), "27801")
  expect_equal(unclass(icd::vermont_dx[1, "DX1"]), "27801")
  expect_is(icd::vermont_dx[1, "DX1"], c("icd9cm", "icd9", "character"))
  expect_equal(unclass(icd::vermont_dx[[1, 6]]), "27801")
  expect_is(icd::vermont_dx[[1, "DX1"]], c("icd9cm", "icd9", "character"))
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

test_that("subset with double bracket doesn't override the underlying class", {
  x <- icd9(list(my_codes = c("V10.1", "441.1")))
  expect_false(inherits(x[[1]], "list"))
  expect_false(inherits(x[[1]][2], "list"))

  y <- icd10(list(thine = icd10cm(c("A01", "B0234"))))
  expect_true(inherits(y[1], "list"))

  expect_false(inherits(y[[1]], "list"))
  expect_false(inherits(y[[1]][1], "list"))
})

test_that("printing a comorbidity map works very simply", {
  expect_warning(
    capture.output(
      print.icd_comorbidity_map(icd::icd9_map_quan_elix)
    ), NA)
})

test_that("is comorbidity map?", {
  icd9_map_ahrq %>% unclass %>% icd9 %>% icd_comorbidity_map -> x
  expect_true(is.icd_comorbidity_map(icd9_map_ahrq))
  expect_true(is.icd_comorbidity_map(x))
  expect_is(x, "icd_comorbidity_map")
  expect_is(icd9_map_ahrq, "icd_comorbidity_map")
})

context("class conflicts")

test_that("no conflict for standard classes", {
  expect_false(icd_classes_conflict(icd9("100.1")))
  expect_false(icd_classes_conflict(icd9("100.1") %>% icd_decimal_code))
  expect_false(icd_classes_conflict(icd9("1001") %>% icd_short_code))
  expect_false(icd_classes_conflict(icd9cm("100.1")))
  expect_false(icd_classes_conflict(icd9cm("100.1") %>% icd_decimal_code))
  expect_false(icd_classes_conflict(icd9cm("1001") %>% icd_short_code))
  expect_false(icd_classes_conflict(icd10("A00.0")))
  expect_false(icd_classes_conflict(icd10("A00.0") %>% icd_decimal_code))
  expect_false(icd_classes_conflict(icd10("A000") %>% icd_short_code))
  expect_false(icd_classes_conflict(icd10cm("A00.0")))
  expect_false(icd_classes_conflict(icd10cm("A00.0") %>% icd_decimal_code))
  expect_false(icd_classes_conflict(icd10cm("A000") %>% icd_short_code))
  expect_false(icd_classes_conflict(icd10who("A00.0")))
  expect_false(icd_classes_conflict(icd10who("A00.0") %>% icd_decimal_code))
  expect_false(icd_classes_conflict(icd10who("A000") %>% icd_short_code))
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
  expect_true(icd_classes_conflict(structure("V10", class = c("icd9cm", "icd10", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd9", "icd10", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd9cm", "icd10cm", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd10cm", "icd10who", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd10cm", "icd9", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd10cm", "icd9cm", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd10", "icd9", "character"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd10", "icd9cm", "character"))))
  expect_true(icd_classes_conflict(structure(list("V10", "A20"), class = c("icd9cm", "icd10", "list"))))
})

test_that("conflicting short vs decimal class asssignment", {
  expect_true(icd_classes_conflict(structure("V10", class = c("icd_short_code", "icd_decimal_code"))))
  expect_true(icd_classes_conflict(structure("V10", class = c("icd_decimal_code", "icd_short_code"))))
})

test_that("long vs wide data conflict identified", {
  v_bad <- vermont_dx
  class(v_bad) <- c(class(v_bad), "icd_long_data")
  u_bad <- uranium_pathology
  class(u_bad) <- c(class(u_bad), "icd_wide_data")

  expect_true(icd_classes_conflict(v_bad))
  expect_true(icd_classes_conflict(u_bad))
})


context("class updates")

test_that("update data frame class for simple cases", {

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
