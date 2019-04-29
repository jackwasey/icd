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
  expect_classes_ordered(icd9(""))
  expect_classes_ordered(icd9cm(""))
  expect_classes_ordered(icd10(""))
  expect_classes_ordered(icd10cm(""))

  expect_classes_ordered(as.short_diag(""))
  expect_classes_ordered(as.decimal_diag(""))
  expect_classes_ordered(as.decimal_diag(as.icd9("")))
})

test_that("well ordered class lists short/decimal ICD combos are created", {
  expect_classes_ordered(as.short_diag(icd9("V102")))
  expect_classes_ordered(as.decimal_diag(icd9cm("410.00")))
  expect_classes_ordered(as.short_diag(icd10("A100")))
  expect_classes_ordered(as.decimal_diag(icd10cm("B23.1")))

  expect_classes_ordered(as.decimal_diag(icd9("V10.2")))
  expect_classes_ordered(as.short_diag(icd9cm("41000")))
  expect_classes_ordered(as.decimal_diag(icd10("A10.0")))
  expect_classes_ordered(as.short_diag(icd10cm("B231")))
})

test_that("is short or decimal code", {
  expect_true(is.short_diag(as.short_diag("1234")))
  expect_true(is.decimal_diag(as.decimal_diag("12.34")))
  expect_true(is.short_diag(as.short_diag("1234")))
  expect_true(is.short_diag(as.short_diag("1234")))
  expect_true(is.decimal_diag(as.decimal_diag("12.34")))
  expect_true(is.decimal_diag(as.decimal_diag("12.34")))
})

test_that("no warning or error for combining same types", {
  expect_warning(c(as.icd9cm(""), as.icd9cm("")), regexp = NA)
  expect_warning(c(as.icd9(""), as.icd9("")), regexp = NA)
  expect_warning(c(as.icd10(""), as.icd10("")), regexp = NA)
  expect_warning(c(as.icd10cm(""), as.icd10cm("")), regexp = NA)
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
  expect_true(is.short_diag(x))
  expect_false(is.short_diag(no_short_code))
})

test_that("ICD version supertype set", {
  expect_true(is.icd9(icd9cm("")))
  expect_true(is.icd10(icd10cm("")))
})

x <- icd9_map_quan_elix

test_that("constructing a comorbidity map works", {
  expect_equal(comorbidity_map(x), x)
  expect_equal(as.comorbidity_map(x), x)
  expect_equivalent(as.list(x), x)
  expect_equivalent(comorbidity_map(as.list(x)), x)
})

test_that("constructing a comorbidity map with unnamed list, etc. fails", {
  expect_error(as.comorbidity_map(unname(unclass(x))))
})

test_that("subsetting a comorbidity map gives the right class", {
  wonky_map <- comorbidity_map(list(a = as.icd9cm("100"), b = as.icd9cm("V22")))
  attr(wonky_map$a, "icd_short_diag") <- TRUE
  attr(wonky_map$b, "icd_short_diag") <- TRUE

  expect_is(wonky_map, "comorbidity_map")
  expect_false(is.short_diag(wonky_map))

  expect_true(!inherits(wonky_map[[1]], "comorbidity_map"))
  expect_true(is.short_diag(wonky_map[[1]]))
  expect_is(wonky_map[[1]], "icd9cm")
  expect_is(wonky_map[[1]], "icd9")
  expect_is(wonky_map[[1]], "character")

  expect_true(!inherits(wonky_map$a, "comorbidity_map"))
  expect_true(is.short_diag(wonky_map$a))
  expect_is(wonky_map$a, "icd9cm")
  expect_is(wonky_map$a, "icd9")
  expect_is(wonky_map$a, "character")

  expect_true(!inherits(wonky_map[[2]], "comorbidity_map"))
  expect_true(is.short_diag(wonky_map[[2]]))
  expect_is(wonky_map[[2]], "icd9cm")
  expect_is(wonky_map[[2]], "icd9")
  expect_is(wonky_map[[2]], "character")
})

test_that("constructing wide data works", {
  expect_equal(
    as.icd_wide_data(vermont_dx),
    vermont_dx
  )
  expect_equivalent(
    as.icd_wide_data(vermont_dx),
    vermont_dx
  )
  expect_equivalent(
    as.icd_wide_data(as.data.frame(vermont_dx)),
    vermont_dx
  )
})

test_that("constructing long data works", {
  l <- data.frame(id = c(1, 1), code = c("A10", "B10"))
  expect_true(is.icd_long_data(as.icd_long_data(l)))
  expect_identical(
    as.icd_long_data(uranium_pathology),
    uranium_pathology
  )
  expect_equivalent(
    as.icd_long_data(uranium_pathology),
    uranium_pathology
  )
  expect_identical(
    as.icd_long_data(as.data.frame(uranium_pathology)),
    uranium_pathology
  )
})

test_that("is long or wide data?", {
  expect_true(is.icd_wide_data(as.icd_wide_data(vermont_dx)))
  expect_true(is.icd_long_data(as.icd_long_data(uranium_pathology)))
  expect_true(is.icd_wide_data(vermont_dx))
  expect_true(is.icd_long_data(uranium_pathology))
  expect_is(as.icd_wide_data(vermont_dx), "icd_wide_data")
  expect_is(as.icd_long_data(uranium_pathology), "icd_long_data")
})

test_that("constructing wide or long format for non-data frame gives error", {
  expect_error(as.icd_wide_data(e))
  expect_error(as.icd_wide_data(letters))
})

test_that("subsetting data frame works", {
  v_subsets <- list(
    vermont_dx,
    vermont_dx[1:10, ],
    vermont_dx[, 1:15],
    vermont_dx[1:10, 1:17]
  )
  for (v in v_subsets) {
    i <- paste(nrow(v), "x", ncol(v))
    expect_equal(unclass(v[1, "DX1"]), as.short_diag("27801"), info = i)
    expect_true(is.short_diag(v[, "DX2"]))
    expect_equivalent(unclass(v[[1, "DX1"]]), "27801", info = i)
    expect_is(v[2, "DX3"], c("icd9cm", "icd9", "character"), info = i)
    expect_is(v[[3, "DX4"]], c("icd9cm", "icd9", "character"), info = i)
    expect_true(is.icd9(v[[4, "DX5"]]), info = i)
    expect_true(is.icd9cm(v[[5, "DX6"]]), info = i)
    # columns
    expect_is(v[9], c("icd9cm", "icd9", "data.frame"), info = i)
    expect_is(v[[8]], c("icd9cm", "icd9", "character"), info = i)
  }
})

test_that("data frame subsetting doesn't incorrectly set class on columns", {
  expect_true(is.numeric(pts_invalid_mix[c(TRUE, TRUE, TRUE), "visit_id"]))
  expect_false(
    inherits(pts_invalid_mix[c(TRUE, TRUE, TRUE), "visit_id"], "icd9")
  )
  expect_equal(
    lapply(pts_invalid_mix[c(TRUE, TRUE, TRUE), ], class),
    structure(list(visit_id = "numeric", icd9 = c("icd9", "character"), poa = "factor"), .Names = c("visit_id", "icd9", "poa"))
  )
})

test_that("printing a comorbidity map works very simply", {
  expect_warning(
    capture.output(
      print.comorbidity_map(icd9_map_quan_elix)
    ),
    regexp = NA
  )
})

test_that("is comorbidity map?", {
  icd9_map_ahrq %>%
    unclass() %>%
    comorbidity_map() -> x
  expect_true(is.comorbidity_map(icd9_map_ahrq))
  expect_true(is.comorbidity_map(x))
  expect_is(icd9_map_ahrq, "comorbidity_map")
  expect_is(x, "comorbidity_map")
})

context("class conflicts")

test_that("no conflict for standard classes", {
  expect_false(icd_classes_conflict(icd9("100.1")))
  expect_false(icd_classes_conflict(icd9("100.1") %>% as.decimal_diag()))
  expect_false(icd_classes_conflict(icd9("1001") %>% as.short_diag()))
  expect_false(icd_classes_conflict(icd9cm("100.1")))
  expect_false(icd_classes_conflict(icd9cm("100.1") %>% as.decimal_diag()))
  expect_false(icd_classes_conflict(icd9cm("1001") %>% as.short_diag()))
  expect_false(icd_classes_conflict(icd10("A00.0")))
  expect_false(icd_classes_conflict(icd10("A00.0") %>% as.decimal_diag()))
  expect_false(icd_classes_conflict(icd10("A000") %>% as.short_diag()))
  expect_false(icd_classes_conflict(icd10cm("A00.0")))
  expect_false(icd_classes_conflict(icd10cm("A00.0") %>% as.decimal_diag()))
  expect_false(icd_classes_conflict(icd10cm("A000") %>% as.short_diag()))
})

test_that("no conflict for built-in data", {
  expect_false(icd_classes_conflict(icd9_map_elix))
  expect_false(icd_classes_conflict(icd9_map_elix[2]))
  expect_false(icd_classes_conflict(icd9_map_elix[[2]]))
  expect_false(icd_classes_conflict(icd10_map_quan_deyo[5]))
  expect_false(icd_classes_conflict(icd10_map_quan_deyo[[5]]))
})

test_that("conflicting ICD type classes can be found", {
  expect_true(
    icd_classes_conflict(
      structure("V10",
        class = c("icd9cm", "icd10", "character")
      )
    )
  )
  expect_true(
    icd_classes_conflict(
      structure("V10",
        class = c("icd9", "icd10", "character")
      )
    )
  )
  expect_true(
    icd_classes_conflict(
      structure("V10",
        class = c("icd9cm", "icd10cm", "character")
      )
    )
  )
  expect_true(
    icd_classes_conflict(
      structure("V10",
        class = c("icd10cm", "icd9", "character")
      )
    )
  )
  expect_true(
    icd_classes_conflict(
      structure("V10",
        class = c("icd10cm", "icd9cm", "character")
      )
    )
  )
  expect_true(
    icd_classes_conflict(
      structure("V10",
        class = c("icd10", "icd9", "character")
      )
    )
  )
  expect_true(
    icd_classes_conflict(
      structure("V10",
        class = c("icd10", "icd9cm", "character")
      )
    )
  )
  expect_true(
    icd_classes_conflict(
      structure(list("V10", "A20"),
        class = c("icd9cm", "icd10", "list")
      )
    )
  )
})

test_that("we can't set a data.frame to have a vector class", {
  expect_error(as.icd9(vermont_dx))
  expect_error(as.icd9cm(vermont_dx))
  expect_error(as.icd10(uranium_pathology))
  expect_error(as.icd10cm(uranium_pathology))
})

test_that("long vs wide data conflict identified", {
  v_bad <- vermont_dx
  class(v_bad) <- c(class(v_bad), "icd_long_data")
  u_bad <- uranium_pathology
  class(u_bad) <- c(class(u_bad), "icd_wide_data")
  expect_true(icd_classes_conflict(v_bad))
  expect_true(icd_classes_conflict(u_bad))
})

test_that("can create NA valued ICD code types", {
  expect_true(is.na(as.icd9(NA)))
  expect_true(is.na(as.icd9cm(NA)))
  expect_true(is.na(as.icd10(NA)))
  expect_true(is.na(as.icd10cm(NA)))
})
test_that("icd9 procedure code classes look good", {
  expect_false(icd_conflicts.icd9cm_pc(as.icd9cm_pc("0016073")))
  expect_true(icd_conflicts.icd9cm_pc(as.icd9("A00")))
  expect_true(icd_conflicts.icd9cm_pc(as.icd9cm("A00")))
  expect_identical(icd9cm_pc("001"), as.icd9cm_pc("001"))
  expect_error(as.icd9cm_pc(as.icd9cm("A00")))
})
test_that("icd10 procedure code classes look good", {
  expect_false(icd_conflicts.icd10cm_pc(as.icd10cm_pc("0016073")))
  expect_true(icd_conflicts.icd10cm_pc(as.icd10("A00")))
  expect_true(icd_conflicts.icd10cm_pc(as.icd10cm("A00")))
  expect_true(icd_conflicts.icd10cm_pc(as.icd10who("A00")))
  expect_identical(icd10cm_pc("001"), as.icd10cm_pc("001"))
  expect_true(is.icd10cm_pc(icd10cm_pc("001")))
  expect_true(is.icd10cm_pc(as.icd10cm_pc("001")))
  expect_error(as.icd10cm_pc(as.icd10cm("A00")))
})
test_that("icd10 WHO code classes look good", {
  expect_false(icd_conflicts.icd10who(as.icd10who("A00")))
  expect_true(icd_conflicts.icd10cm_pc(as.icd10("A00")))
  expect_true(icd_conflicts.icd10who(as.icd10cm_pc("001")))
  expect_true(icd_conflicts.icd10cm(as.icd10who("A00")))
  expect_true(is.icd10who(icd10who("Z998")))
  expect_true(is.icd10who(as.icd10who("Z998")))
  expect_false(is.icd10who(as.icd10cm("Z998")))
})
test_that("icd10-fr France code classes look good", {
  expect_false(icd_conflicts.icd10fr(as.icd10fr("A00")))
  expect_true(icd_conflicts.icd10cm_pc(as.icd10("A00")))
  expect_true(icd_conflicts.icd10fr(as.icd10cm_pc("001")))
  expect_true(icd_conflicts.icd10fr(as.icd10who("A00")))
  expect_true(is.icd10fr(icd10fr("Z998")))
  expect_true(is.icd10fr(as.icd10fr("Z998")))
  expect_false(is.icd10fr(as.icd10cm("Z998")))
})
