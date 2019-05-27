context("explain in table format")

test_that("with and without condense returns correct structure", {
  codes <- c("362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414")
  expect_equal(dim(en <- explain_table(codes, condense = FALSE)), c(7, 11))
  expect_equal(dim(ec <- explain_table(codes, condense = TRUE)), c(6, 13))
  expect_equal(
    names(ec)[c(12, 13)],
    c("condensed_codes", "condensed_num")
  )
  expect_true(all(is_valid(en$code) == en$valid_icd9))
})

test_that("valid ICD-9 codes are valid and row numbers ok", {
  v <- c(vermont_dx$DX1, vermont_dx$DX2)
  vt <- explain_table(v)
  expect_true(all(is_valid(vt$code) == vt$valid_icd9))
  expect_equal(rownames(vt), as.character(seq_along(vt$code)))
})

test_that("valid ICD-10 codes are valid and row numbers ok", {
  v <- unlist(unname(icd10_map_pccc_dx))
  vt <- explain_table(v)
  expect_true(all(is_valid(vt$code) == vt$valid_icd10))
  expect_equal(rownames(vt), as.character(seq_along(vt$code)))
})

test_that("reproduces explain_code.list, mixed major and minor", {
  codes <- c("362.5", "413.9", "010.02", "584.9", "357.2", "588.81", "010")
  method1 <- explain_code(codes, condense = FALSE)
  method2 <- explain_table(codes, condense = FALSE)[["long_desc"]]
  expect_identical(method1, method2)
})

test_that("handle invalid icd by filling with NAs.", {
  res <- explain_table("Rick Shaw", condense = TRUE)
  expect_true(is.na(res[["short_desc"]]))
  expect_true(is.na(res[["is_major"]])) # could be false?
  expect_equal(res[["condensed_codes"]], res[["code"]])
  expect_equal(res[["condensed_num"]], 1L)

  res <- explain_table(c("bogus code", "Percy Veer"), condense = TRUE)
  expect_true(all(is.na(res[["short_desc"]])))
  expect_true(all(is.na(res[["is_major"]])))
  expect_equal(res[["condensed_codes"]], res[["code"]])
  expect_equal(res[["condensed_num"]], c(1L, 1L))
})

test_that("condensing explain_table generates correct columns", {
  dat <- data.frame(
    code = c("123", "123.4"),
    three_digit = factor(c("123", "123")),
    stringsAsFactors = FALSE
  )

  expect_identical(
    condense_explain_table_worker(dat),
    structure(
      list(
        code = structure(1L, .Label = "123", class = "factor"),
        condensed_codes = "123, 123.4", condensed_num = 2L
      ),
      .Names = c("three_digit", "condensed_codes", "condensed_num"),
      row.names = c(NA, -1L), class = "data.frame"
    )
  )
  expect_identical(condense_explain_table_worker(dat)$condensed_num, 2L)

  dat <- data.frame(
    code = c("123", "123.4"),
    three_digit = factor(c("123", "123")),
    stringsAsFactors = FALSE
  )
  expect_identical(condense_explain_table_worker(dat)$condensed_num, 2L)

  dat <- data.frame(
    code = c("123.3", "123.4"),
    three_digit = factor(c("123", "123")),
    stringsAsFactors = FALSE
  )
  expect_identical(condense_explain_table_worker(dat)$condensed_num, 2L)

  dat <- data.frame(
    code = c("789.0", "123.4"),
    three_digit = factor(c("789", "123")),
    stringsAsFactors = FALSE
  )
  res <- condense_explain_table_worker(dat)
  expect_is(res$condensed_num, "integer")
  expect_identical(dim(res), c(0L, 3L))

  dat <- data.frame(
    code = c("radish", "feral"),
    three_digit = factor(c("goniatite", "slalom")),
    stringsAsFactors = FALSE
  )
  res <- condense_explain_table_worker(dat)
  expect_is(res$condensed_num, "integer")
  expect_identical(dim(res), c(0L, 3L))

  # NA codes are dropped
  dat <- data.frame(
    code = c("radish", NA),
    three_digit = factor(c("goniatite", NA)),
    stringsAsFactors = FALSE
  )
  expect_identical(dim(condense_explain_table_worker(dat)), c(0L, 3L))

  dat <- data.frame(
    code = c("carvedilol", NA),
    three_digit = factor(c(NA, NA)),
    stringsAsFactors = FALSE
  )
  expect_identical(nrow(condense_explain_table_worker(dat)), 0L)
})

test_that("explain_table num_condense sum after condense equals input length", {
  variations <- list(
    c("362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414"),
    c(
      "362.5", "413.9", "414.01", "584.9", "357.2", "588.81", "414",
      "bogus code"
    ),
    "another bogus",
    c("one bogus", "two bogus")
  )
  for (codes in variations) {
    res <- explain_table(codes, condense = TRUE)
    expect_is(res[["condensed_num"]], "integer")
    expect_equal(
      sum(res$condensed_num, na.rm = TRUE),
      length(codes),
      info = paste("codes", codes, collapse = " ")
    )
  }
})

test_that("appropriately convert mixed code character vector,
          casted icd9, and casted icd10 vectors:", {
  codes <- c("N18.3", "414", "362.5")
  res <- explain_table(as.icd9(codes))
  expect_equal(is.na(res$short_desc), c(TRUE, FALSE, FALSE))

  res <- explain_table(icd10(codes))
  expect_equal(is.na(res$short_desc), c(FALSE, TRUE, TRUE))
})

test_that("explain_table works with factor input", {
  f <- factor(c("25010", "E777", "lorem ipsum"))
  expect_equal(dim(explain_table(f)), c(3, 11))
  expect_equal(explain_table(f)[2, 3], NA_character_)
})

test_that("same columns returned for ICD-9 and ICD-10 codes", {
  expect_identical(
    colnames(
      explain_table(
        as.icd9(
          c("E879", "E932", "E915", "E947", "E939", "E911", "E928")
        )
      )
    ),
    colnames(
      explain_table(
        as.icd10(
          c("M97", "V07", "E934")
        )
      )
    )
  )
})
