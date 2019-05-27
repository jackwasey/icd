context("explain ICD-9: code to human-readable")

test_that("explain a large set of ICD-9 codes succinctly", {
  expect_setequal(
    explain_code(children(as.icd9("391"), defined = FALSE, short_code = TRUE),
      condense = FALSE, short_code = TRUE
    ),
    c(
      NA,
      "Rheumatic fever with heart involvement", "Acute rheumatic pericarditis",
      "Acute rheumatic endocarditis", "Acute rheumatic myocarditis",
      "Other acute rheumatic heart disease",
      "Acute rheumatic heart disease, unspecified"
    )
  )

  expect_identical(
    explain_code(children(as.short_diag(as.icd9("391"))),
      condense = TRUE, short_code = TRUE
    ),
    "Rheumatic fever with heart involvement"
  )
})
test_that("explain a large set of ICD-9 codes succinctly, using factors", {
  expect_setequal(
    explain_code(factor(children(as.icd9("391"),
      defined = FALSE,
      short_code = TRUE
    )),
    condense = FALSE, short_code = TRUE
    ),
    c(
      NA,
      "Rheumatic fever with heart involvement", "Acute rheumatic pericarditis",
      "Acute rheumatic endocarditis", "Acute rheumatic myocarditis",
      "Other acute rheumatic heart disease",
      "Acute rheumatic heart disease, unspecified"
    )
  )

  expect_identical(
    explain_code(factor(children(as.short_diag(as.icd9("391")))),
      condense = TRUE, short_code = TRUE
    ),
    "Rheumatic fever with heart involvement"
  )
})
test_that("github issue #41", {
  expect_equal(
    explain_code(get_defined(icd9_map_quan_deyo[["Dementia"]]), condense = TRUE),
    explain_code(icd9_map_quan_deyo[["Dementia"]], condense = TRUE, warn = FALSE)
  )
})
test_that("explain S3 dispatch", {
  expect_equal(
    explain_code(as.icd9("003.21"), short_code = FALSE),
    "Salmonella meningitis"
  )
  expect_equal(
    explain_code("003.21", short_code = FALSE),
    explain_code(as.icd9("003.21"), short_code = FALSE)
  )
  expect_equal(
    explain_code.list(list(a = "003.21"), short_code = FALSE),
    list(a = explain_code(as.icd9("00321"), short_code = TRUE))
  )
  expect_equal(
    explain_code(list(a = "003.21", b = "390"), short_code = FALSE),
    list(
      a = explain_code(as.icd9cm("00321"), short_code = TRUE),
      b = "Rheumatic fever without mention of heart involvement"
    )
  )
  expect_warning(res <- explain_code(c(a = "not", b = "icd9code"), short_code = TRUE))
  expect_equal(res, character())
  expect_warning(
    res <- explain_code(list(a = icd9("not"), b = icd9("icd9code")),
      short_code = FALSE
    )
  )
  expect_equal(res, list(a = character(), b = character()))

  expect_equal(explain_code("00321", short_code = TRUE), "Salmonella meningitis")
})
test_that("explain single top level code which is billable, has no children", {
  # the code "390" is a billable major: good test case.
  expect_identical(
    explain_code("390"),
    "Rheumatic fever without mention of heart involvement"
  )
})
test_that("expalin a single top level code without a top level explanation", {
  expect_identical(
    explain_code("391"),
    "Rheumatic fever with heart involvement"
  )
})
test_that("explain a single leaf node", {
  expect_equal(
    explain_code("27800", condense = FALSE),
    "Obesity, unspecified"
  )
  expect_equal(
    explain_code("27800", condense = TRUE),
    "Obesity, unspecified"
  )
  expect_equal(
    explain_code("00329"),
    "Other localized salmonella infections"
  )
})
test_that("explain handles mix of valid and invalid", {
  expect_equal(
    explain_code(c("radishes", "123"), warn = FALSE),
    "Other cestode infection"
  )
  expect_warning(explain_code(c("radishes", "123")))
})

test_that("explain works when none ICD-9 codes are even valid", {
  expect_equal(
    explain_code(c("radishes", "123123", NA), warn = FALSE, condense = TRUE),
    character()
  )
  expect_equal(
    explain_code(c("radishes", "123123", NA), warn = FALSE, condense = FALSE),
    c(NA_character_, NA_character_, NA_character_)
  )
})

test_that("guess icd9 types: short", {
  expect_true(guess_short("12345"))
  expect_true(guess_short(c("12345", "234")))
  # we only look at first one...
  expect_true(guess_short(c("12345", "23.4")))
  expect_true(guess_short("1234"))
})
test_that("guess icd9 types: decimal", {
  expect_false(guess_short("123.45"))
  expect_false(guess_short("123.4"))
  expect_false(guess_short("123."))
})
test_that("guess icd9 types: invalid", {
  expect_equal(guess_short(NA_character_), TRUE)
})
test_that("guess with just majors", {
  # it acutally doesn't matter if they are all majors, so we default to 'short'
  # which is usually the most direct route to an answer
  expect_true(guess_short(c("100", "101", "102")))
})
test_that("extract top-level codes from the RTF gives the complete list", {
  # total number of codes
  # all valid major form.

  # format of each hierarchy level:
  expect_equal(names(icd9_chapters[[1]]), c("start", "end"))
  expect_equal(names(icd9_sub_chapters[[1]]), c("start", "end"))
  # should be no NA values
  expect_true(all(!is.na(vapply(icd9_chapters, "[[", FUN.VALUE = "", 1))))
  expect_true(all(!is.na(vapply(icd9_chapters, "[[", FUN.VALUE = "", 2))))
  expect_true(all(!is.na(vapply(icd9_sub_chapters, "[[", FUN.VALUE = "", 1))))
  expect_true(all(!is.na(vapply(icd9_sub_chapters, "[[", FUN.VALUE = "", 2))))
  expect_true(all(!is.na(vapply(icd9_majors, "[[", FUN.VALUE = "", 1))))

  # all the range limits and majors should be valid majors
  expect_true(
    all(is_valid_major.icd9(vapply(icd9_chapters, "[[",
      FUN.VALUE = "", 1
    )))
  )
  expect_true(
    all(is_valid_major.icd9(vapply(icd9_chapters, "[[",
      FUN.VALUE = "", 2
    )))
  )
  expect_true(
    all(is_valid_major.icd9(vapply(icd9_sub_chapters, "[[",
      FUN.VALUE = "", 1
    )))
  )
  expect_true(
    all(is_valid_major.icd9(vapply(icd9_sub_chapters, "[[",
      FUN.VALUE = "", 2
    )))
  )
  expect_true(
    all(is_valid_major.icd9(vapply(icd9_majors, "[[",
      FUN.VALUE = "", 1
    )))
  )
})
test_that("icd9_majors - positive values", {
  expect_true("001" %in% icd9_majors)
  expect_true("V01" %in% icd9_majors)
  expect_true("V91" %in% icd9_majors) # only up to V89 in 2011.
  expect_true("E000" %in% icd9_majors)
  expect_true("E001" %in% icd9_majors)
  expect_true("E009" %in% icd9_majors)
  expect_true("E013" %in% icd9_majors)
  expect_true("E016" %in% icd9_majors)
  expect_true("E849" %in% icd9_majors)
  expect_true("E917" %in% icd9_majors)
})
test_that("icd9_majors - negative values", {
  # there are some gaps: just make sure we don't have any spurious codes:
  expect_false("888" %in% icd9_majors)
  expect_false("889" %in% icd9_majors)
  expect_false("V00" %in% icd9_majors)
  expect_false("V38" %in% icd9_majors)
  expect_false("E777" %in% icd9_majors)
})
for (i in list("icd9_chapters", "icd9_sub_chapters", "icd9_majors")) {
  il <- get(i)
  test_that(paste("icd9 chapters... duplication, blanks:", i), {
    expect_equal(anyDuplicated(names(il)), 0)
    expect_equal(length(il), length(unique(names(il))))
    expect_equal(anyDuplicated(il), 0)
    expect_equal(length(il), length(unique(il)))
    expect_true(all(nchar(names(il) > 0)))
    expect_false(anyNA(il))
    expect_false(anyNA(names(il)))
  })
}
test_that(
  "parse icd9_majors vs other CDC source of the leaf definitions.", {
    # get all the majors from the other list, to compare
    compare_majors <- unique(get_major.icd9(icd9cm_hierarchy[["code"]],
      short_code = TRUE
    ))
    expect_true(all(compare_majors %in% icd9_majors))
    expect_true(all(icd9_majors %in% compare_majors))
  }
)
test_that("unsorted hierarchy tests", {
  expect_equal(
    tolower(icd9cm_hierarchy[icd9cm_hierarchy$code == "00321", "long_desc"]),
    tolower("Salmonella Meningitis")
  )
})
test_that("explain gives appropriate warnings by default", {
  # if we ask for real codes, we should expect all real codes as input:
  expect_warning(condense.icd9("E7777", defined = TRUE, warn = TRUE))
})
test_that("working with named lists of codes, decimal is guessed", {
  expect_no_warn(explain_code(
    list(a = c("001"), b = c("001.1", "001.9")),
    short_code = FALSE
  ))
  expect_no_warn(explain_code(list(a = c("001"), b = c("001.1", "001.9"))))
})
test_that("github #113 for ICD-9 code 038.1", {
  expect_equal(explain_code("0381"), "Staphylococcal septicemia")
})
test_that("explaining a numeric value for icd9 gives warning", {
  expect_warning(explain_code(as.icd9(100)))
})
