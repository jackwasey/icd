context("icd9cm_hierarchy was parsed as expected")
# at present, icd9cm_hierarchy is derived from RTF parsing, a little
# web scraping, some manually entered data, and (for the short description only)
# another text file parsing.`

test_that("icd9cm_hierarchy as saved in data can be recreated as expected", {
  # avoid encoding problems by just doing this on Linux.
  skip_on_os(c("windows", "mac", "solaris"))
  skip_flat_icd9_avail()
  skip_on_no_rtf("2011")
  cmh_headings <- c(
    "code",
    "short_desc",
    "long_desc",
    "three_digit",
    "major",
    "sub_chapter",
    "chapter"
  )
  cmh <- .parse_icd9cm_hierarchy_rtf(
    save_pkg_data = FALSE,
    verbose = FALSE
  )
  for (h in cmh_headings)
    expect_equal(cmh[[h]],
      icd9cm_hierarchy[[h]],
      info = paste("working on :", h)
    )
})

test_that("no NA or zero-length values", {
  expect_false(any(vapply(icd9cm_hierarchy,
    function(x) any(is.na(x)),
    FUN.VALUE = logical(1)
  )))
  expect_false(any(nchar(unlist(icd9cm_hierarchy)) == 0))
})

test_that("factors are in the right place", {
  expect_is(
    icd9cm_hierarchy[["code"]],
    c("icd9cm", "icd9", "character")
  )
  expect_is(icd9cm_hierarchy$short_desc, "character")
  expect_is(icd9cm_hierarchy$long_desc, "character")
  expect_is(icd9cm_hierarchy$three_digit, "factor")
  expect_is(icd9cm_hierarchy$major, "factor")
  expect_is(icd9cm_hierarchy$sub_chapter, "factor")
  expect_is(icd9cm_hierarchy$chapter, "factor")
})

test_that("codes and descriptions are valid and unique", {
  expect_equal(anyDuplicated(icd9cm_hierarchy[["code"]]), 0)
  expect_true(all(icd::is_valid(icd9cm_hierarchy[["code"]])))
})

test_that("some chapters are correct", {
  chaps <- as_char_no_warn(icd9cm_hierarchy$chapter)
  expect_identical(unique(chaps), levels(icd9cm_hierarchy$chapter))
  codes <- icd9cm_hierarchy[["code"]]
  # first and last rows (E codes should be last)
  expect_equal(chaps[1], "Infectious And Parasitic Diseases")
  expect_equal(
    chaps[nrow(icd9cm_hierarchy)],
    "Supplementary Classification Of External Causes Of Injury And Poisoning"
  )
  # first and last rows of a block in the middle
  neoplasm_first_row <- which(codes == "140")
  neoplasm_last_row <- which(codes == "240") - 1
  expect_equal(
    chaps[neoplasm_first_row - 1],
    "Infectious And Parasitic Diseases"
  )
  expect_equal(chaps[neoplasm_first_row], "Neoplasms")
  expect_equal(chaps[neoplasm_last_row], "Neoplasms")
  expect_equal(
    chaps[neoplasm_last_row + 1],
    "Endocrine, Nutritional And Metabolic Diseases, And Immunity Disorders"
  )
})

test_that("some sub-chapters are correct", {
  subchaps <- as_char_no_warn(icd9cm_hierarchy$sub_chapter)
  codes <- icd9cm_hierarchy[["code"]]

  # first and last
  expect_equal(subchaps[1], "Intestinal Infectious Diseases")
  expect_equal(
    subchaps[nrow(icd9cm_hierarchy)],
    "Injury Resulting From Operations Of War"
  )
  # first and last of a block in the middle of range E950 to E959
  psych_codes <- c(
    "E950", "E9500", "E9501", "E9502", "E9503", "E9504",
    "E9505", "E9506", "E9507", "E9508", "E9509", "E951", "E9510",
    "E9511", "E9518", "E952", "E9520", "E9521", "E9528", "E9529",
    "E953", "E9530", "E9531", "E9538", "E9539", "E954", "E955", "E9550",
    "E9551", "E9552", "E9553", "E9554", "E9555", "E9556", "E9557",
    "E9559", "E956", "E957", "E9570", "E9571", "E9572", "E9579",
    "E958", "E9580", "E9581", "E9582", "E9583", "E9584", "E9585",
    "E9586", "E9587", "E9588", "E9589", "E959"
  )
  suicide_rows <- which(codes %in% psych_codes)
  expect_equal(
    subchaps[suicide_rows[1] - 1],
    paste(
      "Drugs, Medicinal And Biological Substances",
      "Causing Adverse Effects In Therapeutic Use"
    )
  )
  expect_equal(subchaps[suicide_rows[1]], "Suicide And Self-Inflicted Injury")
  expect_equal(
    subchaps[suicide_rows[length(suicide_rows)]],
    "Suicide And Self-Inflicted Injury"
  )
  expect_equal(
    subchaps[suicide_rows[length(suicide_rows)] + 1],
    "Homicide And Injury Purposely Inflicted By Other Persons"
  )
})

test_that("some randomly selected rows are correct", {
  expect_equal(
    unname(
      vapply(
        icd9cm_hierarchy[
          icd9cm_hierarchy[["code"]] == "5060",
        ],
        FUN = as_char_no_warn, FUN.VALUE = character(1)
      )
    ),
    c(
      "5060", "TRUE", "Fum/vapor bronc/pneumon",
      "Bronchitis and pneumonitis due to fumes and vapors",
      "506", "Respiratory conditions due to chemical fumes and vapors",
      "Pneumoconioses And Other Lung Diseases Due To External Agents",
      "Diseases Of The Respiratory System"
    )
  )
})

test_that("tricky v91.9 works", {
  expect_equal(
    icd9cm_hierarchy[
      icd9cm_hierarchy[["code"]] == "V9192", "long_desc"
    ],
    "Other specified multiple gestation, with two or more monoamniotic fetuses"
  )
})
