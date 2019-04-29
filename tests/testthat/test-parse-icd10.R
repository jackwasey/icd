context("icd10 fixed width parse")

test_icd10_most_majors <- outer(LETTERS, sprintf(0:99, fmt = "%02i"), paste0)

test_that("icd10 flat file details are okay", {
  skip_slow("this test is very slow, may download data, but is important")
  skip_no_icd_data_resource()
  # check cols at a time, so I get better error feedback:
  col_names <- c(
    "code",
    "billable",
    "short_desc",
    "long_desc",
    "three_digit",
    "major",
    "sub_chapter",
    "chapter"
  )
  for (v in as.character(2014:2019)) {
    test_that(.get_icd10cm_name(v, TRUE), {
      f_info <- with_absent_action(
        "silent",
        .dl_icd10cm_year(year = v, dx = TRUE)
      )
      if (is.null(f_info)) skip(paste0("Skipping only icd10cm", v))
      res <- .parse_icd10cm_year(year = v, save_pkg_data = FALSE)
      expect_identical(colnames(res), col_names)
      expect_is(res$code, "character")
      expect_true(icd::is.icd10cm(res$code))
      expect_is(res$code, class = "icd10")
      expect_is(res$billable, "logical")
      expect_is(res$short_desc, "character")
      expect_is(res$long_desc, "character")
      for (n in c(
        "three_digit",
        "major",
        "sub_chapter",
        "chapter"
      )) {
        expect_true(is.factor(res[[n]]))
        expect_identical(res, get_icd10cm_version(v))
      }
    }) # outer test_that
  } # for all versions
})

# github issue #116
test_that("W02 is correctly parsed", {
  expect_equal_no_icd(icd::explain_code(icd::as.icd10cm("W02")), character(0))
})
