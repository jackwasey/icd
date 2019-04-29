context("WHO")

test_that("No ranges or NA in code section of WHO data", {
  skip_multi()
  skip_missing_dat("icd10who2016")
  skip_missing_dat("icd10who2008fr")
  for (nm in c("icd10who2016", "icd10who2008fr")) {
    if (!.exists_in_cache(nm)) {
      skip(paste(nm, "not available for testing."))
    }
    i <- get_icd_data(nm)
    # structure tests
    expect_true(is.data.frame(i), info = nm)
    expect_false(any(grepl("-", i$code)), info = nm)
    expect_false(any(is.na(i$code)), info = nm)
    expect_false(any(is.na(i$leaf)), info = nm)
    expect_false(any(is.na(i$desc)), info = nm)
    expect_false(any(is.na(i$three_digit)), info = nm)
    expect_false(any(is.na(i$major)), info = nm)
    # sub_sub_chapter may be NA
    expect_false(any(is.na(i$sub_chapter)), info = nm)
    expect_false(any(is.na(i$chapter)), info = nm)
    # data tests
    expect_true(!anyDuplicated(i$code), info = nm)
  }
})

test_that("HIV is problematic for some reason", {
  skip_multi()
  skip_missing_dat("icd10who2016")
  b21 <- .dl_icd10who_children("B21")
  expect_match(b21$ID, "^B21\\.[0-9]$")
})
