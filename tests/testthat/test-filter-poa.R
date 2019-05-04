context("filtering on POA")

test_that("filter POA - not a data frame", {
  expect_error(filter_poa_no(list(pollo = "loco")))
})

test_that("filter POA - no poa field", {
  expect_error(filter_poa_yes(simple_poa_pts[1:2]))
})

test_that("filter POA - generic func - invalid poa type", {
  expect_error(filter_poa(
    x = simple_poa_pts,
    poa_name = "poa", poa = "not an option"
  ))
  expect_error(filter_poa(
    x = simple_poa_pts,
    poa_name = "poa", poa = ""
  ))
  expect_error(filter_poa(
    x = simple_poa_pts,
    poa_name = "poa", poa = NA
  ))
})

test_that("filter POA - wrong name poa field", {
  pd <- simple_poa_pts
  names(pd) <- c("visit_id", "icd9", "achilleus")
  expect_error(filter_poa_yes(pd, poa_name = "poa"))
  expect_error(filter_poa_yes(pd, poa_name = "odysseus"))
  expect_error(filter_poa_yes(pd))
})

test_that("filter POA - poa is factor", {
  # POA flag is an obvious case for using factors. Not sure if it saves much
  # memory, and it certainly risks screwing up the analysis with obscure and
  # difficult to debug errors. ICD-9 code is also factor fodder, and likely to
  # be highly repeated over millions of patients, but I've resisted its charms
  # thus far.

  # just within this closure
  simple_poa_pts$poa <- factor(simple_poa_pts$poa)
  names(simple_poa_pts)[3] <- "poa"
  # just within this closure
  complex_poa_pts$poa <- factor(complex_poa_pts$poa)
  names(complex_poa_pts)[3] <- "poa"

  # row names are preserved here: probably not important, but a little annoying
  expect_identical(
    filter_poa_yes(simple_poa_pts),
    simple_poa_pts[1, 1:2]
  )
  expect_identical(
    filter_poa_not_yes(simple_poa_pts),
    simple_poa_pts[-1, 1:2]
  )
  expect_identical(
    filter_poa_no(simple_poa_pts),
    simple_poa_pts[2, 1:2]
  )
  expect_identical(
    filter_poa_not_no(simple_poa_pts),
    simple_poa_pts[-2, 1:2]
  )

  expect_identical(
    filter_poa_yes(complex_poa_pts),
    complex_poa_pts[1, 1:2]
  )
  expect_identical(
    filter_poa_not_yes(complex_poa_pts),
    complex_poa_pts[-1, 1:2]
  )
  expect_identical(
    filter_poa_no(complex_poa_pts),
    complex_poa_pts[2, 1:2]
  )
  expect_identical(
    filter_poa_not_no(complex_poa_pts),
    complex_poa_pts[-2, 1:2]
  )
})

test_that("filter POA - poa is vector", {
  expect_identical(
    filter_poa_yes(simple_poa_pts),
    simple_poa_pts[1, 1:2]
  )
  expect_identical(
    filter_poa_not_yes(simple_poa_pts),
    simple_poa_pts[-1, 1:2]
  )
  expect_identical(
    filter_poa_no(simple_poa_pts),
    simple_poa_pts[2, 1:2]
  )
  expect_identical(
    filter_poa_not_no(simple_poa_pts),
    simple_poa_pts[-2, 1:2]
  )

  expect_identical(
    filter_poa_yes(complex_poa_pts),
    complex_poa_pts[1, 1:2]
  )
  expect_identical(
    filter_poa_not_yes(complex_poa_pts),
    complex_poa_pts[-1, 1:2]
  )
  expect_identical(
    filter_poa_no(complex_poa_pts),
    complex_poa_pts[2, 1:2]
  )
  expect_identical(
    filter_poa_not_no(complex_poa_pts),
    complex_poa_pts[-2, 1:2]
  )

  # same via core function
  expect_identical(
    filter_poa(complex_poa_pts, poa = "yes"),
    complex_poa_pts[1, 1:2]
  )
  expect_identical(
    filter_poa(complex_poa_pts, poa = "notYes"),
    complex_poa_pts[-1, 1:2]
  )
  expect_identical(
    filter_poa(complex_poa_pts, poa = "no"),
    complex_poa_pts[2, 1:2]
  )
  expect_identical(
    filter_poa(complex_poa_pts, poa = "notNo"),
    complex_poa_pts[-2, 1:2]
  )
})

test_that("filter POA - poa upper and lower case", {
  smpl <- simple_poa_pts
  smpl[["poa"]] <- c("Y", "n", "e", NA)
  expect_identical(filter_poa_no(smpl), filter_poa_no(simple_poa_pts))
})

test_that("filter POA - just Y and N should be complementary", {
  # take any data frame to start out:
  dfrm <- test_twenty
  dfrm <- dfrm[dfrm[["poa"]] %in% c("Y", "N", "y", "n"), ]
  expect_identical(filter_poa_no(dfrm), filter_poa_not_yes(dfrm))
  expect_identical(filter_poa_yes(dfrm), filter_poa_not_no(dfrm))
})
