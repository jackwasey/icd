context("condense ranges of codes to parents and orphans")

test_that("five digit to five digit code range condenses", {
  expect_equal(condense("34500" %i9sa% "34509", defined = FALSE), "3450")
})

test_that("condensing a single real codes gives themselves", {
  expect_message(res <- condense("61172"))
  expect_equal(res, "61172")
  expect_equal(condense("61172", defined = FALSE), "61172")
  expect_equal(condense("143"), "143")
  expect_equal(condense("143", defined = FALSE), "143")
  expect_equal(condense("V1221"), "V1221")
  expect_equal(condense("V1221", defined = FALSE), "V1221")
  expect_equal(condense("V121"), "V121")
  expect_equal(condense("V121", defined = FALSE), "V121")
  expect_equal(condense("E8280"), "E8280")
  expect_equal(condense("E8280", defined = FALSE), "E8280")
  expect_equal(condense("E030"), "E030")
  expect_equal(condense("E030", defined = FALSE), "E030")
})

test_that("dispatch character vector for condense", {
  expect_equal(condense("E8280"), "E8280")
  expect_equal(
    sort(condense(as.character("98799" %i9sa% "98901"), defined = FALSE)),
    sort(c("98799", "988", "98900", "98901"))
  )
})

test_that("condense an ICD-9 code set to minimal group", {
  expect_equal(
    sort(condense("98799" %i9sa% "98901", defined = FALSE)),
    sort(c("98799", "988", "98900", "98901"))
  )
  # non-real end of real range
  res <- expect_warning(condense("988" %i9sa% "98899", defined = TRUE), regexp = NULL)
  expect_equal(res, "988")
  res <- expect_warning(condense("9879" %i9sa% "9891", defined = TRUE), regexp = NULL)
  expect_equal(res, c("9879", "988", "9890", "9891"))
})

test_that("condense ranges which do consense", {
  expect_equal(
    condense(children("123", short_code = TRUE, defined = TRUE), defined = TRUE),
    "123"
  )
  expect_equal(
    condense(children("1", short_code = TRUE, defined = TRUE), defined = TRUE),
    "001"
  )
  expect_equal(condense(children("123", short_code = TRUE)), "123")
  expect_equal(condense(children("1", short_code = TRUE)), "001")
  for (or1 in c(TRUE, FALSE)) {
    for (or2 in c(TRUE, FALSE)) {
      expect_equal(
        condense(children("00321", short_code = TRUE, defined = or1), defined = or2),
        "00321",
        info = paste("or1,2 are: ", or1, ", ", or2)
      )
      expect_equal(
        condense(children("V1221", short_code = TRUE, defined = or1), defined = or2),
        "V1221",
        info = paste("or1,2 are: ", or1, ", ", or2)
      )
    }
  }
  expect_equal(
    condense(children("V12", short_code = TRUE, defined = TRUE),
      defined = TRUE
    ), "V12"
  )
  expect_equal(
    condense(children("V12", short_code = TRUE, defined = FALSE),
      defined = FALSE
    ), "V12"
  )
})

test_that("condense ranges that don't condense at all", {
  expect_equal(
    sort(condense(c("1230", "1232", "1236"), defined = FALSE)),
    c("1230", "1232", "1236")
  )
  expect_equal(
    sort(condense(c("1230", "1232", "1236"), defined = TRUE)),
    c("1230", "1232", "1236")
  )
  # missing 10009
  expect_equal(
    sort(condense(c("1000", as.character(10000:10008)),
      defined = FALSE
    )),
    c("1000", as.character(10000:10008))
  )
})

test_that("condense range invalid data", {
  expect_equal(condense("turnpike", defined = FALSE), character(0))
  expect_equal(condense(c("turnpike", "road"), defined = FALSE), character(0))
  expect_equal(condense(c(""), defined = FALSE), character(0))
  expect_equal(condense(c("", ""), defined = FALSE), character(0))
  expect_equal(condense(c(NA_character_), defined = FALSE), character(0))
  expect_equal(condense(c(NA_character_, ""), defined = FALSE), character(0))
  # one valid vode with invalids
  expect_equal(condense(c("NA", "rotem", "123"), defined = FALSE), "123")
})

test_that("mix of four and five digit billable codes", {

  # this is all of leptospirosis, but missing the "1008" non-billable sub-heading
  expect_equal(
    condense(short_code = TRUE, c("1000", "10081", "10089", "1009")),
    "100"
  )
})

test_that("mix of four and five digit billable codes over bigger range", {

  # this is all of leptospirosis, but missing the "1008" non-billable sub-heading
  expect_equal(
    condense(short_code = TRUE, c("1000", "10081", "10089", "1009", "101")),
    c("100", "101")
  )
})


test_that("mix of four and five digit with non-billable mid-level four digit code", {
  expect_equal(
    condense(short_code = TRUE, c("1000", "1008", "10081", "10089", "1009")),
    "100"
  )
})

test_that("condense short range", {
  expect_equal(explain_code.icd9(othersalmonella), "Other salmonella infections")

  expect_equal(condense(short_code = TRUE, othersalmonella, defined = TRUE), "003")
  expect_warning(
    res <- condense(short_code = TRUE, othersalmonella, defined = FALSE),
    regexp = NA
  )
  expect_equal(res, othersalmonella)
  # missing this leaf node, we can't condense at all
  expect_equal(
    condense(short_code = TRUE, othersalmonella[-3], defined = TRUE),
    othersalmonella[-3]
  )
  # if we demand condensing to all possible values, we get the same back
  expect_equal(
    condense(short_code = TRUE, othersalmonella[-3], defined = FALSE),
    othersalmonella[-3]
  )

  expect_equal_no_icd(
    sort(children(short_code = TRUE, "001", billable = TRUE)),
    c("0010", "0011", "0019")
  )

  expect_equal_no_icd(
    sort(children(short_code = TRUE, x = "001", defined = TRUE)),
    c("001", "0010", "0011", "0019")
  )

  expect_equal(
    condense(
      short_code = TRUE,
      children(short_code = TRUE, "00320", defined = TRUE),
      defined = TRUE
    ), "00320"
  )
  # majors should be okay, even if not 'real'
  expect_warning(
    dup_res <- condense(
      short_code = TRUE,
      children(short_code = TRUE, "003", defined = TRUE)
    ),
    regexp = NA
  )

  expect_equal(condense(short_code = TRUE, c("003", "003"), defined = TRUE), "003")
  expect_equal(condense(short_code = TRUE, c("003", "003"), defined = FALSE), "003")
})

test_that("condense full ranges", {

  # condensing to "real" means we don't get a lot of majors, which are often not
  # themselves defined.
  # majors:
  expect_equal(
    condense(
      short_code = TRUE,
      children(short_code = TRUE, "003", defined = FALSE),
      defined = FALSE
    ),
    "003"
  )
  expect_equal(
    condense(
      short_code = TRUE,
      children(short_code = TRUE, "3", defined = FALSE),
      defined = FALSE
    ),
    "003"
  )
  expect_equal(
    condense(
      short_code = TRUE,
      children(short_code = TRUE, "410", defined = FALSE),
      defined = FALSE
    ),
    "410"
  )
  expect_equal(
    condense(
      short_code = TRUE,
      children(short_code = TRUE, "V12", defined = FALSE),
      defined = FALSE
    ),
    "V12"
  )
  expect_equal(
    condense(
      short_code = TRUE,
      children(short_code = TRUE, "E800", defined = FALSE),
      defined = FALSE
    ),
    "E800"
  )
  # repeat some tests with decimals instead
  expect_equal_no_icd(condense(
    short_code = FALSE,
    children("003", short_code = FALSE, defined = FALSE), defined = FALSE
  ), "003")
  expect_equal_no_icd(condense(
    children(short_code = FALSE, "3", defined = FALSE),
    short_code = FALSE, defined = FALSE
  ), "003")
  expect_equal_no_icd(condense(
    short_code = FALSE,
    children(short_code = FALSE, "410", defined = FALSE),
    defined = FALSE
  ), "410")
  expect_equal_no_icd(condense(
    short_code = FALSE,
    children("V12", short_code = FALSE, defined = FALSE),
    defined = FALSE
  ), "V12")
  expect_equal_no_icd(condense(
    short_code = FALSE,
    children(short_code = FALSE, "E800", defined = FALSE),
    defined = FALSE
  ), "E800")
  # repeat some tests with decimals and smaller codes
  expect_equal_no_icd(
    condense(
      short_code = FALSE,
      children("003.2", short_code = FALSE, defined = FALSE),
      defined = FALSE
    ),
    "003.2"
  )
  expect_equal_no_icd(
    condense(
      children(short_code = FALSE, "3.2", defined = FALSE),
      short_code = FALSE,
      defined = FALSE
    ),
    "003.2"
  )
  expect_equal_no_icd(condense(
    short_code = FALSE,
    children(short_code = FALSE, "410.0", defined = FALSE),
    defined = FALSE
  ), "410.0")
  expect_equal_no_icd(condense(
    short_code = FALSE,
    children("V12", short_code = FALSE, defined = FALSE),
    defined = FALSE
  ), "V12")
  expect_equal_no_icd(condense(
    short_code = FALSE,
    children(short_code = FALSE, "E800", defined = FALSE),
    defined = FALSE
  ), "E800")

  expect_equal(condense(
    short_code = TRUE,
    children(short_code = TRUE, "0031", defined = FALSE),
    defined = FALSE
  ), "0031")
  expect_equal(
    condense(short_code = TRUE, othersalmonella, defined = FALSE),
    othersalmonella
  )
  # now do we fining major if all chilren present?
  almostall003 <- children(short_code = TRUE, "003", defined = FALSE)
  almostall003 <- almostall003[almostall003 != "003"] # drop the major
  expect_equal(condense(short_code = TRUE, almostall003, defined = FALSE), "003")

  expect_equal(
    condense(
      short_code = TRUE, children(
        short_code = TRUE, "0031",
        defined = FALSE
      ),
      defined = FALSE
    ), "0031"
  )
  # gives nothing back if a non-billable code provided, but billable requested

  # billable describes input, it doesn't make any sense to describe output when
  # condensing.
  expect_equal(
    condense(short_code = TRUE, c("003", othersalmonella), defined = TRUE),
    "003"
  )
  # major is returned
  expect_equal(
    condense(short_code = TRUE, othersalmonella, defined = TRUE),
    "003"
  )
  expect_equal(
    condense(short_code = TRUE, othersalmonella, defined = FALSE),
    othersalmonella
  )
  # now do we find a missing major if all chilren present?
  almostall003 <- children("003", short_code = TRUE, defined = FALSE)
  almostall003 <- almostall003[almostall003 != "003"] # drop the major
  expect_equal(
    condense(short_code = TRUE, almostall003, defined = FALSE),
    "003"
  )
})

test_that("condense single major and its children", {
  expect_equal(condense(short_code = TRUE, "003"), "003")
  rheum_fever <- "Rheumatic fever with heart involvement"
  expect_equal(explain_code.icd9("391"), rheum_fever)
  expect_equal(
    explain_code.icd9(children("391", short_code = TRUE)), rheum_fever
  )
  expect_equal(
    explain_code.icd9(children("391", short_code = TRUE, defined = TRUE)),
    rheum_fever
  )
})

test_that("condense a factor of codes instead of character vector", {
  vdat <- unique(vermont_dx$DX1)[1:10]
  # this is not a condensable list
  dat <- as.factor(vdat)
  dat2 <- condense(short_code = TRUE, defined = TRUE, dat)
  expect_true(all(intersect(dat, dat2) == dat))
})

test_that("levels are preserved from source factor", {
  vdat <- unique(vermont_dx$DX1)[1:10]
  dat <- factor(vdat, levels = c("plastic", vdat))
  expect_warning(
    dat3 <- condense(dat,
      short_code = TRUE,
      defined = TRUE,
      keep_factor_levels = TRUE
    )
  )
  expect_warning(
    dat4 <- condense(dat,
      short_code = TRUE,
      defined = TRUE, keep_factor_levels = FALSE
    )
  )
  expect_identical(intersect(dat, dat3), vdat)
  expect_identical(intersect(dat, dat4), vdat)
})
