context("test trim, other utils")

test_that("trim with empty inputs" , {
  expect_equal(trim(character(0)), character(0))
  expect_equal(trim(""), "")
})

test_that("trim with acceptable inputs" , {
  expect_equal(trim(NA_character_), NA_character_)
  # not necesarily desirable, but this states the expectation.
  expect_equal(trim(NA_integer_), NA_character_)
  expect_equal(trim("jack"), "jack")
  expect_equal(trim("jack "), "jack")
  expect_equal(trim(" jack "), "jack")
  expect_equal(trim(" jack"), "jack")
  expect_equal(trim("jack\t"), "jack")
  expect_equal(trim("\tjack\t"), "jack")
  expect_equal(trim("\tjack"), "jack")
  expect_equal(trim("ja ck"), "ja ck")
  expect_equal(trim("ja ck "), "ja ck")
  expect_equal(trim(" ja ck "), "ja ck")
  expect_equal(trim(" ja ck"), "ja ck")

  expect_equal(trim(c(" ja ck\t", "hayley ")),
               c("ja ck", "hayley"))

  # trim and trimCpp should do the same thing, for non-NA
  expect_identical(trim(c("123", " a s d f ")),
                   trimCpp(c("123", " a s d f ")))

  # strim (trim a single string)
  expect_equal(trim(NA_integer_), NA_character_)
  expect_equal(trim(NA_character_), NA_character_)
  expect_equal(strim("jack"), "jack")
  expect_equal(strim("jack "), "jack")
  expect_equal(strim(" jack "), "jack")
  expect_equal(strim(" jack"), "jack")
  expect_equal(strim("jack\t"), "jack")
  expect_equal(strim("\tjack\t"), "jack")
  expect_equal(strim("\tjack"), "jack")
  expect_equal(strim("ja ck"), "ja ck")
  expect_equal(strim("ja ck "), "ja ck")
  expect_equal(strim(" ja ck "), "ja ck")
  expect_equal(strim(" ja ck"), "ja ck")

  # strim and strimCpp should do the same thing, for non-NA
  expect_identical(strim(" a s d f "), strimCpp(" a s d f "))

})
