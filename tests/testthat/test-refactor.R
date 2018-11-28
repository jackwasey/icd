# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

context("refactor")

test_that("simple cases", {
  expect_identical(refactor(factor("a", levels = "a"), "a"),
                   factor("a", levels = "a"))
  expect_identical(refactor(factor(NA, levels = "a"), "a"),
                   factor(NA, levels = "a"))
  expect_identical(refactor(factor(NA, levels = "a"), "b"),
                   factor(NA, levels = "b"))
  expect_identical(refactor(factor(NA, levels = "a"), "a"),
                   factor(NA, levels = "a"))
  expect_identical(refactor(factor(c("a", "b")), "a"),
                   factor(c("a", NA), levels = "a"))
  expect_identical(refactor(factor(c("a", "b")), "b"),
                   factor(c(NA, "b"), levels = "b"))
  expect_identical(refactor(factor(c("a", "b")), "c"),
                   factor(c("a", "b"), levels = "c"))
})

test_that("simple na.rm cases", {
  expect_identical(refactor(factor("a", levels = "a"), "a", na.rm = TRUE),
                   factor("a", levels = "a"))
  expect_identical(refactor(factor(NA, levels = "a"), "a", na.rm = TRUE),
                   factor(c(), levels = "a"))
  expect_identical(refactor(factor(NA, levels = "a"), "b", na.rm = TRUE),
                   factor(c(), levels = "b"))
  expect_identical(refactor(factor(NA, levels = "a"), "a", na.rm = TRUE),
                   factor(c(), levels = "a"))
  expect_identical(refactor(factor(c("a", "b")), "a", na.rm = TRUE),
                   factor(c("a"), levels = "a"))
  expect_identical(refactor(factor(c("a", "b")), "b", na.rm = TRUE),
                   factor(c("b"), levels = "b"))
  expect_identical(refactor(factor(c("a", "b")), "c", na.rm = TRUE),
                   factor(c(), levels = "c"))
})

test_that("basic refactoring", {
  u <- c("a", "b", "c")
  v <- c("c", "d")
  w <- c(NA_character_, "a")
  x <- c("X", NA_character_)
  test_cases <- expand.grid(
    list(u, v, w, x),
    list(u, v, w, x),
    list(u, v, w, x))
  for (tc in seq_along(test_cases[[1]])) {
    m <- test_cases[tc, 1][[1]]
    n <- test_cases[tc, 2][[1]]
    p <- unique(test_cases[tc, 3][[1]])
    f <- factor(m, levels = p)
    f_res <- factor(f, levels = n)
    expect_identical(
      refactor(f, n, na.rm = TRUE, exclude_na = TRUE),
      f_res[!is.na(f_res)], # exclude NA by default, as factor does
      info = paste("m = c('", paste(unlist(m), collapse = "', '"), "')\n",
                   "n = c('", paste(unlist(n), collapse = "', '"), "')\n",
                   "p = c('", paste(p, collapse = "', '"), "')",
                   sep = "")
    )
    expect_identical(
      refactor(f, n, na.rm = FALSE, exclude_na = FALSE),
      factor(f, levels = n, exclude = NULL),
      info = paste("m = c('", paste(unlist(m), collapse = "', '"), "')\n",
                   "n = c('", paste(unlist(n), collapse = "', '"), "')",
                   "p = c(", paste(p, collapse = "', '"), "')",
                   sep = "")
    )
    if (!anyNA(f) && !anyNA(levels(f)))
      expect_identical(
        refactor(f, n, na.rm = FALSE, exclude_na = TRUE),
        f_res,
        info = paste("m = c('", paste(unlist(m), collapse = "', '"), "')\n",
                     "n = c('", paste(unlist(n), collapse = "', '"), "')",
                     "p = c(", paste(p, collapse = "', '"), "')",
                     sep = "")
      )
  }
})

test_that("new factor has empty levels when necessary", {
  f <- factor("a")
  expect_equal(
    refactor(f, levels = NA, na.rm = TRUE),
    factor())
  expect_equal(
    refactor(f, levels = NA, na.rm = FALSE, exclude_na = TRUE),
    factor(NA, levels = NULL))
  expect_equal(
    refactor(f, levels = NA, na.rm = FALSE, exclude_na = FALSE),
    factor(NA, exclude = NULL))
  for (narm in c(TRUE, FALSE)) {
    expect_error(info = paste("narm = ", narm), refactor(NA, NA, na.rm = narm))
    expect_error(regexp = NA, info = paste("narm = ", narm),
                 refactor(factor(NA), NA, na.rm = narm))
    expect_error(regexp = NA, info = paste("narm = ", narm),
                 refactor(factor(NA), "a", na.rm = narm))
    expect_error(regexp = NA, info = paste("narm = ", narm),
                 refactor(factor(NA, levels = NA), c("a", NA), na.rm = narm))
    expect_error(regexp = NA, info = paste("narm = ", narm),
                 refactor(factor(NA), c("a", NA), na.rm = narm))
    expect_error(regexp = NA, info = paste("narm = ", narm),
                 refactor(factor("a"), c("a", NA), na.rm = narm))
    expect_error(regexp = NA, info = paste("narm = ", narm),
                 refactor(factor("a"), NA, na.rm = narm))
  }
})

test_that("NA anywhere in middle of input levels ok", {
  # todo: duplicate target levels should fail or drop silenetly?
  v <- c("a", "b", NA)
  f_perms <- unname(expand.grid(v, v, v))
  l_perms <- unique(apply(f_perms, 1, unique))
  # loop orig values
  for (i in nrow(f_perms)) {
    # loop new levels
    for (ii in seq_along(l_perms)) {
      # loop old levels
      for (iii in seq_along(l_perms)) {
        for (narm in c(TRUE, FALSE)) {
          for (exna in c(TRUE, FALSE)) {
            w <- factor(f_perms[i, ], levels = l_perms[[iii]], exclude = NULL)
            l <- l_perms[[ii]]
            if (narm) {
              w <- w[!is.na(w)]
              l <- l[!is.na(l)]
            }
            ftt <- refactor(w, l, na.rm = narm, exclude_na = exna)
            if (exna)
              gtt <- factor(w, l, exclude = NA)
            else
              gtt <- factor(w, l, exclude = NULL)
            expect_identical(
              ftt, gtt,
              info = paste("i = ", i,
                           "; ii = ", ii,
                           "; iii = ", iii,
                           "; w = ", paste(w, collapse = ", "),
                           "; wlen = ", length(w),
                           "; l = ", paste(l, collapse = ", "),
                           "; llen = ", length(l),
                           ifelse(narm, "; narm=TRUE", "; narm=FALSE"),
                           ifelse(exna, "; exna=TRUE", "exna=FALSE")
              ))
          }
        }
      }
    }
  }
})

test_that("big bad factor frmo github133 test", {
  f <- structure(
    c(1L, 2L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,
      12L, 13L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 15L, 16L, 16L, 17L, 18L, 19L, 20L, 10L, 21L, 22L, 23L,
      24L, 25L, 26L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 27L, 28L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L,
      36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 46L, 47L, 47L, 48L, 49L, 50L, 51L, 52L, 53L,
      54L, 55L, 56L, 57L, 58L, 19L, 59L, 60L, 61L, 42L, 62L, 63L, 64L,
      65L, 66L, 67L, 20L, 68L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 69L, 69L, 69L, 70L, 52L, 71L, 61L, 64L,
      72L, 73L, 74L, 75L, 76L, 77L, 78L, 20L, 68L, 65L, 79L, 22L, 25L,
      80L, 81L, 82L, 83L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 84L, 84L, 84L, 85L, 86L, 40L, 23L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 87L, 88L, 88L, 89L, 90L, 87L,
      91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 101L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 102L, 102L, 102L,
      103L, 33L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      111L, 28L, 28L, 112L, 113L, 50L, 114L, 115L, 116L, 117L, 118L,
      119L, 55L, 120L, 121L, 122L, 123L, 56L, 124L, 52L, 125L, 126L,
      103L, 127L, 128L, 129L, 4L, 130L, 131L, 130L, 131L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 132L, 133L, 133L, 121L, 134L,
      120L, 33L, 76L, 34L, 135L, 136L, 137L, 19L, 138L, 139L, 44L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 140L,
      131L, 140L, 131L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      141L, 142L, 142L, 143L, 144L, 145L, 141L, 146L, 13L, 147L, 67L,
      80L, 93L, 148L, 149L, 99L, 150L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 151L, 152L, 152L, 153L, 154L, 155L, 156L,
      157L, 38L, 158L, 159L, 128L, 160L, 161L, 62L, 162L, 40L, 163L,
      164L, 165L, 166L, 64L, 167L, 61L, 135L, 168L, 169L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 170L,
      170L, 170L, 171L, 172L, 173L, 174L, 175L, 176L, 177L, 178L, 81L,
      179L, 111L, 180L, 181L, 25L, 64L, 86L, 182L, 183L, 184L, 185L,
      19L, 186L, 187L, 118L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 188L, 188L, 188L, 175L, 189L, 179L,
      58L, 183L, 78L, 190L, 40L, 64L, 191L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 192L, 131L, 192L,
      131L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 149L,
      193L, 193L, 87L, 194L, 76L, 40L, 195L, 196L, 61L, 41L, 197L,
      22L, 198L, 199L, 200L, 82L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 46L, 201L, 201L, 33L, 202L, 163L, 61L, 149L, 39L,
      203L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 204L, 205L, 205L, 55L, 206L, 62L,
      207L, 208L, 209L, 210L, 199L, 42L, 24L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 211L, 212L, 212L,
      17L, 213L, 80L, 64L, 149L, 211L, 214L, 215L, 216L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      217L, 131L, 217L, 131L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 218L, 55L, 55L, 33L, 219L, 38L, 39L, 220L, 212L, 221L, 20L,
      222L, 223L, 224L, 188L, 68L, 41L, 167L, 108L, 118L, 225L, 226L,
      190L, 23L, 227L, 228L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 229L, 229L, 229L, 52L, 230L, 19L,
      231L, 40L, 41L, 64L, 61L, 232L, 42L, 233L, 25L, 226L, 234L, 235L,
      236L, 24L, 65L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L),
    .Label = c(
      "R2243",
      "L03116", "R64", "Z681", "L52", "F79", "K121", "D539", "E876",
      "D473", "L219", "L0889", "D638", NA, "R079", "I4891", "I5031",
      "N184", "I2510", "I129", "G4730", "I252", "Z8673", "Z87891",
      "Z7982", "Z8249", "R509", "A419", "J9620", "Z930", "Z9911", "A047",
      "N390", "B952", "B961", "G808", "R6520", "E871", "E860", "I10",
      "K219", "D649", "Z79899", "Z888", "Z882", "R569", "G40401", "J9622",
      "I214", "G9340", "J9811", "I429", "I5040", "Z6841", "N179", "E872",
      "F10239", "I6523", "E1165", "E8351", "E039", "D696", "E8809",
      "E785", "E669", "G4733", "Z9114", "N183", "G312", "G40911", "I5022",
      "Z95810", "I25119", "I499", "Z952", "E119", "I6529", "F1010",
      "Z6830", "Z7901", "D500", "F419", "R339", "Z5189", "S065X0S",
      "R2681", "R45851", "F39", "F323", "F14259", "R45850", "F1421",
      "F17210", "F1210", "J45909", "Z21", "Z560", "Z915", "Z590", "Z91410",
      "Z658", "S72012A", "D62", "B965", "I9581", "R5082", "R000", "M810",
      "R945", "R197", "R001", "J9621", "I5033", "E43", "J90", "R6521",
      "Z515", "Z66", "R570", "I120", "N186", "K652", "I96", "K810",
      "E2740", "I2782", "I82512", "K922", "R188", "X58XXXA", "0", "S72102A",
      "S72145A", "I509", "F418", "I050", "Z992", "Z955", "I447", "W010XXA",
      "M86672", "M86272", "G8220", "M86271", "E441", "M86671", "Z86718",
      "B964", "F329", "Z905", "M549", "A4102", "G061", "J189", "K766",
      "I8500", "M6008", "E1151", "G9520", "M4626", "I484", "L4050",
      "I350", "M5011", "K7460", "I340", "M1990", "M479", "K439", "R55",
      "I81", "L89152", "D684", "D61818", "C228", "I1310", "M6282",
      "F10230", "D6959", "K7030", "N189", "Z9181", "D509", "D7589",
      "F4322", "Z951", "D731", "I951", "E8342", "R296", "S0181XA",
      "W06XXXA", "F3130", "L97519", "F909", "G43909", "Z86711", "G8929",
      "F4310", "Z794", "G40909", "F0390", "Z853", "I611", "I608", "I82612",
      "H53462", "H4010X0", "R2689", "R51", "R002", "I480", "I110",
      "T486X5A", "R0902", "Z23", "Y92230", "R1110", "J8410", "K222",
      "Z9981", "I701", "K529", "D631", "B9620", "Z881", "Z96641", "N289",
      "I442", "Z6839", "J449", "K449", "I739", "Z90710", "Z9049", "Z85828"
    ), class = "factor")

  f2 <- structure(
    c(14L, 14L, 211L, 212L, 212L, 17L, 213L, 80L, 64L,
      149L, 211L, 214L, 215L, 216L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 217L, 131L, 217L, 131L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 218L, 55L, 55L,
      33L, 219L, 38L, 39L, 220L, 212L, 221L, 20L, 222L, 223L, 224L,
      188L, 68L, 41L, 167L, 108L, 118L, 225L, 226L, 190L, 23L, 227L,
      228L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 229L, 229L, 229L, 52L, 230L, 19L, 231L, 40L, 41L,
      64L, 61L, 232L, 42L, 233L, 25L, 226L, 234L, 235L, 236L, 24L,
      65L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L),
    .Label = c(
      "R2243",
      "L03116", "R64", "Z681", "L52", "F79", "K121", "D539", "E876",
      "D473", "L219", "L0889", "D638", NA, "R079", "I4891", "I5031",
      "N184", "I2510", "I129", "G4730", "I252", "Z8673", "Z87891",
      "Z7982", "Z8249", "R509", "A419", "J9620", "Z930", "Z9911", "A047",
      "N390", "B952", "B961", "G808", "R6520", "E871", "E860", "I10",
      "K219", "D649", "Z79899", "Z888", "Z882", "R569", "G40401", "J9622",
      "I214", "G9340", "J9811", "I429", "I5040", "Z6841", "N179", "E872",
      "F10239", "I6523", "E1165", "E8351", "E039", "D696", "E8809",
      "E785", "E669", "G4733", "Z9114", "N183", "G312", "G40911", "I5022",
      "Z95810", "I25119", "I499", "Z952", "E119", "I6529", "F1010",
      "Z6830", "Z7901", "D500", "F419", "R339", "Z5189", "S065X0S",
      "R2681", "R45851", "F39", "F323", "F14259", "R45850", "F1421",
      "F17210", "F1210", "J45909", "Z21", "Z560", "Z915", "Z590", "Z91410",
      "Z658", "S72012A", "D62", "B965", "I9581", "R5082", "R000", "M810",
      "R945", "R197", "R001", "J9621", "I5033", "E43", "J90", "R6521",
      "Z515", "Z66", "R570", "I120", "N186", "K652", "I96", "K810",
      "E2740", "I2782", "I82512", "K922", "R188", "X58XXXA", "0", "S72102A",
      "S72145A", "I509", "F418", "I050", "Z992", "Z955", "I447", "W010XXA",
      "M86672", "M86272", "G8220", "M86271", "E441", "M86671", "Z86718",
      "B964", "F329", "Z905", "M549", "A4102", "G061", "J189", "K766",
      "I8500", "M6008", "E1151", "G9520", "M4626", "I484", "L4050",
      "I350", "M5011", "K7460", "I340", "M1990", "M479", "K439", "R55",
      "I81", "L89152", "D684", "D61818", "C228", "I1310", "M6282",
      "F10230", "D6959", "K7030", "N189", "Z9181", "D509", "D7589",
      "F4322", "Z951", "D731", "I951", "E8342", "R296", "S0181XA",
      "W06XXXA", "F3130", "L97519", "F909", "G43909", "Z86711", "G8929",
      "F4310", "Z794", "G40909", "F0390", "Z853", "I611", "I608", "I82612",
      "H53462", "H4010X0", "R2689", "R51", "R002", "I480", "I110",
      "T486X5A", "R0902", "Z23", "Y92230", "R1110", "J8410", "K222",
      "Z9981", "I701", "K529", "D631", "B9620", "Z881", "Z96641", "N289",
      "I442", "Z6839", "J449", "K449", "I739", "Z90710", "Z9049", "Z85828"
    ), class = "factor")
})

test_that("crash case", {
  f <- factor(c(NA, "a"), levels = c(NA, "a"), exclude = NULL)
  expect_identical(refactor(f, c("a", "b")),
                   structure(c(NA, 1L), .Label = c("a", "b"), class = "factor"))
})

test_that("refactor NA to different level", {
  f <- factor(NA, exclude = NULL)
  expect_identical(refactor(f, levels = NA, na.rm = FALSE, exclude_na = FALSE),
                   f)
  f <- factor(c(NA, "a"), exclude = NULL)
  expect_identical(refactor(f, levels = NA, na.rm = FALSE, exclude_na = FALSE),
                   factor(f, levels = NA, exclude = NULL))
  expect_identical(
    refactor(f, levels = c("a", NA), na.rm = FALSE, exclude_na = FALSE),
    factor(c(NA, "a"), levels = c("a", NA), exclude = NULL))
})
