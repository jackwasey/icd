# Copyright (C) 2014 - 2016  Jack O. Wasey
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

context("explain ICD-10-CM codes")

expect_icd10cm_explain_one <- function(...) {
  x <- list(...)
  for (i in seq_along(x))
    eval(bquote(expect_identical(icd_explain(as.icd10cm(.(names(x[i]))), x[[i]]))))
}

test_that("explain leaf codes", {
  skip("WIP")

  expect_icd10cm_explain_one("G000" = "Hemophilus meningitis",
                             "P960" = "Congenital renal failure"
  )
})
