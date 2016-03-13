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

context("generate defined child codes for ICD-10-CM")

expect_icd10cm_child_is_self <- function(...) {
  dots <- unlist(list(...))
  for (i in dots) {
    eval(bquote(expect_identical(icd_children(icd10cm(.(i))), icd10cm(.(i)))))
    eval(bquote(expect_identical(icd_children(icd10(.(i))), icd10(.(i)))))
    eval(bquote(expect_identical(icd_children(.(i)), .(i)))) # ?should set code

    eval(bquote(expect_identical(icd_children.icd10cm(.(i)), icd10cm(.(i)))))
    eval(bquote(expect_identical(icd_children.icd10cm(icd10(.(i))), icd10cm(.(i)))))
    eval(bquote(expect_identical(icd_children.icd10cm(icd10cm(.(i))), icd10cm(.(i)))))

    eval(bquote(expect_identical(icd_children.icd10(icd10(.(i))), icd10(.(i)))))
    eval(bquote(expect_inherits(icd_children.icd10(icd10cm(.(i))), "icd10")))
    eval(bquote(expect_identical(icd_children.icd10(.(i)), icd10(.(i)))))
    eval(bquote(expect_identical(unclass(icd_children(icd10(.(i)))), .(i))))
  }
}

test_that("children of a leaf node returns itself", {

  expect_icd10cm_child_is_self("O9A119", "O9A53", "S00.00XA", "T3299", "P150", "P159", "Z9989", "Z950", "C7A098", "C7A8")

  rand_icd10cm <- generate_random_icd10cm_bill(50)
  expect_icd10cm_child_is_self(rand_icd10cm)

})
