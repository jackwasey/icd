# Copyright (C) 2014 - 2017  Jack O. Wasey
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

icd_children_defined_r.icd10cm <- function(x, short_code = icd_guess_short(x), warn = FALSE) {

  assert_character(x)
  assert_flag(short_code)

  x <- trim(x)
  if (!short_code)
    x <- icd_decimal_to_short.icd10cm(x)

  # we match twice here, once with %in% and once with match...
  matches_bool <- x %in% icd10cm2016[["code"]]
  # if the codes are not in the source file, we ignore, warn, drop silently?
  if (warn && !all(matches_bool))
    warning("some values did not match any ICD-10-CM codes: ",
            paste(x[!matches_bool], collapse = ", "))

  x <- x[matches_bool]
  matches <- match(x, icd10cm2016[["code"]])
  last_row <- nrow(icd10cm2016)

  kids <- character(0)

  if (length(x) == 0) {
    if (length(x) > 0)
      warning("none of the provided ICD-10 codes matched the canonical list")
    return(icd10cm(character(0)))
  }

  for (i in seq_along(x)) {
    # now the children, assuming the source file is sorted logically, will be
    # subsequent codes, until a code of the same length is found (and stop one before that)
    check_row <- matches[i] + 1 # start with code after the match from the input
    parent_len <- .nc[matches[i]]
    while (.nc[check_row] > parent_len && check_row != last_row + 1)
      check_row <- check_row + 1

    kids <- c(kids, icd10cm2016[matches[i]:(check_row - 1), "code"])
  }
  as.icd10cm(kids, short_code)
}
