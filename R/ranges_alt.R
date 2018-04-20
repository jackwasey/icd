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

icd9_expand_range_worker_alt_base <- function(start, end, lookup, defined,
                                         ex_ambig_start, ex_ambig_end) {
  assert_string(start)
  assert_string(end)
  assert_character(lookup, any.missing = FALSE, min.chars = 3)
  assert_flag(ex_ambig_start)
  assert_flag(ex_ambig_end)

  start_index <- match(start, lookup)
  end_index <- match(end, lookup)
  assert_integer(start_index, len = 1L)
  if (is.na(start_index[1]))
    stop(sprintf("start value '%s' not found in look-up table of ICD-9 codes.", start))
  assert_integer(end_index, len = 1L)
  if (is.na(end_index[1L]))
    stop(sprintf("end value '%s' not found in look-up table of ICD-9 codes.", end))
  if (end_index < start_index)
    stop("end code must be greater than or equal to start code")

  if (start == end)
    return(children.icd9(start, short_code = TRUE, defined = defined))

  out <- lookup[start_index:end_index]
  if (ex_ambig_start) {
    # just remove those codes at the beginning which have children not in the
    # output let's take the first 5, to cover cases like 100, 101, 102.1,
    # 102.11, 102.2
    starts <- tail(out, 5)
    for (s in starts) {
      if (!all(children.icd9(s, short_code = TRUE, defined = defined) %in% out))
        out <- out[-which(out == s)]
    }
  }
  if (ex_ambig_end) {
    # at the end, we don't want any higher-level codes at the end which would
    # have children beyond the range. There could be lots of lower level codes
    # at the end, so we actually have to search the whole list. This means that
    # even if trying to preserve the ambig start, setting ambig end will have to
    # kill it, if it spills over.
    out_cp <- out
    for (o in out_cp) {
      if (any(children.icd9(o, short_code = TRUE, defined = defined) %nin% out))
        out <- out[-which(out == o)]
    }
  }
  sort_icd.icd9(
    unique(
      c(out,
        children.icd9(end, short_code = TRUE, defined = defined)
      )
    ),
    short_code = TRUE
  )
}
