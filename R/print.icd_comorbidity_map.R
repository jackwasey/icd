# Copyright (C) 2014 - 2016  Jack O. Wasey
#
# This file is part of icd9.
#
# icd9 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd9 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

print.icd_comorbidity_map <- function(x, summary = TRUE, n_comorbidities = 7, n_codes = 7) {
  if (summary) {
    message("Showing first ", n_comorbidities, "comorbidities, and ", n_codes, "of each one.")
    x %>%
      lapply(function(x) get_n_or_len(x, n_codes)) %>%
      get_n_or_len(n_comorbidities) %>%
      as.list %>% print
    # not beautiful
    if (length(x) > n_comorbidities)
      print("...\n")
  }
  else
    writeLines("...")
}

get_n_or_len <- function(x, n)
  x[1:ifelse(length(x) < n, length(x), n)]
