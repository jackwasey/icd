# Copyright (C) 2014 - 2015  Jack O. Wasey
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

######################################################################
#
# get and set class types this package uses The master list is: icd9 icd9cm icd10
# icd10cm icd10who icd_long icd_wide icd_decimal icd_short icd_map
#
# I'm not sure of the best order, so I think I'll avoid assuming any order,
# except for more specific ICD types coming first.
######################################################################

icd_set_icd9 <- function(x) {
  if (inherits(x, "icd9")) return(x)
  if (inherits(x, c("icd10", "icd10cm", "icd10who")))
      stop("Trying to set ICD-9 class on an object which already has an ICD-10 class")
  icd9cm_pos = match("icd9cm", class(x))
  if (!is.na(icd9cm_pos)) {
    # this means there is an icd9cm class, but no icd9 class
    class(x) <- append(class(x), "icd9", after = icd9cm_pos)
    return(x)
  }
class(x) <- append(class(x), "icd9")
}


icd_set_icd9cm <- function(x) {
  if (inherits(x, "icd9") && inherits(x, "icd9cm")) return(x)
  if (inherits(x, c("icd10", "icd10cm", "icd10who")))
    stop("Trying to set ICD-9 class on an object which already has an ICD-10 class")
  icd9_pos = match("icd9", class(x))
  if (!is.na(icd9_pos)) {
    class(x) <- append(class(x), "icd9cm", after = icd9_pos - 1)
    return(x)
  }
  class(x) <- append(class(x), "icd9cm", after = 0) # put the more specific type at beginning
}

