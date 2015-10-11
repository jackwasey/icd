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

# Initial attempt at ICD-10 ranges, primarily in order to accurately encode the
# comorbidity mapping from Quan 2005 which specifies ICD-10 ranges such as:
# K70.0-K70.3 and I60.x-I69.x

# At first glance I do not see any ranges where there is a '.x' start and
# specific numeric end, nor vice versa, so will not implement yet.
# https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2016-ICD-10-CM-Guidelines.pdf

# will move this out, but for now:
getIcd10ShortAndLongDesc <- function() {
  url <- "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2016-Code-Descriptions-in-Tabular-Order.zip"
  tf <- tempfile()
  zip_single(url, "icd10cm_order_2016.txt", tf)

}
