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

#' convert to character vector without warning
#' @param x vector, typically numeric or a factor
#' @return character vector
#' @keywords internal
as_char_no_warn <- function(x) {
  if (is.character(x))
    return(x)
  old <- options(warn = -1)
  on.exit(options(old))
  if (is.integer(x))
    return(fastIntToStringRcpp(x))
  if (is.factor(x))
    return(levels(x)[x])
  as.character(x)
}

#' @describeIn as_char_no_warn If the argument is a factor, return the levels
#'   instead
as_char_or_levels <- function(x) {
  if (is.character(x))
    return(x)
  if (is.factor(x))
    return(levels(x))
  as_char_no_warn(x)
}
