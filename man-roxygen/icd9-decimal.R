#' @param <% if (exists("icd9DecimalName")) cat(icd9ShortName) else
#'   cat("icd9Decimal") %> is a character vector of ICD-9 codes. If fewer than
#'   five characters is given in a code, then the digits are greedily assigned
#'   to hundreds, then tens, then units, before the decimal parts. E.g. "10" 
#'   becomes "010", not "0010"
