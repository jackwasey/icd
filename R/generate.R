#' Generate random ICD-9 codes
#'
#' Uses leaf/billable ICD-10-CM codes from current master list
#' @param n number to select, passed to \code{sample}
#' @template short_code
#' @keywords internal debugging datagen
#' @noRd
generate_random_short_icd10cm_bill <- function(n = 10, short_code = TRUE) {
  i <- icd10cm2019
  x <- sample(
    unlist(
      i[i$billable == 1, "code"]
    ),
    replace = TRUE, size = n
  )
  if (short_code) x else short_to_decimal(x)
}
