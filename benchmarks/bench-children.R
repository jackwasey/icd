//' @examples
//' \dontrun{
//' if (requireNamespace("microbenchmark")) {
//'   microbenchmark::microbenchmark(
//'     icd:::icd9ChildrenShort(c("001", 100:500), onlyReal = TRUE),
//'     icd:::icd9ChildrenShort_alt11(c("001", 100:500), onlyReal = TRUE),
//'     times = 5)
//'     # C++11 about 15% faster for this data
//' }
//' }


//' @examples
//' \dontrun{
//' library(microbenchmark)
//' microbenchmark(icd9ChildrenShort("001", T), icd9ChildrenShortStd("001", T), times = 100)
//' microbenchmark(icd9ChildrenShort(c("001", 100:400), T),
//'                icd9ChildrenShortUnordered(c("001", 100:400), T),
//'                icd9ChildrenShortStd(c("001", 100:400), T),
//'                times = 10)
//' }
//' # un-ordered set much faster, but may still need to sort result


#' @examples
#' \dontrun{
#' library(microbenchmark)
#' microbenchmark::microbenchmark(
#'   icd:::children_defined.icd10cm("A01"),
#'   icd:::children_defined_r.icd10cm("A01")
# ' )
#' stopifnot(identical(icd:::children_defined.icd10cm("A00"),
#'   icd:::children_defined_r.icd10cm("A00")))
#' }
#' @keywords internal

