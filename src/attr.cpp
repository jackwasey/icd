#include "attr.h"
#include "icd_types.h"
using namespace Rcpp;

//' Set ICD short-form diagnosis code attribute
//'
//' Doing this in an R function doesn't work for 'void' equivalent, and does a
//' copy if the updated object is returned.
//' @examples
//' j <- "100"
//' attr(j, "icd_short_diag") <- TRUE
//' j
//' attr(j, "icd_short_diag") <- FALSE
//' j
//' icd:::attr_decimal_diag(j)
//' as.decimal_diag(j)
//' # if pryr is installed, use address and refs to see what is going on
//' @keywords internal attribute
// [[Rcpp::export(attr_decimal_diag)]]
void setDecimalDiag(RObject& x, bool value = true) { x.attr("icd_short_diag") = !value; }

//' Set short diagnosis flag in C++
//' @param x Any R object
//' @param value \code{TRUE} or \code{FALSE}
//' @keywords internal attribute
// [[Rcpp::export(attr_short_diag)]]
void setShortDiag(RObject& x, bool value = true) { x.attr("icd_short_diag") = value; }
