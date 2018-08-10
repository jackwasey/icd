#include "icd_types.h"
#include "local.h"
#include <string>
#include <vector>
using Rcpp::runif;
using Rcpp::as;
typedef std::vector<double> VD;

// [[Rcpp::export]]
Rcpp::NumericVector randomMajorCpp(int	n) {
  // This could just be a sprintf like the others.
  Rcpp::NumericVector iv = Rcpp::floor(runif(n) * 999);
  return iv;
}

//' @rdname icd9RandomShort
//' @keywords internal
// [[Rcpp::export]]
VecStr icd9RandomShortN(VecStr::size_type n = 5) {
  VecStr out(n);
  std::vector<double> randoms = as<std::vector<double> >(runif(n, 0, 99999));
  char buffer[6];
  for (std::vector<double>::size_type i = 0; i != n; ++i) {
    sprintf(buffer, "%.0f", randoms[i]);
    out[i] = buffer;
  }
  return out;
}

//' @rdname icd9RandomShort
//' @keywords internal
// [[Rcpp::export]]
VecStr icd9RandomShortV(VecStr::size_type n = 5) {
  VecStr out(n);
  std::vector<double> randoms = as<std::vector<double> >(runif(n, 0, 9999));
  char buffer[6];
  for (std::vector<double>::size_type i = 0; i != n; ++i) {
    sprintf(buffer, "V%.0f", randoms[i]);
    out[i] = buffer;
  }
  return out;
}

//' @rdname icd9RandomShort
//' @keywords internal
// [[Rcpp::export]]
VecStr icd9RandomShortE(VecStr::size_type n = 5) {
  VecStr out(n);
  std::vector<double> randoms = as<std::vector<double> >(runif(n, 0, 9999));
  char buffer[6];
  for (std::vector<double>::size_type i = 0; i != n; ++i) {
    sprintf(buffer, "E%.0f", randoms[i]);
    out[i] = buffer;
  }
  return out;
}

//' Generate random short-form ICD-9 codes
//'
//' Quick pseudo-random by picking numeric, 'V' or 'E' based on modulo three of
//' the number, with predominance of purely numeric codes.
//' @keywords internal
// [[Rcpp::export]]
CV icd9RandomShort(unsigned int n = 5) {
  CV out(n);
  char buffer[7]; // 5+1 for null should be okay...
  uint_fast32_t r; // need 32 bits for 99999
  uint_fast8_t t;
  for (unsigned int i = 0; i != n; ++i) {
    r = R::runif(0.1, 99998.9); // force to unsigned integer type
    t = R::rnorm(2.49, 0.5); // force to unsigned char type
    switch (t % 3) {
    case 2:
      sprintf(buffer, "%u", r);
      break;
    case 0:
      sprintf(buffer, "V%u", r / 10);
      break;
    case 1:
      sprintf(buffer, "E%u", r / 10);
      break;
    default:
      {}
    }
    out[i] = buffer;
  }
  return out;
}
