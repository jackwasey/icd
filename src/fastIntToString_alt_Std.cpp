#include <Rcpp.h>

//' @rdname fastIntToString
//' @title Fast convert integer vector to character vector
//' @description Fast conversion from integer vector to character vector using C++
//' @param x vector of integers
//' @param bufferSize int if any input strings are longer than this number
//'   (default 16) there will be memory errors. No checks done for speed.
//' @keywords internal
// [[Rcpp::export]]
std::vector<std::string> fastIntToString_alt_Std(std::vector<int> x) {
  std::vector<std::string>::size_type len = x.size();
  std::vector<std::string> out(len);
  char buffer[64];
  for (std::vector<double>::size_type i = 0; i != len; ++i) {
    sprintf(buffer, "%u", x[i]);
    out[i] = buffer;
  }
  return out;
}
