#include <Rcpp.h>
using namespace Rcpp;

// Duff's device for 0..n sequence, for fun.
// [[Rcpp::export]]
std::vector<int> seqZero(const int n) {
  const int m = n + 1;
  std::vector<int> out;
  out.reserve(m);
  int c = 0;
  {
    int nl = (m + 7) / 8;
    switch (m % 8) {
    case 0:
      do {
        out.emplace_back(c++);
      case 7:
        out.emplace_back(c++);
      case 6:
        out.emplace_back(c++);
      case 5:
        out.emplace_back(c++);
      case 4:
        out.emplace_back(c++);
      case 3:
        out.emplace_back(c++);
      case 2:
        out.emplace_back(c++);
      case 1:
        out.emplace_back(c++);
      } while (--nl != 0);
    }
  }
  return out;
}

// [[Rcpp::export]]
std::vector<int> seqZeroLoop(const int n) {
  std::vector<int> out;
  const int m = n + 1;
  out.reserve(m);
  for (int i = 0; i != m; ++i) {
    out.emplace_back(i);
  }
  return out;
}

/***R
# This is slower with -O2, and more so with -O3 even with -fno-unroll-loops
bench::press(n = c(100, 1e6, 1e7, 1e8), {
  bench::mark(seqZero(n), seqZeroLoop(n))
})
*/
