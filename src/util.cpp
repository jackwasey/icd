// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
using namespace Rcpp;

std::string trimRight(std::string& s) {
  std::size_t n = s.find_last_not_of(" \f\n\r\t\v");
  s.erase(n + 1);
  return s;
}

std::string trimLeft(std::string& s) {
  std::size_t n = s.find_first_not_of(" \f\n\r\t\v");
  s.erase(0, n);
  return s;
}

// [[Rcpp::export]]
std::string strim_cpp(std::string& s) {
  trimLeft(s);
  trimRight(s);
  return s;
}

// [[Rcpp::export]]
std::vector<std::string > trim_cpp(std::vector<std::string >& sv) {
  for (std::vector<std::string >::iterator i = sv.begin(); i != sv.end(); ++i) {
    strim_cpp(*i);
  }
  return sv;
}

#ifdef ICD9_DEBUG
void printCharVec(CharacterVector cv) {
  for (CharacterVector::iterator i=cv.begin(); i!=cv.end(); ++i) {
    String s = *i;
    std::cout << s.get_cstring() << " ";
  }
  std::cout << "\n";
  return;
}
#endif

int str_to_int(const char *s) {
    int n = 0;
    // would be easy to skip whitespace here too, but probably no need.
    if (*s<'0' && *s>'9') { // V or E code
        ++s;
    }
    while (*s>='0' && *s<='9') {
        n=(n*10)+(*s-'0');
        ++s;
    }
    return n;
}
