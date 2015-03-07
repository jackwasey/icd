// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
//#include <local.h>
#include <icd9.h>
using namespace Rcpp;

std::vector<bool> icd9IsA(const std::vector<std::string>& sv, std::string ms,
		bool inverse = false) {
	int len = sv.size();
	std::vector<bool> out(len);
	for (int i = 0; i < len; ++i) {
		out[i] = inverse == (sv[i].find_first_of(ms) == std::string::npos);
	}
	return out;
}

// [[Rcpp::export]]
bool icd9IsASingleV(const std::string& s) {
	return s.find_first_of("Vv") != std::string::npos;
}

// [[Rcpp::export]]
bool icd9IsASingleE(const std::string& s) {
	return s.find_first_of("Ee") != std::string::npos;
}

// [[Rcpp::export]]
bool icd9IsASingleVEstr(const std::string& s) {
	// TODO: much quicker with c_str
	//char c = s.c_str()[0];
	//return c=='V' || c=='E' || c=='v' || c=='e';
	return s.find_first_of("VvEe") != std::string::npos;
}

// [[Rcpp::export]]
bool icd9IsASingleVE(const std::string& s) {
  const char* t = s.c_str();
  // ditch preceding spaces (probably should also do other white space)
  while (*t == ' ') ++t;
  return *t=='V' || *t=='E' || *t=='v' || *t=='e';
}

//' @name icd9Is
//' @title are the given codes numeric, V or E type?
//' @description Quickly find V or E codes, without any validation.
//' @template icd9-any
//' @export
// [[Rcpp::export]]
std::vector<bool> icd9IsV(const std::vector<std::string>& icd9) {
	return icd9IsA(icd9, "Vv");
}

//' @rdname icd9Is
//' @export
// [[Rcpp::export]]
std::vector<bool> icd9IsE(const std::vector<std::string>& icd9) {
	return icd9IsA(icd9, "Ee");
}

//' @rdname icd9Is
//' @export
// [[Rcpp::export]]
std::vector<bool> icd9IsVE(const std::vector<std::string>& icd9) {
	return icd9IsA(icd9, "VvEe");
}

//' @rdname icd9Is
//' @export
// [[Rcpp::export]]
std::vector<bool> icd9IsN(const std::vector<std::string>& icd9) {
	return icd9IsA(icd9, "VvEe", true);
}
