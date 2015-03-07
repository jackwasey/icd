// [[Rcpp::interfaces(r, cpp)]]
#include <local.h>

bool icd9IsASingle(const char* s, const char* x) {
	while (*s == ' ')
		++s;
	while (*x) {
		if (*s == *x)
			return true;
		++x;
	}
	return false;
}

bool icd9IsASingleV(const char* s) {
	while (*s == ' ')
		++s;
	return *s == 'V' || *s == 'v';
}

bool icd9IsASingleE(const char* s) {
	while (*s == ' ')
		++s;
	return *s == 'E' || *s == 'e';
}

bool icd9IsASingleVE(const char* s) {
	// ditch preceding spaces (probably should also do other white space)
	while (*s == ' ')
		++s;
	return *s == 'V' || *s == 'E' || *s == 'v' || *s == 'e';
	//return s.find_first_of("VvEe") != std::string::npos;
}

std::vector<bool> icd9IsA(const std::vector<std::string>& sv, const char* x,
		bool inverse = false) {
	//TODO benchmark vector<char> or vector<int>
	int len = sv.size();
	std::vector<bool> out(len);
	for (int i = 0; i < len; ++i) {
		//out[i] = inverse == (sv[i].find_first_of(x) == std::string::npos);
		out[i] = inverse != (icd9IsASingle(sv[i].c_str(), x));
	}
	return out;
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
