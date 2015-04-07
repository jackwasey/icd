// [[Rcpp::interfaces(r, cpp)]]
#include <string>
#include <vector>

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

// [[Rcpp::export]]
std::vector<bool> icd9IsA(const std::vector<std::string>& sv, const char* x,
		bool invert = false) {
	// TODO benchmark vector<char> or vector<int> and also check
	// whether this is used in threaded code (vector<bool> not thread safe
	// because even if accessing different elements, the bits can still be
	// stored in the same byte or word. Unfortunately, Rcpp can't cast a
	// char vector to a LogicalVector.
	int len = sv.size();
	std::vector<bool> out(len);
	for (int i = 0; i < len; ++i) {
		out[i] = invert != (icd9IsASingle(sv[i].c_str(), x));
	}
	return out;
}
