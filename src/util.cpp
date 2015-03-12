// [[Rcpp::interfaces(r, cpp)]]
#include <Rinternals.h>
#include <vector>
#include <string>
#ifdef _OPENMP
#include <omp.h>
#endif

//TODO someday, can go much faster (but less portable) with C strings. see is.cpp for similar code.

// trim one string from right
std::string trimRight(std::string& s) {
	std::size_t n = s.find_last_not_of(" \f\n\r\t\v");
	s.erase(n + 1);
	return s;
}

// trim one string from left
std::string trimLeft(std::string& s) {
	std::size_t n = s.find_first_not_of(" \f\n\r\t\v");
	s.erase(0, n);
	return s;
}

// trim a single string at both ends
// [[Rcpp::export]]
std::string strimCpp(std::string& s) {
	trimLeft(s);
	trimRight(s);
	return s;
}

// [[Rcpp::export]]
std::vector<std::string> trimCpp(std::vector<std::string>& sv) {
	for (std::vector<std::string>::iterator i = sv.begin(); i != sv.end();
			++i) {
		strimCpp(*i);
	}
	return sv;
}

// [[Rcpp::export]]
bool assertFactorOrCharacter(SEXP x) {
	if (!Rf_isString(x) && !Rf_isFactor(x)) {
		Rf_error("Must be a factor or character");
	}
	return true; // Rcpp fails with void for some reason
}

#ifdef ICD9_DEBUG
void printCharVec(CharacterVector cv) {
	for (CharacterVector::iterator i=cv.begin(); i!=cv.end(); ++i) {
		String s = *i;
		Rcpp::Rcout << s.get_cstring() << " ";
	}
	Rcpp::Rcout << "\n";
	return;
}
#endif

// [[Rcpp::export]]
int getOmpCores() {
	int cores = 1;
#ifdef _OPENMP
	cores = omp_get_num_procs();
#endif
	return cores;
}

