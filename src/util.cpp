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
	return sv;}

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


std::string myuitos(unsigned int i) {
#ifdef ICD9_DEBUG_SETUP_TRACE
			std::cout << "myuitos\n";
#endif

	std::string s(std::numeric_limits<unsigned int>::digits10+2, 0);
	unsigned int size=0;
	if (i==0) {
		s[size++]='0';
	} else {
#ifdef ICD9_DEBUG_SETUP_TRACE
			std::cout << "non-zero, looping through powers of ten\n";
#endif

		int ro = 0;
		for (int i_div; i; i=i_div) {
			i_div = i/10;
#ifdef ICD9_DEBUG_SETUP_TRACE
			std::cout << "i_div = " << i_div << ", and i = " << i << "\n";
#endif

			int i_mod=i%10;
			s[size++] = static_cast<char>('0' + i_mod);
		}
		std::reverse(&s[ro], &s[size]);
	}
	s.resize(size);
	return s;
}
