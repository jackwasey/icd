#ifdef ICD_STANDALONE

// don't include my source files: this is like an external application so link against my Rcpp generated headers.
#include "../include/icd.h"
#include <Rcpp.h>
#include <RInside.h>
#include <string>
#include <iostream>

int main(int argc, char *argv[]) {

	RInside R(argc, argv);          // create an embedded R instance

	const CV testmaj = CV::create("E10", "V1", "5");
	//CV res = icd9GetMajor(testmaj, false);
	CV res;
	res = icd::icd9GetMajor(testmaj, false);

	Rcpp::Rcout << "res = " << res << "\n";

	return 0;
}

#endif
