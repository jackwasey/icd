#ifdef ICD_STANDALONE

// don't include my source files: this is like an external application so link against my Rcpp generated headers.
#include "../include/icd.h"

#include <Rcpp.h>
#include <RInside.h>
#include <string>
#include <iostream>

int main(int argc, char *argv[]) {

	RInside R(argc, argv);          // create an embedded R instance

	const Rcpp::CharacterVector testmaj = Rcpp::CharacterVector::create("E10", "V1", "5");
	//Rcpp::CharacterVector res = icd9GetMajor(testmaj, false);
	Rcpp::CharacterVector res;
	res = icd::icd9GetMajor(testmaj, false);

	Rcpp::Rcout << "res = " << res << "\n";

	return 0;
}

#endif
