#ifdef ICD_STANDALONE

#include "icd.h"
#include "comorbid_alt_MatMul.h"

// don't include my source files: this is like an external application so link
// against my Rcpp generated headers.

// don't NEED to add LinkingTo: RInside to DESCRIPTION, but this lets rstudio pick up the headers
#include <RInside.h>
#include <RcppEigen.h>
#include <string>
#include <iostream>

using namespace Rcpp;

int main(int argc, char *argv[]) {

  try {
    RInside R(argc, argv);          // create an embedded R instance
    R["txt"] = "Hello, world!\n";	// assign a char* (string) to 'txt'
    R.parseEvalQ("cat(txt)"); // eval the init string, ignoring any returns

    Rcpp::Rcout << "loading icd library" << std::endl;
    R.parseEvalQ("library(icd)");

    //std::string cmd = "icd:::generate_random_pts(5)";
    //DataFrame icd9df = R.parseEval(cmd);
    std::string cmd =
      "data.frame(visit_id = c('v01', 'v01', 'v02', 'v02'), "
      "  icd9 = c('040', '000', '100', '000'), "
      "stringsAsFactors = TRUE)";
    std::cout << "cmd is: " << cmd << std::endl;
    Rcpp::DataFrame two_pts = R.parseEval(cmd);

    List two_map;
    two_map["malady"] = CharacterVector::create("100", "200");
    two_map["ailment"] = CharacterVector::create("003", "040");

    // call direct to avoid Rcpp intercepting the call?
    Rcpp::IntegerMatrix out = icd9Comorbid_alt_MatMul(two_pts, two_map,
                                                      "visit_id", "icd9",
                                                      0, 0, 0, true);

    std::cout << "res = " << out << "\n";

  } catch(std::exception& ex) {
    std::cerr << "Exception caught: " << ex.what() << std::endl;
  } catch(...) {
    std::cerr << "Unknown exception caught" << std::endl;
  }
  std::cout << "done" << std::endl;
  return 0; // or exit(0) ?
}

#endif // #STANDALONE
