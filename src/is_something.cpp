#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// similar speed to regex, amazingly.
// [[Rcpp::export]]
Rcpp::LogicalVector cpp_isV( std::vector< std::string > strings ) {

  int len = strings.size();
  Rcpp::LogicalVector out = Rcpp::LogicalVector(len);

  for( int i=0; i < len; i++ ) {
    std::string str = strings[i];
    //std::transform(str.begin(), str.end(),str.begin(), ::toupper);
    std::size_t fndu = str.find("V");
    //std::size_t fndl = str.find("v");
    //if (fndu == std::string::npos && fndl == std::string::npos) {
    if (fndu == std::string::npos) {
      out(i) = FALSE;
    } else {
      out(i) = TRUE;
    }
  }

  return out;
}

// From http://gallery.rcpp.org/articles/faster-data-frame-creation/
// [[Rcpp::export]]
List CheapDataFrameBuilder(List a) {
    List returned_frame = clone(a);
    GenericVector sample_row = returned_frame(0);

    StringVector row_names(sample_row.length());
    for (int i = 0; i < sample_row.length(); ++i) {
        char name[5];
        sprintf(&(name[0]), "%d", i);
        row_names(i) = name;
    }
    returned_frame.attr("row.names") = row_names;

    StringVector col_names(returned_frame.length());
    for (int j = 0; j < returned_frame.length(); ++j) {
        char name[6];
        sprintf(&(name[0]), "X.%d", j);
        col_names(j) = name;
    }
    returned_frame.attr("names") = col_names;
    returned_frame.attr("class") = "data.frame";

    return returned_frame;
}

// [[Rcpp::export]]
List cpp_icd9MajMinToParts(List a) {
    List returned_frame = clone(a);
    GenericVector sample_row = returned_frame(0);

    StringVector row_names(sample_row.length());
    for (int i = 0; i < sample_row.length(); ++i) {
        char name[5];
        sprintf(&(name[0]), "%d", i);
        row_names(i) = name;
    }
    returned_frame.attr("row.names") = row_names;

    StringVector col_names(returned_frame.length());
    for (int j = 0; j < returned_frame.length(); ++j) {
        char name[6];
        sprintf(&(name[0]), "X.%d", j);
        col_names(j) = name;
    }
    returned_frame.attr("names") = col_names;
    returned_frame.attr("class") = "data.frame";

    return returned_frame;
}
