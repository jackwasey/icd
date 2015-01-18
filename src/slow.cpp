//include boost
// [[depends(BH)]]

// generate header files in 'inst/include'
// [[Rcpp::interfaces(r, cpp)]]
// and use them (does this need two compilation steps?)
//#[[Rcpp::depends(icd9)]]

#include <Rcpp.h>
#include <sstream>
#include <string>
#include <iostream>
#include <boost/algorithm/string.hpp>
#include <icd9.h>
using namespace Rcpp;

// slow versions of functions for benchmarking

// [[Rcpp::export]]
std::vector<bool> icd9IsV_cpp_slower(std::vector< std::string > sv) {
  int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    std::string svt = boost::algorithm::trim_left_copy(sv[i]); // little speed difference
    if (svt.size() == 0) { continue; }
    out[i] = svt.at(0) == 'V' || svt.at(0) == 'v';
  }
  return out;
}

//' @name icd9ShortToParts_cpp_slow
//' @title extract major and minor parts of a decimal ICD-9 code
//' @description accepts Vxxxx Exxxx or xxxxx
//' @template icd9-short
//' @param minorEmpty vector of length one, to be used in place of
//' minor part of zero. Defaults to ""
//' @template invalid
//' @return data.frame with two columns. At least the minor part must be
//' character, because "03" is different to "3", but "30" is the same as "3"
//' @keywords manip
// [[Rcpp::export]]
List icd9ShortToParts_cpp_slow(CharacterVector icd9Short, String minorEmpty = "") {

  CharacterVector mjr;
  CharacterVector mnr;
  // can only reserve with std::vector
  //mjr.reserve(icd9Short.size())
  //mnr.reserve(icd9Short.size())

  for (CharacterVector::iterator i(icd9Short.begin());
  i < icd9Short.end();
  ++i) {

    std::string s = as<std::string>(*i); // do i need to convert?
    boost::algorithm::trim(s); // minimal speed difference

    if (s.find_first_of("Ee") == std::string::npos) { // not an E code
    switch (s.size()) {
      case 1:
      case 2:
      case 3:
      mjr.push_back(s.substr(0, s.size()));
      mnr.push_back(minorEmpty);
      continue;
      case 4:
      case 5:
      mjr.push_back(s.substr(0, 3));
      mnr.push_back(s.substr(3, s.size()-3));
      continue;
      default:
      mjr.push_back(NA_STRING); mnr.push_back(NA_STRING); continue;
    }
    } else { // E code
    if (s.size()<4) {
      mjr.push_back(NA_STRING); mnr.push_back(NA_STRING); continue;
    }
    mjr.push_back(s.substr(0, 4));
    if (s.size()<5) {
      mnr.push_back(minorEmpty);
    } else {
      mnr.push_back(s.substr(4, 1));
    }
    } // E code
  } // for
  return List::create();
  // TODO: return icd9MajMinToParts(mjr, mnr);
}


//' @name icd9MajMinToParts_slower
//' @title icd9MajMinToParts_slower
//' @description Convert vectors of major and minor components into a data frame with a column for major and a column for minor.
//' This is slightly slower than the R implementation. using Rcpp11 std::to_string is even slower than this.
//' @import Rcpp
//' @export
// [[Rcpp::export]]
List icd9MajMinToParts_slower(CharacterVector mjr, CharacterVector mnr) {
  List returned_frame = List::create(
    _["major"] = mjr,
    _["minor"] = mnr);

    StringVector sample_row = returned_frame(0);
    StringVector row_names(sample_row.length());
    for (int i = 0; i < sample_row.length(); ++i) {
      char name[9]; // this is the buffer, so make a good number of chats. Could log10+1 the number of rows. Or use std::string
      sprintf(&(name[0]), "%d", i);
      row_names(i) = name;
    }

    returned_frame.attr("row.names") = row_names;
    returned_frame.attr("class") = "data.frame";

    return returned_frame;
}

bool icd9IsA_cpp_slow(std::string s, const char* c) {
  return s.find_first_of(c) != std::string::npos;
}
std::vector<bool> icd9Is_cpp_slow(std::vector< std::string > sv, const char* c) {
  int len = sv.size();
  std::vector<bool> out(len);
  for (int i = 0; i < len; ++i) {
    out[i] = icd9IsA_cpp_slow(sv[i], c);
  }
  return out;
}


//' @name icd9Is_cpp_slow
//' @title is the given code V or E type?
//' @description quickly find V or E codes, without fully validating V or E
//'   codes. Use fixed instead of regex for speed. Don't check position of V or
//'   E: this is not validation, just a quick classification of pre-validated
//'   codes. TODO: this doesn't look efficient, but can't use fixed with ignore
//'   case, and regex slower.
//' @template icd9-any
//' @examples
//'
//' library(microbenchmark)
//' # regex is a little faster than fixed
//' icd9 <- rep(times = 500, c("1", "not", "V10.0", " E950", ""))
//' microbenchmark(times = 3,
//'   grepl(pattern = "E", icd9, fixed = TRUE) |
//'   grepl(pattern = "e", icd9, fixed = TRUE) |
//'   grepl(pattern = "V", icd9, fixed = TRUE) |
//'   grepl(pattern = "v", icd9, fixed = TRUE))
//' microbenchmark(times = 3, grepl(pattern = "[EeVv]", rnd))
//' microbenchmark(icd9:::icd9IsV_cpp_slower(icd9), icd9:::icd9IsV_R(icd9), icd9:::icd9IsV_cpp_slow(icd9), icd9:::icd9IsV(icd9))
//'
// [[Rcpp::export]]
std::vector<bool> icd9IsV_cpp_slow(std::vector< std::string > sv) { return icd9Is_cpp_slow(sv, "Vv");}

//' @rdname icd9Is_cpp_slow
// [[Rcpp::export]]
std::vector<bool> icd9IsE_cpp_slow(std::vector< std::string > sv) { return icd9Is_cpp_slow(sv, "Ee");}

//' @rdname icd9Is_cpp_slow
// [[Rcpp::export]]
std::vector<bool> icd9IsVE_cpp_slow(std::vector< std::string > sv) { return icd9Is_cpp_slow(sv, "VvEe"); }


// [[Rcpp::export]]
List icd9ShortToParts_cpp_test(CharacterVector icd9Short, std::string minorEmpty = "") {

  CharacterVector mjr(icd9Short.size());
  CharacterVector mnr(icd9Short.size());

  for (int i = 0; i < icd9Short.size(); ++i) {

    std::string s = as<std::string>(icd9Short[i]); // do i need to convert?
    boost::algorithm::trim(s); // minimal speed difference

    if (s.at(0) != char('E') && s.at(0) != char('e')) { // not an E code // char match makes little difference to speed
    switch (s.size()) {
      case 1:
      case 2:
      case 3: mjr[i] = s.substr(0, s.size()); mnr[minorEmpty]; continue;
      case 4:
      case 5: mjr[i] = s.substr(0, 3); mnr[i] = s.substr(3, s.size()-3); continue;
      default: mjr[i] = NA_STRING; mnr[i] = NA_STRING; continue;
    }
    } else { // E code
    if (s.size()<4) {
      mjr[i] = NA_STRING; mnr[i] = NA_STRING; continue;
    }
    mjr[i] = s.substr(0, 4);
    if (s.size()<5) {
      mnr[i] = minorEmpty;
    } else {
      mnr[i] = s.substr(4, 1);
    }
    } // E code
  } // for

  return icd9::icd9MajMinToParts(mjr, mnr);
}
