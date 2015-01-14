// use Rcpp11 here if we want: //// [[plugins(cpp11)]]
// Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

// [[depends(BH)]]

#include <Rcpp.h>
#include <string>
#include <iostream>
#include <boost/algorithm/string.hpp>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector icd9MajMinToCode(std::vector< std::string > mjr, std::vector< std::string > mnr, bool isShort ) {

  CharacterVector out;

  std::vector< std::string >::iterator j = mjr.begin();
  std::vector< std::string >::iterator n = mnr.begin();

  for(; j != mjr.end() && n != mnr.end(); ++j, ++n) {
    std::string mjrelem = *j;
    switch (mjrelem.length()) {
      case 0:
      out.push_back(NA_STRING);
      continue;
      case 1:
      mjrelem.insert(0, "00");
      break;
      case 2:
      mjrelem.insert(0, "0");
    }
    if (mjrelem == "NA") {
      out.push_back(NA_STRING);
      continue;
    }
    std::string mnrelem = *n;
    if (mnrelem == "NA") {
      out.push_back(mjrelem);
      continue;
    }
    std::string s(mjrelem);
    if (!isShort) { s.append("."); }
    s.append(mnrelem);
    out.push_back(s);

  }
  // String chr_s = NA_STRING;

  return as<CharacterVector>(out);
}

//' @name icd9MajMinToShort
//' @title icd9MajMinToShort
//' @export
// [[Rcpp::export]]
CharacterVector icd9MajMinToShort(std::vector< std::string > mjr,
std::vector< std::string > mnr) {
  return icd9MajMinToCode(mjr, mnr, true);
}

//' @name icd9MajMinToDecimal
//' @title icd9MajMinToDecimal
//' @export
// [[Rcpp::export]]
CharacterVector icd9MajMinToDecimal(std::vector< std::string > mjr,
std::vector< std::string > mnr) {
  return icd9MajMinToCode(mjr, mnr, false);
}

//' @name icd9PartsToShort
//' @title icd9PartsToShort
//' @export
// [[Rcpp::export]]
CharacterVector icd9PartsToShort(List parts) {
  return icd9MajMinToCode(parts["major"], parts["minor"], true);
}

//' @name icd9PartsToDecimal
//' @title icd9PartsToDecimal
//' @export
// [[Rcpp::export]]
CharacterVector icd9PartsToDecimal(List parts) {
  return icd9MajMinToCode(parts["major"], parts["minor"], false);
}

//' @name icd9MajMinToParts_df
//' @title icd9MajMinToParts_df
//' @description Convert vectors of major and minor components into a data frame with a column for major and a column for minor.
//' This is 20x faster in this C++ implementation, and important for a very frequently used function.
//' @import Rcpp
//' @export
// [[Rcpp::export]]
List icd9MajMinToParts(CharacterVector mjr, CharacterVector mnr) {
  List returned_frame = List::create(
    _["major"] = mjr,
    _["minor"] = mnr);

    StringVector sample_row = returned_frame(0);
    // this appears to make it slower:
    IntegerVector row_names = seq_len(sample_row.length());
    returned_frame.attr("row.names") = row_names;
    returned_frame.attr("class") = "data.frame";

    return returned_frame;
}

//' @name icd9MajMinToParts
//' @title icd9MajMinToParts
//' @description Convert vectors of major and minor components into a data frame with a column for major and a column for minor.
//' This is probably just as quick in R, but I think I may rely on having a data frame output (which is slower to construct in R)
//' @import Rcpp
//' @export
// [[Rcpp::export]]
List icd9MajMinToParts_list(CharacterVector mjr, CharacterVector mnr) {
  List out = List::create(
    _["major"] = mjr,
    _["minor"] = mnr);

    return out;
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

//' @name icd9ShortToParts_cpp_slow
//' @title extract major and minor parts of a decimal ICD-9 code
//' @description accepts Vxxxx Exxxx or xxxxx
//' @template icd9-short
//' @param minorEmpty vector of length one, to be used in place of
//'   minor part of zero. Defaults to ""
//' @template invalid
//' @return data.frame with two columns. At least the minor part must be
//'   character, because "03" is different to "3", but "30" is the same as "3"
//' @keywords  manip
//' @export
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

  return icd9MajMinToParts(mjr, mnr);
}

//' @name icd9ShortToParts
//' @title extract major and minor parts of a decimal ICD-9 code
//' @description accepts Vxxxx Exxxx or xxxxx. This version just beats native R, but lowerhead for calls.
//' @template icd9-short
//' @param minorEmpty vector of length one, to be used in place of
//'   minor part of zero. Defaults to ""
//' @template invalid
//' @return data.frame with two columns. At least the minor part must be
//'   character, because "03" is different to "3", but "30" is the same as "3"
//' @keywords  manip
//' @export
// [[Rcpp::export]]
List icd9ShortToParts(CharacterVector icd9Short, std::string minorEmpty = "") {

  CharacterVector mjr(icd9Short.size());
  CharacterVector mnr(icd9Short.size());

  for (int i = 0; i < icd9Short.size(); ++i) {

    std::string s = as<std::string>(icd9Short[i]); // do i need to convert?
    boost::algorithm::trim(s); // minimal speed difference

    if (s.at(0) != char('E') && s.at(0) != char('e')) { // not an E code // char match makes little difference to speed
    switch (s.size()) {
      case 1:
      case 2:
      case 3:
      mjr[i] = s.substr(0, s.size());
      mnr[minorEmpty];
      continue;
      case 4:
      case 5:
      mjr[i] = s.substr(0, 3);
      mnr[i] = s.substr(3, s.size()-3);
      continue;
      default:
      mjr[i] = NA_STRING; mnr[i] = NA_STRING; continue;
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

  return icd9MajMinToParts(mjr, mnr);
}


//' @name icd9ShortToParts_cpp_test
//' @title extract major and minor parts of a decimal ICD-9 code
//' @description accepts Vxxxx Exxxx or xxxxx. This version just beats native R...
//' @template icd9-short
//' @param minorEmpty vector of length one, to be used in place of
//'   minor part of zero. Defaults to ""
//' @template invalid
//' @return data.frame with two columns. At least the minor part must be
//'   character, because "03" is different to "3", but "30" is the same as "3"
//' @keywords  manip
//' @export
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
      case 3:
      mjr[i] = s.substr(0, s.size());
      mnr[minorEmpty];
      continue;
      case 4:
      case 5:
      mjr[i] = s.substr(0, 3);
      mnr[i] = s.substr(3, s.size()-3);
      continue;
      default:
      mjr[i] = NA_STRING; mnr[i] = NA_STRING; continue;
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

  return icd9MajMinToParts(mjr, mnr);
}

//' @name icd9ShortToDecimal
//' @title icd9ShortToDecimal
//' @export
// [[Rcpp::export]]
CharacterVector icd9ShortToDecimal(CharacterVector x) {
  List parts = icd9ShortToParts(x);
  return icd9PartsToDecimal(parts);
}
