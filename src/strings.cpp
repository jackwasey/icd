// use Rcpp11 here if we want: //// [[plugins(cpp11)]]
// Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

// [[depends(BH)]]

#include <Rcpp.h>
#include <sstream>
#include <string>
#include <iostream>
#include <boost/algorithm/string.hpp>
using namespace Rcpp;

//////// headers
// cludge to avoid writing header file
std::vector<bool> icd9IsE(std::vector< std::string > sv);
bool icd9IsSingleVE(std::string s);

// [[Rcpp::export]]
CharacterVector icd9MajMinToCode( CharacterVector mj, CharacterVector mn, bool isShort ) {

  CharacterVector out;
  std::vector< std::string > mjr = as<std::vector< std::string > >(mj);
  std::vector< std::string > mnr = as<std::vector< std::string > >(mn);

  std::vector< std::string >::iterator j = mjr.begin();
  std::vector< std::string >::iterator n = mnr.begin();

  for(; j != mjr.end() && n != mnr.end(); ++j, ++n) {
    std::string mjrelem = *j;
    switch (mjrelem.length()) {
      case 0:
      out.push_back(NA_STRING);
      continue;
      case 1:
      if (!icd9IsSingleVE(mjrelem)) {
        mjrelem.insert(0, "00");
      }
      break;
      case 2:
      if (!icd9IsSingleVE(mjrelem)) {
        mjrelem.insert(0, "0");
      } else {
        mjrelem.insert(1, "0");
      }
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
  // String chr_s = NA_STRING; // TODO

  return as<CharacterVector>(out);
}

//' @name icd9MajMinToShort
//' @rdname icd9MajMinToShort
//' @title icd9MajMinToShort
//' @export
// [[Rcpp::export]]
CharacterVector icd9MajMinToShort(CharacterVector mjr,
CharacterVector mnr) {
  if ((mjr.size()!=1 && mjr.size() != mnr.size()) ||
      (mjr.size()==1 && mnr.size()==0)) {
      stop("icd9MajMinToShort, length of majors and minors must be equal.");
    }
    if (mjr.size() == 1) {
      CharacterVector newmjr(mnr.size(), mjr[0]);
      return icd9MajMinToCode(newmjr, mnr, true);
    }
    return icd9MajMinToCode(mjr, mnr, true);
}

//' @name icd9MajMinToDecimal
//' @title icd9MajMinToDecimal
//' @export
// [[Rcpp::export]]
CharacterVector icd9MajMinToDecimal(CharacterVector mjr, CharacterVector mnr) {
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
//' minor part of zero. Defaults to ""
//' @template invalid
//' @return data.frame with two columns. At least the minor part must be
//' character, because "03" is different to "3", but "30" is the same as "3"
//' @keywords manip
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
//' minor part of zero. Defaults to ""
//' @template invalid
//' @return data.frame with two columns. At least the minor part must be
//' character, because "03" is different to "3", but "30" is the same as "3"
//' @keywords manip
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


// [[Rcpp::export]]
List icd9DecimalToParts(CharacterVector icd9Decimal, std::string minorEmpty = "") {
  //  std::vector< std::string > majors;
  //  std::vector< std::string > minors;
  CharacterVector majors;
  CharacterVector minors;
  int ilen = icd9Decimal.length();
  //majors.reserve(ilen);
  //minors.reserve(ilen);
  for (CharacterVector::iterator it = icd9Decimal.begin(); it != icd9Decimal.end(); ++it) {
    String strna = *it;
    if (strna == NA_STRING) { majors.push_back(NA_STRING); minors.push_back(NA_STRING); continue; }
    std::string thiscode = as<std::string >(*it); // must I really do this??
    std::size_t pos = thiscode.find(".");
    // substring parts
    std::string mjrin;
    String mnrout;
    if (pos != std::string::npos) {
      mjrin = thiscode.substr(0, pos);
      mnrout = thiscode.substr(pos+1);
    } else { // didn't find '.'
    mjrin = thiscode; mnrout = "";
    }
    String mjrout;
    switch (mjrin.size()) {
      case 3: break;
      case 2: mjrout = "0"+mjrin; break;
      case 1: mjrout = "00"+mjrin; break;
      case 0: mjrout = NA_STRING; mnrout = NA_STRING; // can't do this with std::string
      default: break;
    }
    majors.push_back(mjrout);
    minors.push_back(mnrout);
  }
  return List::create(_["major"] = majors, _["minor"] = minors);
}

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

// this is simplest just to hard-code
const CharacterVector vbase = CharacterVector::create("" , "0" ,"1", "2", "3", "4", "5" , "6" , "7" , "8", "9" , "00");
const CharacterVector v0 = CharacterVector::create("0", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09");
const CharacterVector v1 = CharacterVector::create("1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19");
const CharacterVector v2 = CharacterVector::create("2", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29");
const CharacterVector v3 = CharacterVector::create("3", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39");
const CharacterVector v4 = CharacterVector::create("4", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49");
const CharacterVector v5 = CharacterVector::create("5", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59");
const CharacterVector v6 = CharacterVector::create("6", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69");
const CharacterVector v7 = CharacterVector::create("7", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79");
const CharacterVector v8 = CharacterVector::create("8", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89");
const CharacterVector v9 = CharacterVector::create("9", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99");

std::vector< std::string > concatsv(std::vector< std::string > x,
std::vector< std::string > y) {
  std::vector< std::string > v;
  v.reserve(x.size() + y.size());
  v.insert(v.end(), x.begin(), x.end());
  v.insert(v.end(), y.begin(), y.end());
  return v;
}

CharacterVector concatcv(CharacterVector cvx,
CharacterVector cvy) {
  std::vector< std::string > v;
  std::vector< std::string > x = as<std::vector< std::string > >(cvx);
  std::vector< std::string > y = as<std::vector< std::string > >(cvy);
  v = concatsv(x,y);
  return wrap(v);
}

CharacterVector MakeVector() {
  CharacterVector vv = vbase;

  for (int i=0; i<10; ++i) {
    for (int j=0; j<10; ++j) {
      char buffer[2];
      sprintf(buffer, "%02d", i+j*10);
      if (i+j!=0) {
        vv.push_back(buffer);
      }
    }
  }
  return(vv);
}

const CharacterVector vv = MakeVector();

//' @name icd9ExpandMinor
//' @title Expand a root 'minor' part of code into all possible children.
//' @examples
//'
//' microbenchmark(icd9:::icd9ExpandMinor("7", FALSE), icd9:::icd9ExpandMinor_R("7", FALSE))
//' microbenchmark(icd9:::icd9ExpandMinor("", FALSE), icd9:::icd9ExpandMinor_R("", FALSE))
//' microbenchmark(icd9:::icd9ExpandMinor("7", TRUE), icd9:::icd9ExpandMinor_R("7", TRUE))
//' microbenchmark(icd9:::icd9ExpandMinor("", TRUE), icd9:::icd9ExpandMinor_R("", TRUE))
//'
// [[Rcpp::export]]
CharacterVector icd9ExpandMinor(std::string x, bool isE) {
  //std::string x = as<std::string>(y[0]);
  if (!isE) {
    switch (x.size()) {
      case 0:
      return vv;
      case 1:
      switch (x.at(0)) {
        case '0':
        return v0;
        case '1':
        return v1;
        case '2':
        return v2;
        case '3':
        return v3;
        case '4':
        return v4;
        case '5':
        return v5;
        case '6':
        return v6;
        case '7':
        return v7;
        case '8':
        return v8;
        case '9':
        return v9;
        default:
        std::cout << "unrecognized char\n";
      }
      case 2:
      return wrap(x);
      default:
      std::cout << "minor of >2 characters received by icd9ExpandMinor\n";
    }
  } else {
    // is E code, so minor is just one character
    switch (x.size()) {
      case 0:
      return vbase;
      case 1:
      return x;
      default:
      std::cout << "too many characters in E code minor part\n";
    }
  }
}

//' @title Expand 5 character form 'short' ICD9 to all possible sub codes
//' @description Much faster in C++, but gains diminished with increasing numbers of input shorts.
//' @rdname icd9ChildrenShort
//' @template icd9-short
//' @keywords manip
//' @family ICD-9 ranges
//' @examples
//'
//' library(microbenchmark)
//' "400" %i9s% "410" -> shorts
//' microbenchmark(icd9:::icd9ChildrenShort(shorts), icd9:::icd9ChildrenShort_R(shorts), times = 5)
//'
//' @export
// [[Rcpp::export]]
CharacterVector icd9ChildrenShort(CharacterVector icd9Short, bool onlyReal = false) {
  CharacterVector out;
  if (icd9Short.size() == 0) { return out; }
  List parts = icd9ShortToParts(icd9Short, "");
  CharacterVector mjr = parts[0];
  CharacterVector mnr = parts[1];
  CharacterVector::iterator itmjr; // iterator seems to be a CharacterVector of length 1
  CharacterVector::iterator itmnr;
  for (itmjr = mjr.begin(), itmnr = mnr.begin();
  itmjr != mjr.end();
  ++itmjr, ++itmnr) {
    //std::string thismjr = *itmjr;
    //std::vector< std::string > thismjr = as<std::vector< std::string > >(*itmjr);
    std::string thismjr = as<std::string >(*itmjr);
    //bool isE = icd9IsE(thismjr)[0];
    bool isE = thismjr.find_first_of("Ee") != std::string::npos;
    std::string thismnr = as<std::string >(*itmnr);

    CharacterVector newminors = icd9ExpandMinor(thismnr, isE);

    // push back slower, but difficult to predict size of output
    CharacterVector newshort = icd9MajMinToShort(thismjr, newminors);

    // can only push_back or insert one at a time
    for (CharacterVector::iterator itnew = newshort.begin(); itnew != newshort.end(); ++itnew) {
      out.push_back(*itnew);
    }
  }
  if (onlyReal) {
    const Environment env("package:icd9");
    List icd9Hierarchy = env["icd9Hierarchy"]; // TODO: unnecessary copy?
    CharacterVector out_real = intersect(out, as<CharacterVector>(icd9Hierarchy["icd9"]));
    return out_real;
  }
  return out;
}

//TODO export to public in R
// [[Rcpp::export]]
CharacterVector icd9ChildrenDecimal(CharacterVector icd9Decimal, bool onlyReal = false) {
  //  CharacterVector out = icd9ChildrenShort(
  //TODO icd9DecimalToShort(icd9Decimal),
  //    onlyReal);
  //  return icd9ShortToDecimal(out);
}

//' @title extract major part from short or decimal ICD-9 code
//' @description Simply extracts parts, then returns only the major part in a
//'   character vector
//' @template icd9-any
//' @template isShort
//' @template invalid
//' @return character vector
// [[Rcpp::export]]
CharacterVector icd9GetMajor(CharacterVector icd9, bool isShort) {
  if (isShort) {
    List parts = icd9ShortToParts(icd9);
    return as<CharacterVector>(parts[0]);
  }
  stop("not implemented");
}
