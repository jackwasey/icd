// use Rcpp11 here if we want: //// [[plugins(cpp11)]]
// Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

// include boost
// [[depends(BH)]]

// [[Rcpp::interfaces(r, cpp)]]
// and use them (does this need two compilation steps?)
// [[Rcpp::depends(icd9)]]

// TODO: Don't use push_back on Rcpp types. The way Rcpp vectors are currently implemented this requires copying all of the data each time. This is a very expensive operation.

#include <Rcpp.h>
//#include <sstream>
#include <string>
#include <iostream>
#include <boost/algorithm/string/trim.hpp>
#include <icd9.h>

using namespace Rcpp;

// from https://github.com/jjallaire/RcppStrings/blob/master/src/Trim.cpp
namespace {
  // Generic character vector transformation logic
  typedef void (*Transformer)(std::string& str, const std::locale&);
  CharacterVector stringTransform(CharacterVector input, Transformer transform) {
    const std::locale& locale = std::locale();
    CharacterVector output(input.size());
    for (int i = 0; i<input.size(); i++) {
      std::string str(input[i]);
      transform(str, locale);
      output[i] = str;
    }
    return output;
  }
} // anonymous namespace

//' Trim whitespace from start and/or end of string
//'
//' @usage trim(input, side = "both")
//' @param input input character vector
//' @param side side on which whitespace is removed (left, right, or both)
//' @return character vector with leading and trailing whitespace removed
//' @examples
//' \dontrun{
//' trimString("\n\nString with trailing and leading white space \t")
//' }
// [[Rcpp::export("stringTrim")]]
CharacterVector stringTrim(CharacterVector input, std::string side = "both") {
  // select correct transformation
  Transformer transform = NULL;
  if (side == "both")
  transform = boost::algorithm::trim<std::string>;
  else if (side == "left")
  transform = boost::algorithm::trim_left<std::string>;
  else if (side == "right")
  transform = boost::algorithm::trim_right<std::string>;
  else {
    // we need to fix a bug in Rcpp modules to allow throwing exceptions
    // from module functions. for now we just warn and return the input
    stop("Unrecognized side argument '" + side + "'");
    return input; // keep compiler happy
  }
  // perform the transformation
  return stringTransform(input, transform);
}

// [[Rcpp::export]]
CharacterVector icd9MajMinToCode( CharacterVector mjr, CharacterVector mnr, bool isShort ) {

  CharacterVector out;
  CharacterVector::iterator j = mjr.begin();
  CharacterVector::iterator n = mnr.begin();

  for(; j != mjr.end() && n != mnr.end(); ++j, ++n) {
    //std::string mjrelem = as<std::string>(*j);
    String mjrelem = *j;
    if (mjrelem == NA_STRING) {
      out.push_back(NA_STRING);
      continue;
    }
    // work around Rcpp bug with push_front: convert to string just for this
    std::string smj = std::string(mjrelem.get_cstring());
    switch (strlen(mjrelem.get_cstring())) {
      case 0:
      out.push_back(NA_STRING);
      continue;
      case 1:
      if (!icd9::icd9IsASingleVE(mjrelem)) {
        smj.insert(0, "00");
      }
      break;
      case 2:
      if (!icd9::icd9IsASingleVE(mjrelem)) {
        smj.insert(0, "0");
      } else {
        smj.insert(1, "0");
      }
      // default: // major is 3 (or more) chars already
    }
    String mnrelem = *n;
    if (mnrelem == NA_STRING) {
      //out.push_back(mjrelem);
      out.push_back(smj);
      continue;
    }
    // this can probably be done more quickly:
    //std::string smj(mjrelem);
    if (!isShort && mnrelem != "") { smj.append("."); }
    smj.append(mnrelem);
    out.push_back(smj);

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

//' @name icd9MajMinToParts
//' @title icd9MajMinToParts
//' @description Convert vectors of major and minor components into a data frame with a column for major and a column for minor.
//' This is 20x faster in this C++ implementation, and important for a very frequently used function.
//' @import Rcpp
//' @export
// [[Rcpp::export]]
List icd9MajMinToParts(CharacterVector mjr, CharacterVector mnr) {
  List returned_frame = List::create(
    _["major"] = mjr,
    _["minor"] = mnr);

    // TODO: can do this with lists, ?no need for a data frame
    StringVector sample_row = returned_frame(0);
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

//' @name icd9ShortToParts
//' @title extract major and minor parts of a decimal ICD-9 code
//' @description accepts Vxxxx Exxxx or xxxxx. This version just beats native R, but lowerhead for calls. If minorEmpty is NA, it must be an NA_character_ (for now).
//' @template icd9-short
//' @param minorEmpty vector of length one, to be used in place of
//' minor part of zero. Defaults to ""
//' @template invalid
//' @return data.frame with two columns. At least the minor part must be
//' character, because "03" is different to "3", but "30" is the same as "3"
//' @keywords manip
//' @export
// [[Rcpp::export]]
List icd9ShortToParts(CharacterVector icd9Short, String minorEmpty = "") {

  CharacterVector mjr(icd9Short.size());
  CharacterVector mnr(icd9Short.size());

  for (int i = 0; i < icd9Short.size(); ++i) {
    if (icd9Short[i] == NA_STRING) { NA_STRING; mnr[i] = NA_STRING; continue; }

    std::string s = as<std::string>(icd9Short[i]); // do i need to convert?

    // since we loop anyway, don't call vectorized trim
    boost::algorithm::trim(s); // minimal speed difference

    if (!icd9::icd9IsASingleE(s)) { // not an E code
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

    switch (s.size()) {
      case 2:
      case 3:
      case 4:
      mjr[i] = s.substr(0, s.size());
      mnr[i] = minorEmpty; break;
      case 5:
      mjr[i] = s.substr(0, 4);
      mnr[i] = s.substr(4, 1); break;
      default:
      mjr[i] = NA_STRING; mnr[i] = NA_STRING; continue;
    }
    } // E code
    //mjr[i] = icd9::icd9AddLeadingZeroesMajorSingle(mjr[i]); // or loop through them all again...
  } // for

  return icd9MajMinToParts(icd9::icd9AddLeadingZeroesMajor(mjr), mnr);
}

// [[Rcpp::export]]
List icd9DecimalToParts(CharacterVector icd9Decimal, String minorEmpty = "") {
  CharacterVector majors;
  CharacterVector minors;
  int ilen = icd9Decimal.length();

  if (ilen == 0) { return List::create(_["major"] = CharacterVector::create(),
  _["minor"] = CharacterVector::create()); }

  for (CharacterVector::iterator it = icd9Decimal.begin(); it != icd9Decimal.end(); ++it) {
    String strna = *it;
    if (strna == NA_STRING || strna == "") { majors.push_back(NA_STRING); minors.push_back(NA_STRING); continue; }
    std::string thiscode = as<std::string >(*it); // Rcpp::String doesn't implement many functions.
    boost::algorithm::trim(thiscode);
    std::size_t pos = thiscode.find(".");
    // substring parts
    std::string mjrin;
    String mnrout;
    if (pos != std::string::npos) {
      mjrin = thiscode.substr(0, pos);
      mnrout = thiscode.substr(pos+1);
    } else {
      mjrin = thiscode; mnrout = minorEmpty;
    }
    majors.push_back(icd9::icd9AddLeadingZeroesMajorSingle(mjrin));
    minors.push_back(mnrout);
  }
  return List::create(_["major"] = majors, _["minor"] = minors);
}

//' @name icd9ShortToDecimal
//' @title icd9ShortToDecimal
//' @export
// [[Rcpp::export]]
CharacterVector icd9ShortToDecimal(CharacterVector x) {
  return icd9PartsToDecimal(icd9ShortToParts(x));
}

//' @name icd9DecimalToShort
//' @title icd9ShortToDecimal
//' @export
// [[Rcpp::export]]
CharacterVector icd9DecimalToShort(CharacterVector x) {
  return icd9PartsToShort(icd9DecimalToParts(x));
}

// this is simplest just to hard-code
const CharacterVector vbase = CharacterVector::create("",  "0", "1", "2", "3", "4", "5",  "6",  "7",  "8", "9",  "00");
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
  v = concatsv(x,y); // TODO: must i copy?
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

  if (!isE) {
    switch (x.size()) {
      case 0: return vv;
      case 1:
      switch (x.at(0)) {
        case '0':        return v0;
        case '1':        return v1;
        case '2':        return v2;
        case '3':        return v3;
        case '4':        return v4;
        case '5':        return v5;
        case '6':        return v6;
        case '7':        return v7;
        case '8':        return v8;
        case '9':        return v9;
        default:        std::cout << "unrecognized char\n";
      }
      case 2: return wrap(x);
      default: std::cout << "minor of >2 characters received by icd9ExpandMinor\n";
    }
  } else {
    // is E code, so minor is just one character
    switch (x.size()) {
      case 0:      return CharacterVector::create("", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9");
      case 1:      return x;
      //default:      std::cout << "too many characters in E code minor part\n";
    }
  }
  return(NA_STRING); // should never get here
}

//' @title Expand 5 character form 'short' ICD9 to all possible sub codes
//' @description Much faster in C++, but gains diminished with increasing numbers of input shorts, probably because I am copying memory unnecessarily.
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
  std::vector< std::string > out; // we are never going to put NAs in the output?
  if (icd9Short.size() == 0) return wrap(out);
  List parts = icd9ShortToParts(icd9Short, "");
  CharacterVector mjr = parts[0];
  CharacterVector mnr = parts[1];
  CharacterVector::iterator itmjr; // iterator seems to be a CharacterVector of length 1
  CharacterVector::iterator itmnr;
  for (itmjr = mjr.begin(), itmnr = mnr.begin();
  itmjr != mjr.end();
  ++itmjr, ++itmnr) {
    std::string thismjr = as<std::string >(*itmjr);
    std::string thismnr = as<std::string >(*itmnr);

    CharacterVector newminors = icd9ExpandMinor(thismnr, icd9::icd9IsASingleE(thismjr));

    // push back slower, but difficult to predict size of output
    std::vector< std::string > newshort = as<std::vector< std::string > >(icd9MajMinToShort(thismjr, newminors));

    // std insert is a thousand times faster than looping through CharacterVector and push_backing
    out.insert(out.end(), newshort.begin(), newshort.end());
  }
  if (onlyReal) {
    const Environment env("package:icd9");
    List icd9Hierarchy = env["icd9Hierarchy"]; // TODO: unnecessary copy?
    std::vector< std::string > out_real;
    std::vector< std::string > reals = as<std::vector< std::string > >(icd9Hierarchy["icd9"]);
    // sort so set_intersection works:
    std::sort(out.begin(), out.end());
    std::sort(reals.begin(), reals.end());
    std::set_intersection(out.begin(), out.end(),
    reals.begin(), reals.end(),
    std::back_inserter(out_real));
    return wrap(out_real);
  }
  return wrap(out);
}

//' @rdname icd9ChildrenDecimal
//' @template icd9-decimal
//' @template onlyReal
//' @family ICD-9 ranges
//' @keywords internal
// [[Rcpp::export]]
CharacterVector icd9ChildrenDecimal(CharacterVector icd9Decimal, bool onlyReal = false) {
  CharacterVector shrt = icd9::icd9DecimalToShort(icd9Decimal);
  CharacterVector kids = icd9::icd9ChildrenShort(shrt, onlyReal);
  return icd9::icd9ShortToDecimal(kids);
}

//' @rdname icd9ChildrenShort
//' @template icd9-any
//' @template isShort
//' @template onlyReal
//' @family ICD-9 ranges
//' @keywords internal
// [[Rcpp::export]]
CharacterVector icd9Children(CharacterVector icd9, bool isShort, bool onlyReal = false) {
  if (isShort) return(icd9::icd9ChildrenShort(icd9, onlyReal));
  return(icd9::icd9ChildrenDecimal(icd9, onlyReal));
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
    return as<CharacterVector>(icd9ShortToParts(icd9)[0]);
  }
  return as<CharacterVector>(icd9DecimalToParts(icd9)[0]);


}

// zero-pad a single major
// [[Rcpp::export]]
String icd9AddLeadingZeroesMajorSingle(String ms) {
  if (ms == NA_STRING) { return(NA_STRING); }
  std::string m(ms);
  if (!icd9::icd9IsASingleVE(ms)) {
    switch (strlen(ms.get_cstring())) {
      case 0: return(NA_STRING);
      case 1: return("00" + m);
      case 2: return("0" + m);
      case 3: return(m);
    }
  } else {
    switch (strlen(ms.get_cstring())) {
      case 1: return(NA_STRING);
      case 2:
      if (icd9::icd9IsASingleV(m)) {
        m.insert(1, "0"); return(m);
      } else {
        m.insert(1, "00"); return(m);
      }
      case 3:
      if (icd9::icd9IsASingleV(m)) {
        return(m);
      } else {
        m.insert(1, "0"); return(m);
      }
      case 4:
      if (icd9::icd9IsASingleE(m)) return(m);
    }
  }
  return NA_STRING;
}

//' @rdname icd9AddLeadingZeroes
//' @description three digit codes are returned unchanged, one and two digit
//'   codes are preceded by 00 or 0.
// [[Rcpp::export]]
CharacterVector icd9AddLeadingZeroesMajor(CharacterVector mjr) {
  return(sapply(mjr, icd9AddLeadingZeroesMajorSingle));
}

//' @rdname icd9AddLeadingZeroes
//' @description Non-decimal ICD-9 codes with length<5 are often ambiguous. E.g.
//'   100 could be 1.00 10.0 or 100 if coded incorrectly. We must assume 100 is
//'   really 100
//' @template icd9-short
// [[Rcpp::export]]
CharacterVector icd9AddLeadingZeroesShort(CharacterVector icd9Short) {
  List parts = icd9ShortToParts(icd9Short);
  parts["major"] = icd9AddLeadingZeroesMajor(parts["major"]);
  return icd9PartsToShort(parts);
}

// [[Rcpp::export]]
CharacterVector icd9AddLeadingZeroesDecimal(CharacterVector icd9Decimal) {
  List parts = icd9DecimalToParts(icd9Decimal);
  parts["major"] = icd9AddLeadingZeroesMajor(as<CharacterVector>(parts["major"]));
  return icd9PartsToDecimal(parts);
}

// [[Rcpp::export]]
CharacterVector icd9AddLeadingZeroes(CharacterVector icd9, bool isShort) {
  if (isShort) return icd9AddLeadingZeroesShort(icd9);
  return icd9AddLeadingZeroesDecimal(icd9);
}

//' @title match ICD9 codes
//' @aliases "%i9in%"
//' @description Finds children of ricd9Reference and looks for icd9 in the
//'   resulting vector.  It is a glorified %in% function.
//' @templateVar icd9AnyName "icd9,icd9Reference"
//' @template icd9-any
//' @template isShort
//' @param isShortReference logical, see argument \code{isShort}
//' @template invalid
//' @return logical vector of which icd9 match or are subcategory of
//'   icd9Reference
//' @keywords internal
// [[Rcpp::export]]
LogicalVector icd9InReferenceCode(CharacterVector icd9, CharacterVector icd9Reference,
bool isShort = true,
bool isShortReference = true) {

  CharacterVector x = icd9AddLeadingZeroes(icd9, isShort);
  CharacterVector y = icd9Children(icd9Reference, isShortReference);
  // Rcpp match is not quite as good as R:
  LogicalVector res = !is_na(match(x, y));
  return res;
}
