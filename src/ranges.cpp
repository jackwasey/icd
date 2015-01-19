// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <string>
#include <iostream>
#include <icd9.h>
using namespace Rcpp;

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

// horrible one-off code to pre-generate all the minor codes
CharacterVector MakeAllMinors() {
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

const CharacterVector vv = MakeAllMinors();

//' @name icd9ExpandMinor
//' @title expand decimal part of ICD-9 code to cover all possible sub-codes
//' @description Accepts a single number or character input starting point for
//'   generation of all possible decimal parts of ICD9 code. e.g. giving an empty
//'   input will fill out 111 combinations, e..g .1 .11 .12 .... .2 ....
//' @template minor
//' @param isE single logical, which if TRUE, treats the minor as part of an E
//'   code (which is one character), as opposed to a V or numeric-only code,
//'   which is two character. No default.
//' @examples
//' \dontrun{
//'   icd9ExpandMinor(isE = FALSE) # return all possible decimal parts of ICD9 codes (111 in total)
//'   }
//'   icd9ExpandMinor("1") # "1"  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
//' @return NA for invalid minor, otherwise a vector of all possible (perhaps non-existent) sub-divisions.
//' @family ICD-9 ranges
//' @keywords internal manip
// [[Rcpp::export]]
CharacterVector icd9ExpandMinor(std::string mnr, bool isE) {

  if (!isE) {
    switch (mnr.size()) {
      case 0: return vv;
      case 1:
      switch (mnr.at(0)) {
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
      case 2: return wrap(mnr);
      default: std::cout << "minor of >2 characters received by icd9ExpandMinor\n";
    }
  } else {
    // is E code, so minor is just one character
    switch (mnr.size()) {
      case 0:      return CharacterVector::create("", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9");
      case 1:      return mnr;
      //default:      std::cout << "too many characters in E code minor part\n";
    }
  }
  return(NA_STRING); // should never get here
}

//' @title Expand 5 character form 'short' ICD9 to all possible sub codes
//' @description Much faster in C++, but gains diminished with increasing numbers of input shorts, probably because I am copying memory unnecessarily.
//' @template icd9-any
//' @template icd9-short
//' @template icd9-decimal
//' @keywords manip
//' @family ICD-9 ranges
//' @examples
//' library(magrittr)
//' icd9ChildrenShort("10201", FALSE) # no children other than self
//' icd9ChildrenShort("10201", TRUE) # empty list because 102.01 is not a meaningful code
//' icd9ChildrenShort("003", TRUE) %>% icd9ExplainShort(condense = FALSE)
//' icd9ChildrenDecimal("100.0")
//' icd9ChildrenDecimal("100.00")
//' icd9ChildrenDecimal("2.34")
//'
//' @export
// [[Rcpp::export]]
CharacterVector icd9Children(CharacterVector icd9, bool isShort, bool onlyReal = false) {
  if (isShort) return(icd9::icd9ChildrenShort(icd9, onlyReal));
  return(icd9::icd9ChildrenDecimal(icd9, onlyReal));
}

//' @rdname icd9Children
//' @export
// [[Rcpp::export]]
CharacterVector icd9ChildrenShort(CharacterVector icd9Short, bool onlyReal = false) {
  std::set< std::string > out; // we are never going to put NAs in the output?
  if (icd9Short.size() == 0) return wrap(out);
  List parts = icd9::icd9ShortToParts(icd9Short, "");
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
    std::vector< std::string > newshort = as<std::vector< std::string > >(icd9::icd9MajMinToShort(thismjr, newminors));

    // std insert is a thousand times faster than looping through CharacterVector and push_backing
    out.insert(newshort.begin(), newshort.end());
  }
  if (onlyReal) {
    const Environment env("package:icd9");
    List icd9Hierarchy = env["icd9Hierarchy"]; // TODO: unnecessary copy?
    std::set< std::string > out_real;
    std::vector< std::string > tmp = as<std::vector< std::string > >(icd9Hierarchy["icd9"]);
    std::set< std::string > reals(tmp.begin(), tmp.end());
    std::set_intersection(out.begin(), out.end(), reals.begin(), reals.end(), std::inserter(out_real, out_real.begin()));
    return wrap(out_real);
  }
  return wrap(out);
}

//' @rdname icd9Children
//' @export
// [[Rcpp::export]]
CharacterVector icd9ChildrenDecimal(CharacterVector icd9Decimal, bool onlyReal = false) {
  CharacterVector shrt = icd9::icd9DecimalToShort(icd9Decimal);
  CharacterVector kids = icd9::icd9ChildrenShort(shrt, onlyReal);
  return icd9::icd9ShortToDecimal(kids);
}

//' @title match ICD9 codes
//' @description Finds children of \code{icd9Reference} and looks for \code{icd9} in the
//'   resulting vector.
//' @templateVar icd9AnyName "icd9,icd9Reference"
//' @template icd9-any
//' @template isShort
//' @param isShortReference logical, see argument \code{isShort}
//' @template invalid
//' @return logical vector of which icd9 match or are subcategory of
//'   \code{icd9Reference}
//' @keywords internal
// [[Rcpp::export]]
LogicalVector icd9InReferenceCode(CharacterVector icd9, CharacterVector icd9Reference,
bool isShort,
bool isShortReference = true) {

  CharacterVector x = icd9::icd9AddLeadingZeroes(icd9, isShort);
  if (!isShort)
    x = icd9::icd9DecimalToShort(x);

  CharacterVector y = icd9Children(icd9Reference, isShortReference);
  if (!isShortReference)
    y = icd9::icd9DecimalToShort(y);
  // Rcpp match is not quite as good as R:
  LogicalVector res = !is_na(match(x, y));
  return res;
}
