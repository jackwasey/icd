#include "local.h"                          // for icd_set
#include "ranges.h"
#include "icd_types.h"                      // for CV, VecStr, Str
#include <algorithm>                        // for set_intersection
#include <iterator>                         // for insert_iterator, inserter
#include <set>                              // for _Rb_tree_const_iterator, set
#include <string>                           // for basic_string
#include <vector>                           // for vector, vector<>::iterator
#include "appendMinor.h"                    // for icd9MajMinToShort, icd9Ma...
#include "convert.h"                        // for icd9DecimalToShort, icd9S...
#include "is.h"                             // for icd9IsASingleE

using namespace Rcpp;

//const std::vector<std::string> allMinorsStd{
CV allMinors = {
  "", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
  "00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
  "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
  "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
  "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
  "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
  "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
  "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
  "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
  "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
  "90", "91", "92", "93", "94", "95", "96", "97", "98", "99"
};

// [[Rcpp::export(icd9_expand_minor_wrap)]]
CV icd9ExpandMinor(const Str& mnr, bool isE) {
  if (!isE) {
    switch (mnr.size()) {
    case 0:
      return allMinors;
    case 1:
      switch (mnr.at(0)) {
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
        stop("unrecognized minor character");
      return CV::create();
      }
      break;
    case 2:
      return wrap(mnr);
    default:
      stop("minor of more than two characters");
    return CV::create();
    }
  } else {
    // is E code, so minor must be just one character
    switch (mnr.size()) {
    case 0:
      return CV::create("", "0", "1", "2", "3", "4", "5",
                        "6", "7", "8", "9");
    case 1:
      return mnr;
    default:
      stop("too many characters for an E code minor\n");
    }
  }
  return (NA_STRING); // should never get here
}

// [[Rcpp::export]]
CV icd9ChildrenShort(CV icd9Short,
                     const VecStr& icd9cmReal,
                     bool onlyReal) {
  std::set<Str> out;
  if (icd9Short.size() == 0) {
    icd9Short.attr("icd_short_diag") = true;
    return icd9Short;
  }
  List parts = icd9ShortToParts(icd9Short, "");
  CV mjr = parts[0];
  CV mnr = parts[1];
  CV::iterator itmjr = mjr.begin();
  CV::iterator itmnr = mnr.begin();
  for (; itmjr != mjr.end(); ++itmjr, ++itmnr) {
    Str thismjr = as<Str>(*itmjr);
    Str thismnr = as<Str>(*itmnr);
    const CV newminors = icd9ExpandMinor(thismnr, icd9IsASingleE(thismjr.c_str()));
    VecStr newshort = as<VecStr >(icd9MajMinToShort(thismjr, newminors));
    out.insert(newshort.begin(), newshort.end());
  }
  if (onlyReal) {
    std::set<Str> out_real;
    const std::set<Str> reals(icd9cmReal.begin(), icd9cmReal.end());
    std::set_intersection(out.begin(), out.end(),
                          reals.begin(), reals.end(),
                          std::inserter(out_real, out_real.begin()));
    out = out_real;
  }
  CV rcppOut = wrap(out);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}

// TODO: icd9ChildrenShortUnordered no NA version

// [[Rcpp::export]]
CV icd9ChildrenShortUnordered(CV icd9Short,
                              const VecStr& icd9cmReal,
                              bool onlyReal) {
  icd_set out;
  if (icd9Short.size() == 0) {
    icd9Short.attr("icd_short_diag") = true;
    return icd9Short;
  }
  List parts = icd9ShortToParts(icd9Short, "");
  CV mjr = parts[0];
  CV mnr = parts[1];
  CV::iterator itmjr = mjr.begin();
  CV::iterator itmnr = mnr.begin();
  for (; itmjr != mjr.end(); ++itmjr, ++itmnr) {
    Str thismjr = as<Str>(*itmjr);
    Str thismnr = as<Str>(*itmnr);
    const CV newminors = icd9ExpandMinor(thismnr, icd9IsASingleE(thismjr.c_str()));
    VecStr newshort = as<VecStr>(icd9MajMinToShort(thismjr, newminors));
    out.insert(newshort.begin(), newshort.end());
  }
  if (onlyReal) {
    icd_set out_real;
    icd_set reals(icd9cmReal.begin(), icd9cmReal.end());
    for (icd_set::iterator j = out.begin(); j != out.end(); ++j) {
      if (reals.find(*j) != reals.end())
        out_real.insert(*j);
    }
    out = out_real;
  }
  CV rcppOut = wrap(out);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}

// [[Rcpp::export]]
CV icd9ChildrenDecimalCpp(CV icd9Decimal,
                          const VecStr& icd9cmReal,
                          bool onlyReal) {
  CV shrt = icd9DecimalToShort(icd9Decimal);
  CV kids = icd9ChildrenShort(shrt, icd9cmReal, onlyReal);
  CV out = icd9ShortToDecimal(kids);
  out.attr("icd_short_diag") = false;
  return out;
}

// [[Rcpp::export]]
CV icd9ChildrenCpp(CV icd9, bool isShort,
                   const VecStr icd9cmReal,
                   bool onlyReal = true) {
  if (isShort)
    return icd9ChildrenShort(icd9, icd9cmReal, onlyReal);
  return icd9ChildrenDecimalCpp(icd9, icd9cmReal, onlyReal);
}
