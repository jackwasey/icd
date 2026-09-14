#include "ranges.h"
#include "appendMinor.h" // for icd9MajMinToShort, icd9Ma...
#include "convert.h"     // for icd9DecimalToShort, icd9S...
#include "icd_types.h"   // for CV, VecStr, Str
#include "is.h"          // for icd9IsASingleE
#include "local.h"       // for icd_set
#include <algorithm>     // for set_intersection
#include <iterator>      // for insert_iterator, inserter
#include <set>           // for _Rb_tree_const_iterator, set
#include <string>        // for basic_string
#include <vector>        // for vector, vector<>::iterator

using namespace Rcpp;

// const std::vector<std::string> allMinorsStd{
CV allMinors = {"",   "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",
                "9",  "00", "01", "02", "03", "04", "05", "06", "07", "08",
                "09", "10", "11", "12", "13", "14", "15", "16", "17", "18",
                "19", "20", "21", "22", "23", "24", "25", "26", "27", "28",
                "29", "30", "31", "32", "33", "34", "35", "36", "37", "38",
                "39", "40", "41", "42", "43", "44", "45", "46", "47", "48",
                "49", "50", "51", "52", "53", "54", "55", "56", "57", "58",
                "59", "60", "61", "62", "63", "64", "65", "66", "67", "68",
                "69", "70", "71", "72", "73", "74", "75", "76", "77", "78",
                "79", "80", "81", "82", "83", "84", "85", "86", "87", "88",
                "89", "90", "91", "92", "93", "94", "95", "96", "97", "98",
                "99"};

// [[Rcpp::export(icd9_expand_minor_rcpp)]]
CV icd9ExpandMinor(const Str &mnr, bool isE) {
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
      return CV::create("", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9");
    case 1:
      return mnr;
    default:
      stop("too many characters for an E code minor\n");
    }
  }
  return (NA_STRING); // should never get here
}

std::set<Str> icd9ChildrenShortWorker(const CV& icd9Short) {
  std::set<Str> out;
  List parts = icd9ShortToParts(icd9Short, "");
  CV mjr = parts[0];
  CV mnr = parts[1];
  CV::iterator itmjr = mjr.begin();
  CV::iterator itmnr = mnr.begin();
  for (; itmjr != mjr.end(); ++itmjr, ++itmnr) {
    Str thismjr = as<Str>(*itmjr);
    Str thismnr = as<Str>(*itmnr);
    const CV newminors =
      icd9ExpandMinor(thismnr, icd9IsASingleE(thismjr.c_str()));
    VecStr newshort = as<VecStr>(icd9MajMinToShort(thismjr, newminors));
    out.insert(newshort.begin(), newshort.end());
  }
  return out;
}

// [[Rcpp::export(icd9_children_short_undefined_rcpp)]]
CV icd9ChildrenShortUndefined(const CV& icd9Short) {
  if (icd9Short.size() == 0) {
    CV qout(0);
    qout.attr("icd_short_diag") = true;
    return qout;
  }
  std::set<Str> out = icd9ChildrenShortWorker(icd9Short);
  CV rcppOut = wrap(out);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}

// [[Rcpp::export(icd9_children_short_defined_rcpp)]]
CV icd9ChildrenShortDefined(const CV& icd9Short,
                            const VecStr &icd9cmReal) {
  if (icd9Short.size() == 0) {
    CV qout(0);
    qout.attr("icd_short_diag") = true;
    return qout;
  }
  std::set<Str> out = icd9ChildrenShortWorker(icd9Short);
  std::set<Str> out_real;
  const std::set<Str> reals(icd9cmReal.begin(), icd9cmReal.end());
  std::set_intersection(out.begin(),
                        out.end(),
                        reals.begin(),
                        reals.end(),
                        std::inserter(out_real, out_real.begin()));
  CV rcppOut = wrap(out_real);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}

// [[Rcpp::export(icd9_children_short_rcpp)]]
CV icd9ChildrenShort(const CV& icd9Short,
                     const VecStr& icd9cmReal,
                     bool onlyReal) {
  if (onlyReal)
    return icd9ChildrenShortDefined(icd9Short, icd9cmReal);
  else
    return icd9ChildrenShortUndefined(icd9Short);
}

icd_set icd9ChildrenShortUnorderedWorker(const CV& icd9Short) {
  icd_set out;
  List parts = icd9ShortToParts(icd9Short, "");
  CV mjr = parts[0];
  CV mnr = parts[1];
  CV::iterator itmjr = mjr.begin();
  CV::iterator itmnr = mnr.begin();
  for (; itmjr != mjr.end(); ++itmjr, ++itmnr) {
    Str thismjr = as<Str>(*itmjr);
    Str thismnr = as<Str>(*itmnr);
    const CV newminors =
      icd9ExpandMinor(thismnr, icd9IsASingleE(thismjr.c_str()));
    VecStr newshort = as<VecStr>(icd9MajMinToShort(thismjr, newminors));
    out.insert(newshort.begin(), newshort.end());
  }
  return out;
}

// [[Rcpp::export(icd9_children_short_unordered_undefined_rcpp)]]
CV icd9ChildrenShortUnorderedUndefined(const CV& icd9Short) {
  if (icd9Short.size() == 0) {
    CV qout(0);
    qout.attr("icd_short_diag") = true;
    return qout;
  }
  icd_set out = icd9ChildrenShortUnorderedWorker(icd9Short);
  CV rcppOut = wrap(out);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}

// [[Rcpp::export(icd9_children_short_unordered_defined_rcpp)]]
CV icd9ChildrenShortUnorderedDefined(const CV& icd9Short,
                                     const VecStr& icd9cmReal) {
  if (icd9Short.size() == 0) {
    CV qout(0);
    qout.attr("icd_short_diag") = true;
    return qout;
  }
  icd_set out = icd9ChildrenShortUnorderedWorker(icd9Short);
  icd_set out_real;
  icd_set reals(icd9cmReal.begin(), icd9cmReal.end());
  for (icd_set::iterator j = out.begin(); j != out.end(); ++j) {
    if (reals.find(*j) != reals.end()) out_real.insert(*j);
  }
  CV rcppOut = wrap(out_real);
  rcppOut.attr("icd_short_diag") = true;
  return rcppOut;
}

// [[Rcpp::export(icd9_children_short_unordered_rcpp)]]
CV icd9ChildrenShortUnordered(const CV& icd9Short,
                              const VecStr& icd9cmReal,
                              bool onlyReal) {
  if (onlyReal)
    return icd9ChildrenShortUnorderedDefined(icd9Short, icd9cmReal);
  else
    return icd9ChildrenShortUnorderedUndefined(icd9Short);
}

// [[Rcpp::export(icd9_children_decimal_rcpp)]]
CV icd9ChildrenDecimal(const CV& icd9Decimal,
                       const VecStr& icd9cmReal,
                       bool onlyReal) {
  // note that this uses icd9cm...
  CV shrt = icd9DecimalToShort(icd9Decimal);
  CV kids = icd9ChildrenShort(shrt, icd9cmReal, onlyReal);
  CV out = icd9ShortToDecimal(kids);
  out.attr("icd_short_diag") = false;
  return out;
}

// [[Rcpp::export(icd9_children_decimal_unordered_rcpp)]]
CV icd9ChildrenDecimalUnordered(const CV& icd9Decimal,
                                const VecStr& icd9cmReal,
                                bool onlyReal) {
  // note that this uses icd9cm, but usually doesn't matter, and definitely not
  // for 'undefined' children.
  CV shrt = icd9DecimalToShort(icd9Decimal);
  CV kids = icd9ChildrenShortUnordered(shrt, icd9cmReal, onlyReal);
  CV out = icd9ShortToDecimal(kids);
  out.attr("icd_short_diag") = false;
  return out;
}

// [[Rcpp::export(icd9_children_decimal_unordered_undefined_rcpp)]]
CV icd9ChildrenDecimalUnorderedUndefined(const CV& icd9Decimal) {
  CV shrt = icd9DecimalToShort(icd9Decimal);
  CV kids = icd9ChildrenShortUnorderedUndefined(shrt);
  CV out = icd9ShortToDecimal(kids);
  out.attr("icd_short_diag") = false;
  return out;
}

// [[Rcpp::export(icd9_children_decimal_unordered_defined_rcpp)]]
CV icd9ChildrenDecimalUnorderedDefined(const CV& icd9Decimal,
                                const VecStr& icd9cmReal) {
  CV shrt = icd9DecimalToShort(icd9Decimal);
  CV kids = icd9ChildrenShortUnorderedDefined(shrt, icd9cmReal);
  CV out = icd9ShortToDecimal(kids);
  out.attr("icd_short_diag") = false;
  return out;
}
// [[Rcpp::export(icd9_children_rcpp)]]
CV icd9Children(const CV& icd9,
                bool isShort,
                const VecStr& icd9cmReal,
                bool onlyReal) {
  if (isShort) return icd9ChildrenShort(icd9, icd9cmReal, onlyReal);
  return icd9ChildrenDecimal(icd9, icd9cmReal, onlyReal);
}
