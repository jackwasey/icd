#include "convert10.hpp"
#include "convert.hpp"
#include "icd_types.hpp"
extern "C" {
#include <cstddef> // for size_t
}
#include "appendMinor.hpp" // for icd9MajMinToCode
#include "is.hpp"          // for icd9IsASingleE, icd9IsAS...
#include "manip.hpp"       // for icd9AddLeadingZeroesMajor
#include "util.hpp"        // for strimCpp, trimLeftCpp
#include <string>        // for string

using namespace Rcpp;

// //Rcpp::export(icd10_short_to_parts_rcpp)]]

// [[Rcpp::export]]
Rcpp::List icd10ShortToParts(const Rcpp::CharacterVector & x, const Rcpp::String & mnrEmpty) {
  R_xlen_t i10sz = x.size();
  CV mjr(i10sz);
  CV mnr(i10sz);
  std::string::size_type sz;
  for (R_xlen_t i = 0; i != i10sz; ++i) {
    String thisShort = x[i];
    if (thisShort == NA_STRING) {
      mjr[i] = NA_STRING;
      mnr[i] = NA_STRING;
      continue;
    }
    std::string s(thisShort.get_cstring()); // maybe faster to use as?
    s  = strimCpp(s); // in place or rewrite? do this at all?
    sz = s.size();
    if (sz <= 3 && sz > 0) {
      mjr[i] = s.substr(0, sz);
      mnr[mnrEmpty];
    } else if (sz > 3) {
      mjr[i] = s.substr(0, 3);
      mnr[i] = s.substr(3, sz - 3);
    } else {
      mjr[i] = NA_STRING;
      mnr[i] = NA_STRING;
    }
  } // for
  return majMinToParts(mjr, mnr);
}

// [[Rcpp::export]]
List icd10DecimalToParts(const CV & x, const String & mnrEmpty) {
  CV mjrs;
  CV mnrs;
  R_xlen_t ilen = x.length();
  if (ilen == 0) {
    return List::create(_["mjr"] = CV::create(), _["mnr"] = CV::create());
  }

  for (CV::const_iterator it = x.begin(); it != x.end(); ++it) {
    String strna = *it;
    if (is_true(all(is_na(CV::create(strna)))) || strna == "") {
      mjrs.push_back(NA_STRING);
      mnrs.push_back(NA_STRING);
      continue;
    }
    std::string thiscode = as<std::string>(*it);
    thiscode = strimCpp(thiscode); // Updates 'thisccode' by reference, no copy
    std::size_t pos = thiscode.find('.');
    // substring parts
    std::string mjrin;
    String mnrout;
    if (pos != std::string::npos) {
      mjrin  = thiscode.substr(0, pos);
      mnrout = thiscode.substr(pos + 1);
    } else {
      mjrin  = thiscode;
      mnrout = mnrEmpty;
    }
    mjrs.push_back(mjrin);
    mnrs.push_back(mnrout);
  }
  return List::create(_["mjr"] = mjrs, _["mnr"] = mnrs);
}
