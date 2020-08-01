#include "manip.h"
#include "convert.h"
#include "is.h"
#include <string.h> // for strlen

using namespace Rcpp;

//' Simpler add leading zeroes without converting to parts and back
//' @keywords internal manip
//' @noRd
// [[Rcpp::export]]
String icd9AddLeadingZeroesMajorSingle(String mjr) {
  if (mjr == NA_STRING) return (NA_STRING);
  std::string m(mjr);
  if (!icd9IsASingleVE(mjr.get_cstring())) {
    switch (strlen(mjr.get_cstring())) {
    case 0:
      return (NA_STRING);
    case 1:
      return ("00" + m);
    case 2:
      return ("0" + m);
    case 3:
      return (m);
    }
  } else {
    switch (strlen(mjr.get_cstring())) {
    case 1:
      return (NA_STRING);
    case 2:
      if (icd9IsASingleV(m.c_str())) {
        m.insert(1, "0");
        return (m);
      } else {
        m.insert(1, "00");
        return (m);
      }
    case 3:
      if (icd9IsASingleV(m.c_str())) {
        return (m);
      } else {
        m.insert(1, "0");
        return (m);
      }
    case 4:
      if (icd9IsASingleE(m.c_str())) return (m);
      // # nocov start
      // avoid fallthrough warning
      stop("Major length invalid");
    default:
      stop("Major length invalid");
      // # nocov end
    }
  }
  return NA_STRING;
}

// [[Rcpp::export]]
std::string icd9AddLeadingZeroesMajorSingleStd(std::string m) {
  const char* cs                   = m.c_str();
  const std::string::size_type len = m.length();
  if (!icd9IsASingleVE(cs)) {
    switch (len) {
    case 0:
      return ("");
    case 1:
      return ("00" + m);
    case 2:
      return ("0" + m);
    case 3:
      return (m);
    }
  } else {
    switch (len) {
    case 1:
      return ("");
    case 2:
      if (icd9IsASingleV(cs)) {
        m.insert(1, "0");
        return (m);
      } else {
        m.insert(1, "00");
        return (m);
      }
    case 3:
      if (icd9IsASingleV(cs)) {
        return (m);
      } else {
        m.insert(1, "0");
        return (m);
      }
    case 4:
      if (icd9IsASingleE(cs)) return (m);
    }
  }
  return "";
}

// [[Rcpp::export(icd9_add_leading_zeroes_major)]]
CV icd9AddLeadingZeroesMajor(CV mjr) { return sapply(mjr, icd9AddLeadingZeroesMajorSingle); }

//' @title Add leading zeroes to incomplete ICD-9 codes
//' @description Non-decimal ICD-9 codes with length<5 are often ambiguous. E.g.
//'   100 could be 1.00 10.0 or 100 if coded incorrectly. We must assume 100 is
//'   really 100
//' @param x Character vector of ICD-9 codes
//' @template short_code
//' @return character vector of ICD-9 codes with leading zeroes
//' @keywords internal manip
//' @noRd
// [[Rcpp::export(icd9_add_leading_zeroes_rcpp)]]
CV icd9AddLeadingZeroes(CV x, bool short_code) {
  if (short_code) {
    // a shortcut for when short codes is just to add the appropriate leading
    // zeros when the total length is <3. Even then decimal may be quicker by
    // converting from short than calculating by parts.
    List parts   = icd9ShortToParts(x, "");
    parts["mjr"] = icd9AddLeadingZeroesMajor(parts["mjr"]);
    return icd9PartsToShort(parts);
  } else {
    List parts   = icd9DecimalToParts(x);
    parts["mjr"] = icd9AddLeadingZeroesMajor(parts["mjr"]);
    return icd9PartsToDecimal(parts);
  }
}
