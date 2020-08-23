#include "manip.h"
#include "convert.h"
#include "is.h"
#include "local.h"

using namespace Rcpp;

//' Simpler add leading zeroes without converting to parts and back
//'
//' @details Returning a 'String' is (probably?) going to require that Rcpp use R's C
//' interface to make a new \code{CHARSXP}, which involves enconding scanning, global
//' charsxp lookup. However, if we are actually changing a string, we must do this
//' if it is to be used back in R.
//' @keywords internal manip
//' @noRd
//[[Rcpp::export]]
Rcpp::String icd9AddLeadingZeroesMajorSingle(Rcpp::String mjr) {
  if (mjr == NA_STRING) return (NA_STRING);
  // TODO: really copy to a new string? Maybe Rcpp API can do this now without
  // additional copy.
  //
  // In a regular const char*[] for R-based CHARSXP anyway. the 'length' or
  // 'truelength' data bytes in a CHARSXP: used for the string, or always 1? (or
  // -1 for NA_STRING)
  // std::string m(mjr);
  R_xlen_t nc      = XLENGTH(mjr.get_sexp());
  const char* mstr = mjr.get_cstring();
  if (icd_likely(!icd9IsASingleVE(mstr))) {
    if (icd_likely(nc == 3)) return mjr;
    std::string out = std::string();
    switch (nc) {
    case 1:
      out = "00";
      break;
    case 2:
      out = "0";
      break;
    case 3:
      break;
    default:
      return NA_STRING;
    }
    return (out + mstr);
  } // V or E
  if (icd9IsASingleV(mjr.get_cstring())) {
    if (icd_likely(nc == 3)) return mjr;
    if (nc == 2) {
      std::string out(mjr);
      out.insert(1, "0");
      return out;
    }
    return NA_STRING;
  } // else  E...
  if (icd_likely(nc == 4)) return mjr;
  std::string out(mjr);
  switch (nc) {
  case 3: // e.g. E01 -> E001
    out.insert(1, "0");
    return (out);
  case 2: // e.g. E9 -> E009
    out.insert(1, "00");
    return (out);
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
    List parts   = icd9DecimalToParts(x, "");
    parts["mjr"] = icd9AddLeadingZeroesMajor(parts["mjr"]);
    return icd9PartsToDecimal(parts);
  }
}
