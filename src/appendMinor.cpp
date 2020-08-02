#include "icd_types.h"
#include "is.h"      // for icd9IsASingleVE
#include <algorithm> // for fill
#include <iterator>  // for distance
#include <string.h>  // for strlen
#include <string>    // for basic_string, operator!=
#include <vector>    // for vector, vector<>::iterator
#include <Rcpp.h>
typedef Rcpp::CharacterVector CV;
using namespace Rcpp;

//' Convert \code{mjr} and \code{mnr} vectors to single code
//' @template mjr
//' @template mnr
//' @template short_code
//' @return Character vector
//' @keywords internal manip
//' @noRd
// [[Rcpp::export]]
CV icd9MajMinToCode(const CV mjr, const CV mnr, const bool short_code) {
#ifdef ICD_DEBUG_TRACE
  Rcout << "icd9MajMinToCode: mjr.size() = " << mjr.size() << " and mnr.size() = " << mnr.size()
        << "\n";
#endif
  if (mjr.size() != mnr.size()) stop("major and minor lengths differ");
  std::vector<std::string> out(mjr.size());
  std::vector<char> out_is_na(mjr.size()); // Boolean in char
  CV::const_iterator j = mjr.cbegin();
  CV::const_iterator n = mnr.cbegin();
  for (; j != mjr.cend() && n != mnr.cend(); ++j, ++n) {
    String mjrelem = *j;
    if (CV::is_na(*j)) {
      out_is_na[std::distance(mjr.cbegin(), j)] = 1;
      continue;
    }
    const char* smj_c = mjrelem.get_cstring();
    std::string smj   = std::string(smj_c);
    switch (strlen(smj_c)) {
    case 0:
      out_is_na[std::distance(mjr.cbegin(), j)] = 1;
      continue;
    case 1:
      if (!icd9IsASingleVE(smj_c)) { smj.insert(0, "00"); }
      break;
    case 2:
      if (!icd9IsASingleVE(smj_c)) {
        smj.insert(0, "0");
      } else {
        smj.insert(1, "0");
      }
      // default: // mjr is 3 (or more) chars already
    }
    String mnrelem = *n;
    if (CV::is_na(*n)) { mnrelem = ""; }
    if (!short_code && mnrelem != "") { smj.append("."); }
    smj.append(mnrelem);
    out[std::distance(mjr.cbegin(), j)] = smj;
  }
  CV r_out = wrap(out);
#ifdef ICD_DEBUG_TRACE
  Rcout << "NA loop size: " << out_is_na.size() << "\n";
#endif
  for (auto i = out_is_na.cbegin(); i != out_is_na.cend(); ++i) {
#ifdef ICD_DEBUG_TRACE
    Rcout << "NA loop: " << std::distance(out_is_na.cbegin(), i) << "\n";
#endif
    if (*i == 0) continue;
    r_out[std::distance(out_is_na.cbegin(), i)] = NA_STRING;
  }
  return r_out;
}

// [[Rcpp::export]]
CV icd9MajMinToShort(const CV& mjr, const CV& mnr) {
#ifdef ICD_DEBUG_TRACE
  Rcout << "icd9MajMinToShort: mjr.size() = " << mjr.size() << " and mnr.size() = " << mnr.size()
        << "\n";
#endif
  if ((mjr.size() != 1 && mjr.size() != mnr.size()) || (mjr.size() == 1 && mnr.size() == 0)) {
    stop("Length of mjrs and mnrs must be equal, unless major length is one.");
  }
  if (mjr.size() == 1) {
#ifdef ICD_DEBUG_TRACE
    Rcout << "icd9MajMinToShort: mjr.size() = 1\n";
#endif
    CV newmjr(mnr.size(), mjr[0]);
    return icd9MajMinToCode(newmjr, mnr, true);
  }
  return icd9MajMinToCode(mjr, mnr, true);
}

// [[Rcpp::export]]
CV icd9MajMinToDecimal(const CV& mjr, const CV& mnr) { return icd9MajMinToCode(mjr, mnr, false); }
