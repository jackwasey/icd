#include "icd_types.h"
#include "is.h" // for icd9IsASingleVE
#include "local.h"
#include <Rcpp.h>
#include <algorithm> // for fill
#include <iterator>  // for distance
#include <string.h>  // for strlen
#include <string>    // for basic_string, operator!=
#include <vector>    // for vector, vector<>::iterator
typedef Rcpp::CharacterVector CV;
using namespace Rcpp;
// typedef Rcpp::Rostream<true> Rcout;

//' Convert \code{mjr} and \code{mnr} vectors to single code
//' @template mjr
//' @template mnr
//' @template short_code
//' @return Character vector
//' @keywords internal manip
//' @noRd
// [[Rcpp::export]]
Rcpp::CharacterVector icd9MajMinToCode(const Rcpp::CharacterVector& mjr,
                                       const Rcpp::CharacterVector& mnr,
                                       const bool short_code) {
  TRACE("icd9MajMinToCode: mjr.size() = " << mjr.size() << " and mnr.size() = " << mnr.size());
  if (mjr.size() != mnr.size()) Rcpp::stop("major and minor lengths differ");
  std::vector<std::string> out(mjr.size());
  std::vector<char> out_is_na(mjr.size()); // Boolean in char
  CV::const_iterator j = mjr.cbegin();
  CV::const_iterator n = mnr.cbegin();
  for (; j != mjr.cend() && n != mnr.cend(); ++j, ++n) {
    String mjrelem = *j;
    SEXP mjr_sexp  = mjrelem.get_sexp(); // CHARSXP
    if (mjr_sexp == NA_STRING) {
      out_is_na[std::distance(mjr.cbegin(), j)] = 1;
      continue;
    }
    std::string smj = (std::string)mjrelem;
    // TODO: possibly better to use Rcpp String here, which I think caches the
    // length look-up, but not sure whether it can do so across entry to C/C++
    // code.
    const char* smj_c = mjrelem.get_cstring();
    switch (smj.size()) {
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
  Rcpp::Rcout << "NA loop size: " << out_is_na.size() << "\n";
#endif
  for (auto i = out_is_na.cbegin(); i != out_is_na.cend(); ++i) {
#ifdef ICD_DEBUG_TRACE
    Rcpp::Rcout << "NA loop: " << std::distance(out_is_na.cbegin(), i) << "\n";
#endif
    if (*i == 0) continue;
    r_out[std::distance(out_is_na.cbegin(), i)] = NA_STRING;
  }
  return r_out;
}

// [[Rcpp::export]]
CV icd9MajMinToShort(const CV& mjr, const CV& mnr) {
  const R_xlen_t mjrsz = mjr.size();
  const R_xlen_t mnrsz = mnr.size();
  TRACE("icd9MajMinToShort: mjrsz, mnrsz");
  TRACE(mjrsz);
  TRACE(mnrsz);
  if ((mjrsz != 1 && mjrsz != mnrsz) || (mjrsz == 1 && mnrsz == 0)) {
    stop("Length of mjrs and mnrs must be equal, unless major length is one.");
  }
  TRACE("No problem with discordance of major and minor sizes.");
  if (mjrsz == 1) {
    TRACE("icd9MajMinToShort: mjr.size() is one");
    CV newmjr(mnrsz, mjr[0]);
    return icd9MajMinToCode(newmjr, mnr, true);
  }
  return icd9MajMinToCode(mjr, mnr, true);
}

// [[Rcpp::export]]
CV icd9MajMinToDecimal(const CV& mjr, const CV& mnr) { return icd9MajMinToCode(mjr, mnr, false); }
