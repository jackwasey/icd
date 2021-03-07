#include <Rcpp.h>
#include "sort.hpp"
using namespace Rcpp;

IntegerVector orderWorker(
    const CharacterVector& x,
    std::function <bool (String, String)> f
) {
  IntegerVector index =  Rcpp::no_init_vector(x.size());
  // R indexed
  std::iota(index.begin(), index.end(), 1);
  // stable sort give the same indices for identical values, although these may
  // not be what R would give.
  std::stable_sort(
    index.begin(),
    index.end(),
    [&](R_xlen_t a,
        R_xlen_t b) {
      return f(x[a - 1], x[b - 1]);
    }
  );
  return index;
}

LogicalVector compareVectorWorker(const StringVector& x,
                                  const StringVector& y,
                                  const std::function <bool (String, String)>& f) {
  if (x.size() != y.size()) stop("Both x and y must be same length");
  if (x.size() == 0) return LogicalVector(0);
  LogicalVector out(x.size());
  auto xit = x.cbegin();
  auto yit = y.cbegin();
  for (; xit != x.cend(); ++xit, ++yit) {
    const String& xx = *xit;
    const String& yy = *yit;
    auto n = std::distance(x.cbegin(), xit);
    if (xx == NA_STRING || yy == NA_STRING)
      out(n) = NA_LOGICAL;
    else
      out(n) = f(*xit, *yit);
  }
  return out;
}

// [[Rcpp::export(icd9_compare_rcpp)]]
bool icd9Compare(const String& a, const String& b) {
  if (b == NA_STRING && a != NA_STRING) {
    // even if x is also NA, we still return true
    return true;
  }
  if (a == NA_STRING) {
    // NA < NA is false and NA < any code is false
    return false;
  }
  const char *acs = a.get_cstring();
  const char *bcs = b.get_cstring();
  // most common is numeric, so deal with that first:
  if (*acs < 'A') return strcmp(acs, bcs) < 0;
  // if the second char is now  a number, then we can immediately return false
  if (*bcs < 'A') return false;
  // V vs E as first or both characters is next
  if (*acs == 'V' && *bcs == 'E') return true;
  if (*acs == 'E' && *bcs == 'V') return false;
  // now cover both V codes or both E codes
  return strcmp(acs, bcs) < 0;
}

// [[Rcpp::export(icd9_compare_vector_rcpp)]]
LogicalVector icd9CompareVector(const StringVector& x,
                                const StringVector& y) {
  return compareVectorWorker(x, y, icd9Compare);
}

// [[Rcpp::export(icd9_sort_rcpp)]]
CharacterVector icd9Sort(const CharacterVector &x) {
  CharacterVector y = clone(x);
  std::sort(y.begin(), y.end(), icd9Compare);
  return y;
}

// [[Rcpp::export(icd9_order_rcpp)]]
IntegerVector icd9Order(const CharacterVector& x) {
  return orderWorker(x, icd9Compare);
}

//std::vector<char> qfirst = {'C', 'D', 'M', 'Z'};
std::vector<std::string> qx  = {"C4A", "D3A", "M1A", "Z3A", "C7A", "C7B"};
std::vector<std::string> qb  = {"C4399999", "D3699999", "M0999999", "Z3699999", "C7599999", "C7A99999"};
std::vector<std::string> qa  = {"C44", "D37", "M10", "Z37", "C7B", "C76"};
std::vector<std::string> qbb = {"C4399999", "D3699999", "M0999999", "Z3699999", "C7599999", "C7599999"};
std::vector<std::string> qaa = {"C44", "D37", "M10", "Z37", "C76", "C76"};

////////////
// ICD-10 //
////////////

bool icd10cmCompareQuirk(
    const char* xstr,
    const char* ystr,
    const char *quirk,
    const char *beforeQuirk,
    const char *afterQuirk,
    const char *beforeBeforeQuirk,
    const char *afterAfterQuirk,
    bool& res) {
  bool mx = (xstr == quirk || strncmp(xstr, quirk, 3) == 0);
  bool my = (ystr == quirk || strncmp(ystr, quirk, 3) == 0);
  if (!mx && !my) {
    TRACE("icd10cmCompareQuirk !mx and !my");
    res = false;
    return false;
  }
  if (xstr == ystr) {
    TRACE("icd10cmCompareQuirk xstr == ystr");
    res = false;
    return true;
  }
  if (mx) {
    TRACE(quirk << " matched x");
    if (my) {
      TRACE(quirk << " also matched y for same quirk");
      res = strcmp(xstr, ystr) < 0;
      return true;
    }
    TRACE(quirk << " didn't match y");
    TRACE(quirk << " after x match falling through.\nx = " << xstr
                << "\nbeforeQuirk = " << beforeQuirk
                << "\nafterQuirk = " << afterQuirk
                << "\nbeforeBeforeQuirk = " << beforeBeforeQuirk
                << "\nafterAfterQuirk = " << afterAfterQuirk);
    if (strcmp(beforeQuirk, beforeBeforeQuirk)) {
      TRACE("beforeQuirk matches beforeBeforeQuirk");
      bool res_nested;
      return icd10cmCompareQuirk(xstr,
                                 ystr,
                                 quirk,
                                 beforeBeforeQuirk,
                                 afterQuirk,
                                 beforeBeforeQuirk,
                                 afterAfterQuirk,
                                 res_nested);
    }
    TRACE("Didn't do X recursion");
    res = strcmp(beforeQuirk, ystr) < 0;
    return true;
  }
  TRACE(quirk << " falling through. x = " << xstr
                   << ", afterQuirk = " << afterQuirk);
  if (strcmp(afterQuirk, afterAfterQuirk)) {
    bool res_nested2;
    return icd10cmCompareQuirk(xstr,
                               ystr,
                               quirk,
                               beforeQuirk,
                               afterAfterQuirk,
                               beforeBeforeQuirk,
                               afterAfterQuirk,
                               res_nested2);
  }
  TRACE("Didn't do Y recursion");
  res = strcmp(xstr, afterQuirk) < 0;
  return true;
}

// [[Rcpp::export(icd10cm_compare_c)]]
bool icd10cmCompareC(const char* xstr,
                     const char* ystr) {
  TRACE("icd10cmCompareC comparing " << xstr << " with " << ystr);
  const int i = strncmp(xstr, ystr, 1);
  TRACE("icd10cmCompareC first char: " << xstr << " vs " << ystr << " = " << i);
  // get out quick when possible
  if (i != 0) {
    TRACE("icd10cmCompareC First char differs so no need to do quirks");
#ifdef ICD_DEBUG_TRACE
    if (i < 0)
      TRACE("icd10cmCompareC found that " << xstr << " < " << ystr);
    else
      TRACE("icd10cmCompareC found that " << xstr << " > " << ystr);
#endif
    return i < 0;
  }
  // we know the first char is different. If one isn't in the quirk list, we can
  // return immediately
  const char x1 = *xstr;
  if (x1 != 'C' && x1 != 'D' && x1 != 'M' && x1 != 'Z') {
    return strcmp(xstr, ystr) < 0;
  }
  const char y1 = *ystr;
  if (y1 != 'C' && y1 != 'D' && y1 != 'M' && y1 != 'Z') {
    return strcmp(xstr, ystr) < 0;
  }
  // the quirks are all A or B in third position
  if (strlen(xstr) >= 3 &&
      strlen(ystr) >= 3 &&
      isdigit(xstr[2]) &&
      isdigit(ystr[2])) {
    TRACE("third character is digit, so cannot be quirk for xstr = " <<
      xstr << " and ystr = " << ystr);
    return strcmp(xstr, ystr) < 0;
  }
  TRACE("icd10cmCompareC found that first char equal for " <<
    xstr << " and " << ystr << ". Comparing quirky codes...");
  // in flat file, C4A is between 43 and 44. Definitive reference I am using is
  // the flat file with all the codes from CMS.
  bool qres;
  for (std::vector<std::string>::size_type j = 0; j != qa.size(); ++j) {
    TRACE("icd10cmCompareC Working on quirk: " << qx[j]);
    if (icd10cmCompareQuirk(xstr,
                            ystr,
                            qx[j].c_str(),
                            qb[j].c_str(),
                            qa[j].c_str(),
                            qbb[j].c_str(),
                            qaa[j].c_str(),
                            qres)
    )
      return qres;
  }
  TRACE("icd10cmCompareC falling back to strcmp");
  return strcmp(xstr, ystr) < 0;
}


// [[Rcpp::export(icd10cm_compare_rcpp)]]
bool icd10cmCompare(const String& x, const String& y) {
  // only return TRUE if x definitely comes before y (i.e. not equal and less)
  if (y == NA_STRING) {
    // even if x is also NA, we still return true
    TRACE("icd10cmCompare y is NA");
    return true;
  }
  if (x == NA_STRING) {
    TRACE("icd10cmCompare x is NA");
    // y is NOT NA, so x must go after, and is not equal
    return false;
  }
  return icd10cmCompareC(x.get_cstring(), y.get_cstring());
}





// [[Rcpp::export(icd10cm_compare_vector_rcpp)]]
LogicalVector icd10cmCompareVector(const StringVector& x,
                                   const StringVector& y) {
  return compareVectorWorker(x, y, icd10cmCompare);
}

// [[Rcpp::export(icd10cm_sort_rcpp)]]
CharacterVector icd10cmSort(const CharacterVector &x) {
  auto y = clone(x);
  std::sort(y.begin(), y.end(), icd10cmCompare);
  return y;
}

//' @title Order ICD-10-CM codes
//' @description Currently required for C7A, C7B (which fall after C80 nad before C81), and
//'   D3A, which falls after D48. C4A M1A Z3A are also problems within
//'   sub-chapters.
//' https://www.icd10data.com/ICD10CM/Codes/C00-D49
//' https://www.icd10data.com/ICD10CM/Codes/C00-D49/C43-C44
//' https://www.icd10data.com/ICD10CM/Codes/M00-M99/M05-M14
//' https://www.icd10data.com/ICD10CM/Codes/Z00-Z99/Z30-Z39
//' @keywords internal
//' @noRd
// [[Rcpp::export(icd10cm_order_rcpp)]]
IntegerVector icd10cmOrder(const CharacterVector& x) {
  return orderWorker(x, icd10cmCompare);
}
