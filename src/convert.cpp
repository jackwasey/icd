// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <icd9.h>
using namespace Rcpp;

// // temporarily move  @export to my shim
// [[Rcpp::export("icd9_MajMinToCode")]]
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
      // default: // mjr is 3 (or more) chars already
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

  return wrap(out);
}

// [[Rcpp::export("icd9_MajMinToShort")]]
CharacterVector icd9MajMinToShort(CharacterVector mjr, CharacterVector mnr) {
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

// [[Rcpp::export("icd9_MajMinToDecimal")]]
CharacterVector icd9MajMinToDecimal(CharacterVector mjr, CharacterVector mnr) {
  return icd9MajMinToCode(mjr, mnr, false);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9PartsToShort(List parts) {
  return icd9MajMinToCode(parts["major"], parts["minor"], true);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9PartsToDecimal(List parts) {
  return icd9MajMinToCode(parts["major"], parts["minor"], false);
}

// [[Rcpp::export("icd9_MajMinToParts")]]
List icd9MajMinToParts(CharacterVector mjr, CharacterVector mnr) {
  List returned_frame = List::create(
    _["major"] = mjr,
    _["minor"] = mnr);

    // TODO: can do this with lists, ?no need for a data frame
    StringVector sample_row = returned_frame(0);
    IntegerVector row_names = seq_along(sample_row);
    returned_frame.attr("row.names") = row_names;
    returned_frame.attr("class") = "data.frame";

    return returned_frame;
}

// this is even faster, but loses some useful data frame features which cause test failure
// [[Rcpp::export("icd9_MajMinToParts_list")]]
List icd9MajMinToParts_list(CharacterVector mjr, CharacterVector mnr) {
  List out = List::create(
    _["major"] = mjr,
    _["minor"] = mnr);

    return out;
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
List icd9ShortToParts(CharacterVector icd9Short, String minorEmpty = "") {

  CharacterVector mjr(icd9Short.size());
  CharacterVector mnr(icd9Short.size());

  for (int i = 0; i < icd9Short.size(); ++i) {
    if (icd9Short[i] == NA_STRING) { NA_STRING; mnr[i] = NA_STRING; continue; }

    std::string s = as<std::string>(icd9Short[i]);
    s = icd9::strim(s); // do i need to convert?

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

  return icd9MajMinToParts(icd9::icd9_AddLeadingZeroesMajor(mjr), mnr);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
List icd9DecimalToParts(CharacterVector icd9Decimal, String minorEmpty = "") {
  CharacterVector mjrs;
  CharacterVector mnrs;
  int ilen = icd9Decimal.length();

  if (ilen == 0) { return List::create(_["major"] = CharacterVector::create(),
  _["minor"] = CharacterVector::create()); }

  for (CharacterVector::iterator it = icd9Decimal.begin(); it != icd9Decimal.end(); ++it) {
    String strna = *it;
    if (strna == NA_STRING || strna == "") { mjrs.push_back(NA_STRING); mnrs.push_back(NA_STRING); continue; }
    std::string thiscode = as<std::string >(*it); // Rcpp::String doesn't implement many functions.
    thiscode = icd9::strim(thiscode); // TODO: update in place.
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
    mjrs.push_back(icd9::icd9_AddLeadingZeroesMajorSingle(mjrin));
    mnrs.push_back(mnrout);
  }
  return List::create(_["major"] = mjrs, _["minor"] = mnrs);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9ShortToDecimal(CharacterVector icd9Short) {
  return icd9PartsToDecimal(icd9ShortToParts(icd9Short));
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9DecimalToShort(CharacterVector icd9Decimal) {
  return icd9PartsToShort(icd9DecimalToParts(icd9Decimal));
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9GetMajor(CharacterVector icd9, bool isShort) {
  if (isShort) {
    return as<CharacterVector>(icd9ShortToParts(icd9)[0]);
  }
  return as<CharacterVector>(icd9DecimalToParts(icd9)[0]);
}
