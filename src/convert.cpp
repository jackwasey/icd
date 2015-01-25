// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <icd9.h>
using namespace Rcpp;

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9MajMinToCode( CharacterVector major, CharacterVector minor, bool isShort ) {

  CharacterVector out;
  CharacterVector::iterator j = major.begin();
  CharacterVector::iterator n = minor.begin();

  for(; j != major.end() && n != minor.end(); ++j, ++n) {
    //std::string majorelem = as<std::string>(*j);
    String majorelem = *j;
    if (majorelem == NA_STRING) {
      out.push_back(NA_STRING);
      continue;
    }
    // work around Rcpp bug with push_front: convert to string just for this
    std::string smj = std::string(majorelem.get_cstring());
    switch (strlen(majorelem.get_cstring())) {
      case 0:
      out.push_back(NA_STRING);
      continue;
      case 1:
      if (!icd9::icd9IsASingleVE(majorelem)) {
        smj.insert(0, "00");
      }
      break;
      case 2:
      if (!icd9::icd9IsASingleVE(majorelem)) {
        smj.insert(0, "0");
      } else {
        smj.insert(1, "0");
      }
      // default: // major is 3 (or more) chars already
    }
    String minorelem = *n;
    if (minorelem == NA_STRING) {
      //out.push_back(majorelem);
      out.push_back(smj);
      continue;
    }
    // this can probably be done more quickly:
    //std::string smj(majorelem);
    if (!isShort && minorelem != "") { smj.append("."); }
    smj.append(minorelem);
    out.push_back(smj);

  }
  // String chr_s = NA_STRING; // TODO

  return wrap(out);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9MajMinToShort(CharacterVector major, CharacterVector minor) {
  if ((major.size()!=1 && major.size() != minor.size()) ||
  (major.size()==1 && minor.size()==0)) {
    stop("icd9MajMinToShort, length of majors and minors must be equal.");
  }
  if (major.size() == 1) {
    CharacterVector newmajor(minor.size(), major[0]);
    return icd9MajMinToCode(newmajor, minor, true);
  }
  return icd9MajMinToCode(major, minor, true);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9MajMinToDecimal(CharacterVector major, CharacterVector minor) {
  return icd9MajMinToCode(major, minor, false);
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

//' @rdname convert
//' @export
// [[Rcpp::export]]
List icd9MajMinToParts(CharacterVector major, CharacterVector minor) {
  List returned_frame = List::create(
    _["major"] = major,
    _["minor"] = minor);

    // TODO: can do this with lists, ?no need for a data frame
    StringVector sample_row = returned_frame(0);
    IntegerVector row_names = seq_along(sample_row);
    returned_frame.attr("row.names") = row_names;
    returned_frame.attr("class") = "data.frame";

    return returned_frame;
}

// this is even faster, but loses some useful data frame features which cause test failure
// [[Rcpp::export]]
List icd9MajMinToParts_list(CharacterVector major, CharacterVector minor) {
  List out = List::create(
    _["major"] = major,
    _["minor"] = minor);

    return out;
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
List icd9ShortToParts(CharacterVector icd9Short, String minorEmpty = "") {

  CharacterVector major(icd9Short.size());
  CharacterVector minor(icd9Short.size());

  for (int i = 0; i < icd9Short.size(); ++i) {
    if (icd9Short[i] == NA_STRING) { NA_STRING; minor[i] = NA_STRING; continue; }

    std::string s = as<std::string>(icd9Short[i]);
    s = icd9::strim_cpp(s); // do i need to convert?

    if (!icd9::icd9IsASingleE(s)) { // not an E code
    switch (s.size()) {
      case 1:
      case 2:
      case 3:
      major[i] = s.substr(0, s.size());
      minor[minorEmpty];
      continue;
      case 4:
      case 5:
      major[i] = s.substr(0, 3);
      minor[i] = s.substr(3, s.size()-3);
      continue;
      default:
      major[i] = NA_STRING; minor[i] = NA_STRING; continue;
    }
    } else { // E code

    switch (s.size()) {
      case 2:
      case 3:
      case 4:
      major[i] = s.substr(0, s.size());
      minor[i] = minorEmpty; break;
      case 5:
      major[i] = s.substr(0, 4);
      minor[i] = s.substr(4, 1); break;
      default:
      major[i] = NA_STRING; minor[i] = NA_STRING; continue;
    }
    } // E code
    //major[i] = icd9::icd9AddLeadingZeroesMajorSingle(major[i]); // or loop through them all again...
  } // for

  return icd9MajMinToParts(icd9::icd9AddLeadingZeroesMajor(major), minor);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
List icd9DecimalToParts(CharacterVector icd9Decimal, String minorEmpty = "") {
  CharacterVector majors;
  CharacterVector minors;
  int ilen = icd9Decimal.length();

  if (ilen == 0) { return List::create(_["major"] = CharacterVector::create(),
  _["minor"] = CharacterVector::create()); }

  for (CharacterVector::iterator it = icd9Decimal.begin(); it != icd9Decimal.end(); ++it) {
    String strna = *it;
    if (strna == NA_STRING || strna == "") { majors.push_back(NA_STRING); minors.push_back(NA_STRING); continue; }
    std::string thiscode = as<std::string >(*it); // Rcpp::String doesn't implement many functions.
    thiscode = icd9::strim_cpp(thiscode); // TODO: update in place.
    std::size_t pos = thiscode.find(".");
    // substring parts
    std::string majorin;
    String minorout;
    if (pos != std::string::npos) {
      majorin = thiscode.substr(0, pos);
      minorout = thiscode.substr(pos+1);
    } else {
      majorin = thiscode; minorout = minorEmpty;
    }
    majors.push_back(icd9::icd9AddLeadingZeroesMajorSingle(majorin));
    minors.push_back(minorout);
  }
  return List::create(_["major"] = majors, _["minor"] = minors);
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
