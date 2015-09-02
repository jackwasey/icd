// Copyright (C) 2014 - 2015  Jack O. Wasey
//
// This file is part of icd9.
//
// icd9 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// icd9 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with icd9. If not, see <http://www.gnu.org/licenses/>.

// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <is.h>
#include <util.h>
#include <manip.h>
#include <local.h>

using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector icd9MajMinToCode(const CharacterVector major,
		const CharacterVector minor, bool isShort) {
#ifdef ICD9_DEBUG_TRACE
  Rcout << "icd9MajMinToCode: major.size() = " << major.size()
        << " and minor.size() = " << minor.size() << "\n";
#endif

  if (major.size() != minor.size())
    //throw(std::length_error("major and minor lengths differ"));
    //Rcpp::stop("major and minor lengths differ"); // this fails the address sanitizer
    Rcpp::stop("major and minor lengths differ");

#ifdef ICD9_DEBUG_TRACE
  Rcout << "major and minor are the same?\n";
#endif

  CharacterVector out; // wish I could reserve space for this
	CharacterVector::const_iterator j = major.begin();
	CharacterVector::const_iterator n = minor.begin();

	for (; j != major.end() && n != minor.end(); ++j, ++n) {
		String mjrelem = *j;
		if (mjrelem == NA_STRING) {
			out.push_back(NA_STRING);
			continue;
		}
		// work around Rcpp bug with push_front: convert to string just for this
		const char* smj_c = mjrelem.get_cstring();
		std::string smj = std::string(smj_c);
		switch (strlen(smj_c)) {
		case 0:
			out.push_back(NA_STRING);
			continue;
		case 1:
			if (!icd9IsASingleVE(smj_c)) {
				smj.insert(0, "00");
			}
			break;
		case 2:
			if (!icd9IsASingleVE(smj_c)) {
				smj.insert(0, "0");
			} else {
				smj.insert(1, "0");
			}
			// default: // major is 3 (or more) chars already
		}
		String mnrelem = *n;
		if (mnrelem == NA_STRING) {
			//out.push_back(mjrelem);
			out.push_back(smj);
			continue;
		}
		// this can probably be done more quickly:
		//std::string smj(mjrelem);
		if (!isShort && mnrelem != "") {
			smj.append(".");
		}
		smj.append(mnrelem);
		out.push_back(smj);

	}

	return wrap(out);
}

// [[Rcpp::export]]
CharacterVector icd9MajMinToShort(const CharacterVector major,
		const CharacterVector minor) {
#ifdef ICD9_DEBUG_TRACE
	Rcout << "icd9MajMinToShort: major.size() = " << major.size()
	<< " and minor.size() = " << minor.size() << "\n";
#endif
	if ((major.size() != 1 && major.size() != minor.size())
			|| (major.size() == 1 && minor.size() == 0)) {
	  Rcpp::stop("icd9MajMinToShort, length of majors and minors must be equal, unless majors length is one.");
	}
	if (major.size() == 1) {
#ifdef ICD9_DEBUG_TRACE
		Rcout << "icd9MajMinToShort: major.size() = 1\n";
#endif
		CharacterVector newmajor(minor.size(), major[0]);
		return icd9MajMinToCode(newmajor, minor, true);
	}
	return icd9MajMinToCode(major, minor, true);
}

// [[Rcpp::export]]
CharacterVector icd9MajMinToDecimal(const CharacterVector major,
		const CharacterVector minor) {
	return icd9MajMinToCode(major, minor, false);
}

//' @rdname convert
//' @keywords internal manip
// [[Rcpp::export]]
CharacterVector icd9PartsToShort(const List parts) {
	return icd9MajMinToCode(parts["major"], parts["minor"], true);
}

//' @rdname convert
//' @keywords internal manip
// [[Rcpp::export]]
CharacterVector icd9PartsToDecimal(const List parts) {
	return icd9MajMinToCode(parts["major"], parts["minor"], false);
}

// [[Rcpp::export]]
List icd9MajMinToParts(const CharacterVector major,
		const CharacterVector minor) {
	List returned_frame = List::create(_["major"] = major, _["minor"] = minor);

	StringVector sample_row = returned_frame(0);
	IntegerVector row_names = seq_along(sample_row);
	returned_frame.attr("row.names") = row_names;
  // doesn't actually need a data frame, although it is barely distinguishable
  // from a list, and not costly to construct in this manner.
	returned_frame.attr("class") = "data.frame";

	return returned_frame;
}

// [[Rcpp::export]]
List icd9MajMinToParts_list(const CharacterVector major,
		const CharacterVector minor) {
	List out = List::create(_["major"] = major, _["minor"] = minor);
	return out;
}

//' @rdname convert
//' @keywords internal manip
// [[Rcpp::export]]
List icd9ShortToParts(const CharacterVector icd9Short, const String minorEmpty = "") {

	CharacterVector major(icd9Short.size());
	CharacterVector minor(icd9Short.size());

	for (int i = 0; i < icd9Short.size(); ++i) {
		if (CharacterVector::is_na(icd9Short[i])) {
			minor[i] = NA_STRING;
			continue;
		}

		std::string s = as<std::string>(icd9Short[i]);
		s = strimCpp(s);
		std::string::size_type sz = s.size();

		if (icd9IsASingleE(s.c_str())) { // E code
		  switch (sz) {
		  case 2:
		  case 3:
		  case 4:
		    major[i] = s.substr(0, sz);
		    minor[i] = minorEmpty;
		    break;
		  case 5:
		    major[i] = s.substr(0, 4);
		    minor[i] = s.substr(4, 1);
		    break;
		  default:
		    major[i] = NA_STRING;
		  minor[i] = NA_STRING;
		  continue;
		  }
		} else { // not an E code
			switch (sz) {
			case 1:
			case 2:
			case 3:
				major[i] = s.substr(0, sz);
				minor[minorEmpty];
				continue;
			case 4:
			case 5:
				major[i] = s.substr(0, 3);
				minor[i] = s.substr(3, sz - 3);
				continue;
			default:
				major[i] = NA_STRING;
				minor[i] = NA_STRING;
				continue;
			}
		}

	} // for

	return icd9MajMinToParts(icd9AddLeadingZeroesMajor(major), minor);
}

//' @rdname convert
//' @keywords internal manip
// [[Rcpp::export]]
List icd9DecimalToParts(const CharacterVector icd9Decimal,
		const String minorEmpty = "") {
	CharacterVector majors;
	CharacterVector minors;
	int ilen = icd9Decimal.length();

	if (ilen == 0) {
		return List::create(_["major"] = CharacterVector::create(), _["minor"] =
				CharacterVector::create());
	}

	for (CharacterVector::const_iterator it = icd9Decimal.begin();
			it != icd9Decimal.end(); ++it) {
		String strna = *it;
		if (strna == NA_STRING || strna == "") {
			majors.push_back(NA_STRING);
			minors.push_back(NA_STRING);
			continue;
		}
		// Rcpp::String doesn't implement many functions, so using STL. A FAST way
		// would be to use String's function get_cstring, and recode the trim
		// functions to take const char *. This would avoid the type change AND be
		// faster trimming.
		std::string thiscode = as<std::string>(*it);
		thiscode = strimCpp(thiscode); // This updates 'thisccode' by reference, no copy
		std::size_t pos = thiscode.find(".");
		// substring parts
		std::string majorin;
		String minorout;
		if (pos != std::string::npos) {
			majorin = thiscode.substr(0, pos);
			minorout = thiscode.substr(pos + 1);
		} else {
			majorin = thiscode;
			minorout = minorEmpty;
		}
		majors.push_back(icd9AddLeadingZeroesMajorSingle(majorin));
		minors.push_back(minorout);
	}
	return List::create(_["major"] = majors, _["minor"] = minors);
}

//' @title Convert ICD-9 codes between short and decimal forms
//' @template icd9-short
//' @template icd9-decimal
//' @export
// [[Rcpp::export]]
CharacterVector icd9ShortToDecimal(const CharacterVector icd9Short) {
	return icd9PartsToDecimal(icd9ShortToParts(icd9Short));
}

//' @rdname icd9ShortToDecimal
//' @export
// [[Rcpp::export]]
CharacterVector icd9DecimalToShort(const CharacterVector icd9Decimal) {
	return icd9PartsToShort(icd9DecimalToParts(icd9Decimal));
}

//' @title Get major (three-digit) part of ICD-9 codes
//' @template icd9-any
//' @template isShort
//' @export
// [[Rcpp::export]]
CharacterVector icd9GetMajor(const CharacterVector icd9, const bool isShort) {
	if (isShort) {
		return as<CharacterVector>(icd9ShortToParts(icd9)[0]);
	}
	return as<CharacterVector>(icd9DecimalToParts(icd9)[0]);
}
