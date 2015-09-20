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
#include "convert.h"
#include "local.h"
#include "util.h"
#include "is.h"
#include "manip.h"
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::CharacterVector icd9MajMinToCode(const Rcpp::CharacterVector major,
		const Rcpp::CharacterVector minor, bool isShort) {
#ifdef ICD9_DEBUG_TRACE
  Rcpp::Rcout << "icd9MajMinToCode: major.size() = " << major.size()
			<< " and minor.size() = " << minor.size() << "\n";
#endif

	if (major.size() != minor.size())
		Rcpp::stop("major and minor lengths differ");

#ifdef ICD9_DEBUG_TRACE
	Rcpp::Rcout << "major and minor are the same?\n";
#endif

	Rcpp::CharacterVector out; // wish I could reserve space for this
	Rcpp::CharacterVector::const_iterator j = major.begin();
	Rcpp::CharacterVector::const_iterator n = minor.begin();

	for (; j != major.end() && n != minor.end(); ++j, ++n) {
		Rcpp::String mjrelem = *j;
		if (mjrelem == NA_STRING) {
			out.push_back(NA_STRING);
			continue;
		}
		// work around Rcpp bug with push_front: convert to string just for this
		// TODO: try to do this with C string instead
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
		Rcpp::String mnrelem = *n;
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
	// ?slow step somewhere around here, with use of Rcpp::String, maybe in the wrapping? Maybe in the multiple push_back calls

	//return wrap(out);
	return out;
}

// [[Rcpp::export]]
Rcpp::CharacterVector icd9MajMinToShort(const Rcpp::CharacterVector major,
		const Rcpp::CharacterVector minor) {
#ifdef ICD9_DEBUG_TRACE
  Rcpp::Rcout << "icd9MajMinToShort: major.size() = " << major.size()
			<< " and minor.size() = " << minor.size() << "\n";
#endif
	if ((major.size() != 1 && major.size() != minor.size())
			|| (major.size() == 1 && minor.size() == 0)) {
		Rcpp::stop(
				"icd9MajMinToShort, length of majors and minors must be equal, unless majors length is one.");
	}
	if (major.size() == 1) {
#ifdef ICD9_DEBUG_TRACE
	  Rcpp::Rcout << "icd9MajMinToShort: major.size() = 1\n";
#endif
		Rcpp::CharacterVector newmajor(minor.size(), major[0]);
		return icd9MajMinToCode(newmajor, minor, true);
	}
	return icd9MajMinToCode(major, minor, true);
}

// [[Rcpp::export]]
Rcpp::CharacterVector icd9MajMinToDecimal(const Rcpp::CharacterVector major,
		const Rcpp::CharacterVector minor) {
	return icd9MajMinToCode(major, minor, false);
}

//' @rdname convert
//' @keywords internal manip
// [[Rcpp::export]]
Rcpp::CharacterVector icd9PartsToShort(const Rcpp::List parts) {
	return icd9MajMinToCode(parts["major"], parts["minor"], true);
}

//' @rdname convert
//' @keywords internal manip
// [[Rcpp::export]]
Rcpp::CharacterVector icd9PartsToDecimal(const Rcpp::List parts) {
	return icd9MajMinToCode(parts["major"], parts["minor"], false);
}

// [[Rcpp::export]]
Rcpp::List icd9MajMinToParts(const Rcpp::CharacterVector major,
		const Rcpp::CharacterVector minor) {
	Rcpp::List returned_frame = Rcpp::List::create(Rcpp::_["major"] = major,
			Rcpp::_["minor"] = minor);

	Rcpp::StringVector sample_row = returned_frame(0);
	Rcpp::IntegerVector row_names = seq_along(sample_row);
	returned_frame.attr("row.names") = row_names;
	// doesn't actually need a data frame, although it is barely distinguishable
	// from a list, and not costly to construct in this manner.
	returned_frame.attr("class") = "data.frame";

	return returned_frame;
}

// [[Rcpp::export]]
Rcpp::List icd9MajMinToParts_list(const Rcpp::CharacterVector major,
		const Rcpp::CharacterVector minor) {
	Rcpp::List out = Rcpp::List::create(Rcpp::_["major"] = major,
			Rcpp::_["minor"] = minor);
	return out;
}

//' @rdname convert
//' @keywords internal manip
// [[Rcpp::export]]
Rcpp::List icd9ShortToPartsCpp(const Rcpp::CharacterVector icd9Short, const Rcpp::String minorEmpty) {

	Rcpp::CharacterVector major(icd9Short.size());
	Rcpp::CharacterVector minor(icd9Short.size());

	for (int i = 0; i < icd9Short.size(); ++i) {
		Rcpp::String thisShort = icd9Short[i];
		if (thisShort == NA_STRING) { // .is_na() is private?
			minor[i] = NA_STRING; // I think set_na() might be an alternative.
			continue;
		}

		std::string s(thisShort.get_cstring()); // TODO maybe better to use as?
		s = strimCpp(s); // in place or rewrite?
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
Rcpp::List icd9DecimalToPartsCpp(const Rcpp::CharacterVector icd9Decimal, const Rcpp::String minorEmpty) {
	Rcpp::CharacterVector majors;
	Rcpp::CharacterVector minors;
	int ilen = icd9Decimal.length();

	if (ilen == 0) {
		return Rcpp::List::create(Rcpp::_["major"] =
				Rcpp::CharacterVector::create(), Rcpp::_["minor"] =
						Rcpp::CharacterVector::create());
	}

	for (Rcpp::CharacterVector::const_iterator it = icd9Decimal.begin();
			it != icd9Decimal.end(); ++it) {
		Rcpp::String strna = *it;
		if (strna == NA_STRING || strna == "") {
			majors.push_back(NA_STRING);
			minors.push_back(NA_STRING);
			continue;
		}
		// TODO: Rcpp::Rcpp::String doesn't implement many functions, so using STL. A FAST way
		// would be to use Rcpp::String's function get_cstring, and recode the trim
		// functions to take const char *. This would avoid the type change AND be
		// faster trimming.
		std::string thiscode = Rcpp::as<std::string>(*it);
		thiscode = strimCpp(thiscode); // This updates 'thisccode' by reference, no copy
		std::size_t pos = thiscode.find(".");
		// substring parts
		std::string majorin;
		Rcpp::String minorout;
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
	return Rcpp::List::create(Rcpp::_["major"] = majors, Rcpp::_["minor"] =
			minors);
}

//' @title Convert ICD-9 codes between short and decimal forms
//' @template icd9-short
//' @template icd9-decimal
//' @export
// [[Rcpp::export]]
Rcpp::CharacterVector icd9ShortToDecimal(
		const Rcpp::CharacterVector icd9Short) {
	return icd9PartsToDecimal(icd9ShortToPartsCpp(icd9Short, ""));
}

//' @rdname icd9ShortToDecimal
//' @export
// [[Rcpp::export]]
Rcpp::CharacterVector icd9DecimalToShortOld(
		const Rcpp::CharacterVector icd9Decimal) {
	return icd9PartsToShort(icd9DecimalToPartsCpp(icd9Decimal, ""));
}

// ' //rdname icd9ShortToDecimal
// ' //export
// [[//Rcpp::export]]
//Rcpp::CharacterVector icd9DecimalToShort2(const Rcpp::CharacterVector icd9Decimal) {
//  SEXP Sexpr_icd9Decimal = icd9Decimal.getSexpr();
//  if (TYPEOF(Sexpr_icd9Decimal) != STRSXP) Rcpp::stop("icd9DecimalToShort requires string vector input.");
//  // change in place, or return a copy?
//  dropdot(Sexpr_icd9Decimal);
//  return icd9Decimal;
//}

//' @rdname convert
//' @export
// [[Rcpp::export]]
Rcpp::CharacterVector icd9DecimalToShort(
		const Rcpp::CharacterVector icd9Decimal) {
	Rcpp::CharacterVector out = clone(icd9Decimal); // clone instead of pushing back thousands of times
  size_t ilen = icd9Decimal.length();
	if (ilen == 0)
		return out;
	for (size_t i = 0; i != ilen; ++i) {
		Rcpp::String strna = icd9Decimal[i]; // need to copy here? does it copy?
		if (strna == NA_STRING || strna == "")
			continue;
		// TODO: Rcpp::String doesn't implement many functions, so using STL. A FAST way
		// might be to use Rcpp::String's function get_cstring, and recode the trim
		// functions to take const char *. This would avoid the type change AND be
		// faster trimming.
		const char * thiscode_cstr = strna.get_cstring();
		std::string thiscode(thiscode_cstr);
		thiscode = trimLeftCpp(thiscode);
		// TODO consider rejecting grossly invalid codes as NA:
		std::size_t pos = thiscode.find_first_of(".");
		if (pos != std::string::npos) {
#ifdef ICD9_DEBUG_TRACE
			Rcpp::Rcout << "found .\n";
#endif
			// now we assume that the major is snug against the left side, so we can add zero padding
			thiscode.erase(pos, 1); // remove the decimal point
			// TODO could do fewer tests on the code by doing this last, but most codes are not V or E...
			if (pos > 0 && pos < 4 && !icd9IsASingleVE(thiscode_cstr)) {
#ifdef ICD9_DEBUG_TRACE
				Rcpp::Rcout << "found numeric\n";
#endif
				thiscode.insert(0, 3 - pos, '0');
			} else if (pos == 2 && icd9IsASingleV(thiscode_cstr)) {
#ifdef ICD9_DEBUG_TRACE
				Rcpp::Rcout << "found V\n";
#endif
				thiscode.insert(1, 1, '0');
				out[i] = thiscode;
			} else if ((pos == 2 || pos == 3) && icd9IsASingleE(thiscode_cstr)) {
#ifdef ICD9_DEBUG_TRACE
				Rcpp::Rcout << "found E\n";
#endif
				thiscode.insert(1, 4 - pos, '0');
			}
			// otherwise leave the code alone
			out[i] = thiscode;

		} else {
			out[i] = Rcpp::String(icd9AddLeadingZeroesMajorSingleStd(thiscode));
		}
	}
	return out;
}

// ' //rdname icd9ShortToDecimal
// ' //export
// [[//Rcpp::export]]
//int icd9DecimalToShortUpdate(const SEXP icd9Decimal) {
//  if (TYPEOF(Sexpr_icd9Decimal) != STRSXP) Rcpp::stop("icd9DecimalToShort requires string vector input.");
// change in place, or return a copy?
//  dropdot(Sexpr_icd9Decimal);
//return 0;
//}


//' @title Get major (three-digit) part of ICD-9 codes
//' @description This is reasonably fast, but calculates all the minors, then throws away the result.
//' @template icd9-any
//' @template isShort
//' @keywords internal manip
//' @export
// [[Rcpp::export]]
Rcpp::CharacterVector icd9GetMajor(const Rcpp::CharacterVector icd9, const bool isShort) {
	if (isShort) {
		// TODO: casting (or just compiler/syntax checker hinting?) SEXP may be costly.
		SEXP majors = icd9ShortToPartsCpp(icd9, "")[0]; // actually wants to be an Rcpp::List
		return Rcpp::as<Rcpp::CharacterVector>(majors);
	}
	SEXP majors = icd9DecimalToPartsCpp(icd9, "")[0];
	return Rcpp::as<Rcpp::CharacterVector>(majors);
}

//' @rdname icd9GetMajor
//' @keywords internal manip
// [[Rcpp::export]]
Rcpp::CharacterVector icd9GetMajorShort(const Rcpp::CharacterVector icd9Short) {
	// compiles with or without the (SEXP) cast, but Eclipse can't resolve without the SEXP
	return Rcpp::as<Rcpp::CharacterVector>((SEXP)icd9ShortToPartsCpp(icd9Short, "")[0]);
}
