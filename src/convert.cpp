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
CharacterVector icd9MajMinToCodeShim(const CharacterVector mjr,
		const CharacterVector mnr, bool isShort) {
#ifdef ICD9_DEBUG_TRACE
	Rcout << "icd9MajMinToCode\n";
	if (mjr.size() != mnr.size())
	Rcpp::stop("aborting because mjr and mnr lengths differ");
#endif
	CharacterVector out; // wish I could reserve space for this
	CharacterVector::const_iterator j = mjr.begin();
	CharacterVector::const_iterator n = mnr.begin();

	for (; j != mjr.end() && n != mnr.end(); ++j, ++n) {
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
		if (!isShort && mnrelem != "") {
			smj.append(".");
		}
		smj.append(mnrelem);
		out.push_back(smj);

	}

	return wrap(out);
}

// [[Rcpp::export]]
CharacterVector icd9MajMinToShortShim(const CharacterVector mjr,
		const CharacterVector mnr) {
#ifdef ICD9_DEBUG_TRACE
	Rcout << "icd9MajMinToShort: mjr.size() = " << mjr.size()
	<< " and mnr.size() = " << mnr.size() << "\n";
#endif
	if ((mjr.size() != 1 && mjr.size() != mnr.size())
			|| (mjr.size() == 1 && mnr.size() == 0)) {
		Rcpp::stop("icd9MajMinToShort, length of mjrs and mnrs must be equal.");
	}
	if (mjr.size() == 1) {
#ifdef ICD9_DEBUG_TRACE
		Rcout << "icd9MajMinToShort: mjr.size() = 1\n";
#endif
		CharacterVector newmjr(mnr.size(), mjr[0]);
		return icd9MajMinToCodeShim(newmjr, mnr, true);
	}
	return icd9MajMinToCodeShim(mjr, mnr, true);
}

// [[Rcpp::export]]
CharacterVector icd9MajMinToDecimalShim(const CharacterVector mjr,
		const CharacterVector mnr) {
	return icd9MajMinToCodeShim(mjr, mnr, false);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9PartsToShort(const List parts) {
	return icd9MajMinToCodeShim(parts["major"], parts["minor"], true);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9PartsToDecimal(const List parts) {
	return icd9MajMinToCodeShim(parts["major"], parts["minor"], false);
}

// [[Rcpp::export]]
List icd9MajMinToPartsShim(const CharacterVector mjr,
		const CharacterVector mnr) {
	List returned_frame = List::create(_["major"] = mjr, _["minor"] = mnr);

	// TODO: can do this with lists, ?no need for a data frame
	StringVector sample_row = returned_frame(0);
	IntegerVector row_names = seq_along(sample_row);
	returned_frame.attr("row.names") = row_names;
	returned_frame.attr("class") = "data.frame";

	return returned_frame;
}

// [[Rcpp::export]]
List icd9MajMinToParts_listShim(const CharacterVector mjr,
		const CharacterVector mnr) {
	List out = List::create(_["major"] = mjr, _["minor"] = mnr);
	return out;
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
List icd9ShortToParts(const CharacterVector icd9Short, const String minorEmpty =
		"") {

	CharacterVector mjr(icd9Short.size());
	CharacterVector mnr(icd9Short.size());

	for (int i = 0; i < icd9Short.size(); ++i) {
		if (CharacterVector::is_na(icd9Short[i])) {
			mnr[i] = NA_STRING;
			continue;
		}

		std::string s = as<std::string>(icd9Short[i]);
		s = strimCpp(s); // do i need to convert?

		if (!icd9IsASingleE(s.c_str())) { // not an E code
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
				mnr[i] = s.substr(3, s.size() - 3);
				continue;
			default:
				mjr[i] = NA_STRING;
				mnr[i] = NA_STRING;
				continue;
			}
		} else { // E code

			switch (s.size()) {
			case 2:
			case 3:
			case 4:
				mjr[i] = s.substr(0, s.size());
				mnr[i] = minorEmpty;
				break;
			case 5:
				mjr[i] = s.substr(0, 4);
				mnr[i] = s.substr(4, 1);
				break;
			default:
				mjr[i] = NA_STRING;
				mnr[i] = NA_STRING;
				continue;
			}
		} // E code
		  //mjr[i] = icd9AddLeadingZeroesmjrSingle(mjr[i]); // or loop through them all again...
	} // for

	return icd9MajMinToPartsShim(icd9AddLeadingZeroesMajorShim(mjr), mnr);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
List icd9DecimalToParts(const CharacterVector icd9Decimal,
		const String minorEmpty = "") {
	CharacterVector mjrs;
	CharacterVector mnrs;
	int ilen = icd9Decimal.length();

	if (ilen == 0) {
		return List::create(_["major"] = CharacterVector::create(), _["minor"] =
				CharacterVector::create());
	}

	for (CharacterVector::const_iterator it = icd9Decimal.begin();
			it != icd9Decimal.end(); ++it) {
		String strna = *it;
		if (strna == NA_STRING || strna == "") {
			mjrs.push_back(NA_STRING);
			mnrs.push_back(NA_STRING);
			continue;
		}
		std::string thiscode = as<std::string>(*it); // Rcpp::String doesn't implement many functions.
		thiscode = strimCpp(thiscode); // TODO: update in place.
		std::size_t pos = thiscode.find(".");
		// substring parts
		std::string mjrin;
		String mnrout;
		if (pos != std::string::npos) {
			mjrin = thiscode.substr(0, pos);
			mnrout = thiscode.substr(pos + 1);
		} else {
			mjrin = thiscode;
			mnrout = minorEmpty;
		}
		mjrs.push_back(icd9AddLeadingZeroesMajorSingleShim(mjrin));
		mnrs.push_back(mnrout);
	}
	return List::create(_["major"] = mjrs, _["minor"] = mnrs);
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9ShortToDecimal(const CharacterVector icd9Short) {
	return icd9PartsToDecimal(icd9ShortToParts(icd9Short));
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9DecimalToShort(const CharacterVector icd9Decimal) {
	return icd9PartsToShort(icd9DecimalToParts(icd9Decimal));
}

//' @rdname convert
//' @export
// [[Rcpp::export]]
CharacterVector icd9GetMajor(const CharacterVector icd9, const bool isShort) {
	if (isShort) {
		return as<CharacterVector>(icd9ShortToParts(icd9)[0]);
	}
	return as<CharacterVector>(icd9DecimalToParts(icd9)[0]);
}
