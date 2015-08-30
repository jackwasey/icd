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

#ifndef CONVERT_H_
#define CONVERT_H_

#ifndef Rcpp_hpp
#include <Rcpp.h>
#endif
Rcpp::List icd9ShortToParts(const Rcpp::CharacterVector icd9Short,
		const Rcpp::String minorEmpty = "");
Rcpp::List icd9DecimalToParts(const Rcpp::CharacterVector icd9Decimal,
		const Rcpp::String minorEmpty = "");
Rcpp::CharacterVector icd9PartsToShort(const Rcpp::List parts);
Rcpp::CharacterVector icd9PartsToDecimal(const Rcpp::List parts);
Rcpp::CharacterVector icd9MajMinToShort(const Rcpp::CharacterVector major,
		const Rcpp::CharacterVector mnr);
Rcpp::CharacterVector icd9MajMinToDecimal(const Rcpp::CharacterVector major,
		const Rcpp::CharacterVector mnr);
Rcpp::CharacterVector icd9DecimalToShort(
		const Rcpp::CharacterVector icd9Decimal);
Rcpp::CharacterVector icd9ShortToDecimal(const Rcpp::CharacterVector icd9Short);

#endif /* CONVERT_H_ */
