#ifndef APPENDMINOR_H_
#define APPENDMINOR_H_

#include <Rcpp.h>
typedef Rcpp::CharacterVector CV;
CV icd9MajMinToCode(const CV& mjr, const CV& mnr, const bool short_code);
CV icd9MajMinToShort(const CV& mjr, const CV& mnr);
CV icd9MajMinToDecimal(const CV& mjr, const CV& mnr);

#endif /* APPENDMINOR_H_ */
