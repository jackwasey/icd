#ifndef APPENDMINOR_H_
#define APPENDMINOR_H_

#include "icd_types.h"
CV icd9MajMinToCode(const CV& mjr, const CV& mnr, const bool isShort);
CV icd9MajMinToShort(const CV& mjr, const CV& mnr);
CV icd9MajMinToDecimal(const CV mjr, const CV mnr);
void icd9AppendMinors(VecStr& m, const VecStr& mnr, bool isShort);

inline void icd9AppendMinorsShort(std::vector<std::string>& m,
                                  const std::vector<std::string>& mnr) {
  for (std::vector<std::string>::size_type j = 0; j != m.size(); ++j) { m[j].append(mnr[j]); }
}

inline void icd9AppendMinorsShort(std::vector<std::string>& m,
                                  const std::vector<std::string>& mnr,
                                  bool reserve) {
  if (reserve) m.reserve(5);
  for (std::vector<std::string>::size_type j = 0; j != m.size(); ++j) { m[j].append(mnr[j]); }
}

#endif /* APPENDMINOR_H_ */
