#ifndef RANGES_H_
#define RANGES_H_

#include "icd_types.h"
#include "range-const.h"

CV icd9ChildrenShort(CV icd9Short,
                     const VecStr& icd9cmReal,
                     bool onlyReal = true);
CV icd9ChildrenShortUnordered(CV icd9Short,
                              const VecStr& icd9cmReal,
                              bool onlyReal = true);
CV icd9ChildrenDecimalCpp(CV icd9Decimal,
                          const VecStr& icd9cmReal,
                          bool onlyReal = true);
CV icd9ExpandMinor(const Str& mnr,
                   bool isE = false);

#endif /* RANGES_H_ */
