#ifndef RANGES_H_
#define RANGES_H_

#include "icd_types.h"
#include "range-const.h"

CV icd9ExpandMinor(const std::string& mnr,
                   bool isE = false);
CV icd9ChildrenShortUndefined(const CV& icd9Short);
CV icd9ChildrenShortDefined(const CV& icd9Short,
                            const VecStr& icd9cmReal);
CV icd9ChildrenShort(const CV& icd9Short,
                     const VecStr& icd9cmReal,
                     bool onlyReal = true);
CV icd9ChildrenShortUnorderedUndefined(const CV& icd9Short);
CV icd9ChildrenShortUnorderedDefined(const CV& icd9Short,
                                     const VecStr& icd9cmReal);
CV icd9ChildrenShortUnordered(const CV& icd9Short,
                              const VecStr& icd9cmReal,
                              bool onlyReal = true);
CV icd9ChildrenDecimal(const CV& icd9Decimal,
                       const VecStr& icd9cmReal,
                       bool onlyReal = true);
CV icd9ChildrenDecimalUnordered(const CV& icd9Decimal,
                                const VecStr& icd9cmReal,
                                bool onlyReal);
CV icd9Children(const CV& icd9,
                bool isShort,
                const VecStr& icd9cmReal,
                bool onlyReal = true);
#endif /* RANGES_H_ */
