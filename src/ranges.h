#ifndef RANGES_H_
#define RANGES_H_

#include "icd_types.h"
#include "range-const.h"

CV icd9ExpandMinor(const std::string& mnr,
                   bool isE = false);
CV icd9ChildrenShortUndefined(const CV& x);
CV icd9ChildrenShortDefined(const CV& x,
                            const VecStr& is_defined);
CV icd9ChildrenShort(const CV& x,
                     const VecStr& is_defined,
                     bool leaf = true);
CV icd9ChildrenShortUnorderedUndefined(const CV& x);
CV icd9ChildrenShortUnorderedDefined(const CV& x,
                                     const VecStr& is_defined);
CV icd9ChildrenShortUnordered(const CV& x,
                              const VecStr& is_defined,
                              bool leaf = true);
CV icd9ChildrenDecimal(const CV& x,
                       const VecStr& is_defined,
                       bool leaf = true);
CV icd9ChildrenDecimalUnordered(const CV& x,
                                const VecStr& is_defined,
                                bool leaf);
CV icd9Children(const CV& icd9,
                bool isShort,
                const VecStr& is_defined,
                bool leaf = true);
#endif /* RANGES_H_ */
