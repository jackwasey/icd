#ifndef VALGRIND_ICD_H_
#define VALGRIND_ICD_H_

#include "icd_types.h"
#include "local.h"
#include <string>                       // for string
#include <utility>                      // for pair
#include <vector>                       // for vector

int valgrindCallgrindStart(bool zerostats);
int valgrindCallgrindStop();

#endif /* ICD_VALGRIND_ICD_H_ */
