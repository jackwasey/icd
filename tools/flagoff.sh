#!/bin/sh

# turn off a flag in local.h

flag="${1:-ICD_DEBUG}"

ICD_HOME=${ICD_HOME:-"$HOME/rprojects/icd"}

# flip on ICD_DEBUG
sed -i "s@ *#define ${flag}\$@// #define ${flag}@" "$ICD_HOME/src/local.h"

