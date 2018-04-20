#!/bin/bash

reg="src/registration.c"
if [ ! -f "$reg" ]; then
  if [ ! -f "$HOME/$reg" ]; then
    echo "Cannot locate the target src/registration.c file"
    exit 1
  fi
fi

Rscript -e 'tools::package_native_routine_registration_skeleton(".", "src/registration.c", character_only = FALSE)'
# try to drop unwanted registrations, but this led to other problems, as Rcpp::compileAttributes still generated code which needed registration
# sed -i '' '/_alt_/d' "$reg"
sed -i '' '/\"fun/d' "$reg"

