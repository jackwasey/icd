#!/bin/bash
Rscript -e 'tools::package_native_routine_registration_skeleton(".", "src/registration.c", character_only = FALSE)'
#sed -i '' '/_alt_/d' src/registration.c

