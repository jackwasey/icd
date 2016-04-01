#!/bin/bash

Rscript -e 'options(warn = 1); devtools::load_all(); for (i in c("I70", "I71", "I731", "I738", "I739", "I771", "I790", "I792", "K551", "K558", "K559", "Z958", "Z959")) {message(i); devnull <- icd_children.icd10cm(i) }'
Rscript -e 'devtools::load_all(); icd_children.icd10cm(c("I792", "K551"))'
Rscript -e 'devtools::load_all(); icd_children.icd10cm(c("I790", "I792"))'

